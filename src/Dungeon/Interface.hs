{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Dungeon.Interface (
    Turn(..),
    Direction(..),
    UIKey(..),
    UIInput(..),
    UIMode(..),
    Config(..),
    HasVty(..),
    parseInput,
    uiStep,
    uiState,
    choicePageSize,
    pageChoices,
    screenLines,
    inputVty,
    outputVty,
    viewport,
    playerStatsLines,
    messageLogLines,
    gameViewCell
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.MSF.Except (performOnFirstSample)

import Data.Array
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.MonadicStreamFunction
import Graphics.Vty

import Dungeon.GameData
    ( attackConfirmationMessage
    , containerDescription
    , itemDescription
    , itemGlyph
    , nothingToPickupMessage
    , nothingToLootMessage
    , npcGlyph
    , npcName
    )
import Dungeon.Item
    ( Container(..)
    , Item(..)
    , ItemId
    , containersIn
    , itemStackToList
    , itemStackItems
    , itemStackSize
    , itemsAt
    , lookupItem
    )
import Dungeon.Npc (Npc(..), npcAt)
import Dungeon.Map (movePosition)
import Dungeon.Logic
    ( FightMode(..)
    , GameView(..)
    , Player(..)
    , Position
    , WetStatus(..)
    )
import Dungeon.Types
    ( CharAttributes(..)
    , Direction(..)
    , Stats(..)
    , Turn(..)
    , Vitals(..)
    )


-- | Configuration for the application
data Config = Config
    { cfgVty :: Vty
    , cfgViewportDims :: (Int, Int)
    , cfgPadding :: (Int, Int)
    }

-- | A command received from the terminal frontend.
data UIKey
    = CharKey Char
    | EscapeKey
    | OtherKey
    deriving (Show, Eq)

data UIInput
    = PlayTurn Turn
    | LogMessage String
    | Quit
    deriving (Show, Eq)

data UIMode
    = DungeonScreen
    | InventoryScreen Int
    | ItemScreen ItemId
    | DropScreen Int
    | PickupScreen Int
    | LootContainersScreen Int
    | LootItemsScreen ItemId Int
    | AttackConfirmation Direction
    deriving (Show, Eq)

-- | Type class for accessing Vty and other common configuration data
class HasVty r where
    getVty :: r -> Vty
    getViewportDims :: r -> (Int, Int)
    getPadding :: r -> (Int, Int)

instance HasVty Config where
    getVty = cfgVty
    getViewportDims = cfgViewportDims
    getPadding = cfgPadding


-- | An MSF reading keys frm the terminal.
inputVty :: (MonadReader r m, HasVty r, MonadIO m)
    => MSF m () (Maybe UIKey)
inputVty = arrM $ \_ -> do
    vty <- asks getVty
    evt <- liftIO $ nextEvent vty
    case evt of
        EvKey KEsc [] -> pure (Just EscapeKey)
        EvKey KEnter [] -> pure (Just (CharKey '\n'))
        EvKey (KChar c) [] -> pure (Just (CharKey c))
        EvKey _ _ -> pure (Just OtherKey)
        _ -> pure Nothing

-- | Translate an unmodified character key into a frontend command.
parseInput :: Char -> Maybe UIInput
parseInput 'h' = Just (PlayTurn (Move West))
parseInput 'j' = Just (PlayTurn (Move South))
parseInput 'k' = Just (PlayTurn (Move North))
parseInput 'l' = Just (PlayTurn (Move East))
parseInput '.' = Just (PlayTurn Wait)
parseInput '\n' = Just (PlayTurn Inspect)
parseInput 'q' = Just Quit
parseInput _ = Nothing

choicePageSize :: Int
choicePageSize = 15

pageChoices :: Int -> [a] -> [(Char, a)]
pageChoices page values =
    zip ['a' ..]
        $ take choicePageSize
        $ drop (normalizePage page values * choicePageSize) values

pageCount :: [a] -> Int
pageCount values = max 1
    $ (length values + choicePageSize - 1) `div` choicePageSize

normalizePage :: Int -> [a] -> Int
normalizePage page values = max 0 page `mod` pageCount values

nextPage :: Int -> [a] -> Int
nextPage page values =
    (normalizePage page values + 1) `mod` pageCount values

uiState
    :: Monad m
    => MSF
         m
         (Maybe UIKey, GameView)
         ([UIMode], Maybe UIInput)
uiState = mealy step [DungeonScreen]
    where
        step (key, view) screens =
            let (screens', command) =
                    maybe (screens, Nothing)
                        (\input -> uiStep input view screens)
                        key
            in ((screens', command), screens')

uiStep
    :: UIKey
    -> GameView
    -> [UIMode]
    -> ([UIMode], Maybe UIInput)
uiStep key view screens =
    case key of
        EscapeKey -> (back screens, Nothing)
        OtherKey -> cancelConfirmation
        CharKey c -> stepScreen c
    where
        current = case screens of
            mode : _ -> mode
            [] -> DungeonScreen
        inventory = plInventory (vPlayer view)
        playerPos = plPos (vPlayer view)
        floorStack = itemsAt playerPos (vFloorItems view)

        stepScreen c =
            case current of
                DungeonScreen ->
                    case c of
                        'i' -> open (InventoryScreen 0)
                        'd' -> open (DropScreen 0)
                        'p' -> pickup
                        'L' -> loot
                        _ -> dungeonCommand (parseInput c)
                InventoryScreen page ->
                    choose c page (itemStackToList inventory)
                        (open . ItemScreen . fst)
                        InventoryScreen
                ItemScreen ident ->
                    case c of
                        'd'
                            | Just _ <- lookupItem ident inventory ->
                                finish (Drop ident)
                        'w'
                            | Just (WeaponItem _) <- lookupItem ident inventory ->
                                finish (Wield ident)
                        _ -> (screens, Nothing)
                DropScreen page ->
                    choose c page (itemStackToList inventory)
                        (finish . Drop . fst)
                        DropScreen
                PickupScreen page ->
                    choose c page (itemStackToList floorStack)
                        (finish . Pick . fst)
                        PickupScreen
                LootContainersScreen page ->
                    choose c page (containersIn floorStack)
                        (\(ident, _) -> open (LootItemsScreen ident 0))
                        LootContainersScreen
                LootItemsScreen containerId page ->
                    choose c page (containerContents containerId floorStack)
                        (finish . Loot containerId . fst)
                        (LootItemsScreen containerId)
                AttackConfirmation dir
                    | c == 'y' || c == '\n' -> finish (Move dir)
                    | otherwise -> (back screens, Nothing)

        open mode = (mode : nonEmpty screens, Nothing)
        finish turn = ([DungeonScreen], Just (PlayTurn turn))
        dungeonCommand command@(Just (PlayTurn (Move dir))) =
            case npcAt (movePosition dir playerPos) (vNpcs view) of
                Just (_, npc) ->
                    ( AttackConfirmation dir : nonEmpty screens
                    , Just
                        (LogMessage
                            (attackConfirmationMessage (npcKind npc)))
                    )
                Nothing -> (screens, command)
        dungeonCommand command = (screens, command)
        pickup =
            case itemStackToList floorStack of
                [] ->
                    (nonEmpty screens, Just (LogMessage nothingToPickupMessage))
                [(ident, _)] -> finish (Pick ident)
                _ -> open (PickupScreen 0)
        loot =
            case containersIn floorStack of
                [] ->
                    (nonEmpty screens, Just (LogMessage nothingToLootMessage))
                [(ident, _)] -> open (LootItemsScreen ident 0)
                _ -> open (LootContainersScreen 0)
        pageTo c constructor page values
            | c == ' ' =
                (constructor (nextPage page values) : parent screens, Nothing)
            | otherwise = (screens, Nothing)

        choose c page values selected constructor =
            maybe
                (pageTo c constructor page values)
                selected
                (lookup c $ pageChoices page values)

        containerContents ident stack =
            case lookupItem ident stack of
                Just (ContainerItem container) ->
                    itemStackToList (containerItems container)
                _ -> []

        back (_ : rest@(_ : _)) = rest
        back _ = [DungeonScreen]
        cancelConfirmation =
            case current of
                AttackConfirmation _ -> (back screens, Nothing)
                _ -> (screens, Nothing)
        parent (_ : rest) = nonEmpty rest
        parent [] = [DungeonScreen]
        nonEmpty [] = [DungeonScreen]
        nonEmpty xs = xs

screenLines :: GameView -> UIMode -> [String]
screenLines view mode =
    case mode of
        DungeonScreen -> []
        InventoryScreen page -> itemChoices "Inventory" page inventory
        ItemScreen ident -> itemDetails ident
        DropScreen page -> itemChoices "Drop which item?" page inventory
        PickupScreen page ->
            itemChoices "Pick up which item?" page floorStack
        LootContainersScreen page ->
            choiceLines "Loot which container?" page
                (containerDescription . snd)
                (containersIn floorStack)
        LootItemsScreen containerId page ->
            case lookupItem containerId floorStack of
                Just (ContainerItem container) ->
                    itemChoices
                        ("Loot from the " ++ containerDescription container)
                        page
                        (containerItems container)
                _ -> ["Container unavailable", "Esc: back"]
        AttackConfirmation _ -> []
    where
        player = vPlayer view
        inventory = plInventory player
        floorStack = itemsAt (plPos player) (vFloorItems view)

        itemChoices title page =
            choiceLines title page (itemDescription . snd) . itemStackToList

        itemDetails ident =
            case lookupItem ident inventory of
                Nothing -> ["Item unavailable", "Esc: back"]
                Just item ->
                    [itemDescription item]
                    ++ ["(wielded)" | plWielded player == Just ident]
                    ++ ["d: drop"]
                    ++ ["w: wield" | WeaponItem _ <- [item]]
                    ++ ["Esc: back"]

        choiceLines title page describe values =
            [ title
                ++ " (page "
                ++ show (normalizePage page values + 1)
                ++ "/"
                ++ show (pageCount values)
                ++ ")"
            ]
            ++ case pageChoices page values of
                [] -> ["(empty)", "Esc: back"]
                choices ->
                    [ label : ") " ++ describe value
                    | (label, value) <- choices
                    ]
                    ++ ["Space: next page  Esc: back"]

-- | An MSF that renders the game state to the terminal.
outputVty :: (MonadReader r m, HasVty r, MonadIO m)
    => MSF m (GameView, UIMode) ()
outputVty = proc (gv, mode) -> do
    let pos = plPos (vPlayer gv)
    let dungeonDims = snd $ bounds $ vDungeon gv
    vp <- viewport -< (pos, dungeonDims)
    render -< ((gv, mode), vp)

-- | An MSF that calculates the viewport based on the player's position.
viewport :: (MonadReader r m, HasVty r)
    => MSF
         m
         ((Int, Int), (Int, Int))
         (Int, Int, Int, Int)
viewport =
    performOnFirstSample $ do
        (vw, vh) <- asks getViewportDims
        padding <- asks getPadding
        pure
            $ accumulateWith
                (updateViewport padding)
                (1, 1, vw, vh)
    where
        updateViewport
            :: (Int, Int)
            -> ((Int, Int), (Int, Int))
            -> (Int, Int, Int, Int)
            -> (Int, Int, Int, Int)
        updateViewport
            (padX, padY)
            ((px, py), (lw, lh))
            (x1, y1, x2, y2) =
            let vpW = x2 - x1 + 1
                vpH = y2 - y1 + 1
                updateAxis pos' start end mapSize vpSize padding
                    | mapSize <= vpSize = 1
                    | pos' < start + padding =
                        max 1 (pos' - padding)
                    | pos' > end - padding =
                        min
                            (mapSize - vpSize + 1)
                            (pos' + padding - vpSize + 1)
                    | otherwise = start
                x1' = updateAxis px x1 x2 lw vpW padX
                y1' = updateAxis py y1 y2 lh vpH padY
            in (x1', y1', x1' + vpW - 1, y1' + vpH - 1)

dungeonX, dungeonY, statsGap, messageGap, messageLogHeight :: Int
dungeonX = 2
dungeonY = 1
statsGap = 3
messageGap = 1
messageLogHeight = 6

-- | Renders the game state to the Vty terminal.
render :: (MonadReader r m, HasVty r, MonadIO m)
    => MSF m ((GameView, UIMode), (Int, Int, Int, Int)) ()
render = arrM $ \((gv, mode), (x1, y1, x2, y2)) -> do
    vty <- asks getVty
    let player = vPlayer gv
    let (px, py) = plPos player
    let drawRect =
            vertCat
                [ string defAttr [ gameViewCell gv (x, y) | x <- [x1 .. x2] ]
                | y <- [y1 .. y2]
                ]
    let vpW = x2 - x1 + 1
    let vpH = y2 - y1 + 1
    let textScreen =
            vertCat
                [ string defAttr (padRight vpW line)
                | line <- padRows vpH (screenLines gv mode)
                ]
    let dungeonLayer =
            if showsDungeon mode then drawRect else textScreen
    let logRows =
            vertCat (map (string defAttr) (messageLogLines (vMessages gv)))
    let statsPanel =
            vertCat (map (string defAttr) (playerStatsLines gv))
    let statsX = dungeonX + vpW + statsGap
    let logY = dungeonY + vpH + messageGap

    liftIO $ do
        update vty (picForLayers
            [ translate dungeonX dungeonY dungeonLayer
            , translate statsX dungeonY statsPanel
            , translate dungeonX logY logRows
            ])
        if showsDungeon mode
            then do
                setCursorPos
                    (outputIface vty)
                    (px - x1 + dungeonX)
                    (py - y1 + dungeonY)
                showCursor (outputIface vty)
            else hideCursor (outputIface vty)
    where
        padRight width line =
            take width line ++ replicate (max 0 (width - length line)) ' '
        padRows height rows =
            take height (rows ++ repeat "")
        showsDungeon DungeonScreen = True
        showsDungeon (AttackConfirmation _) = True
        showsDungeon _ = False

-- | Lines shown in the player panel to the right of the dungeon.
playerStatsLines :: GameView -> [String]
playerStatsLines gv =
    [ "Player"
    , "Position: " ++ show (plPos player)
    , "HP: " ++ show (vitalHealth vitals)
    , "MP: " ++ show (vitalMana vitals)
    , "Hunger: " ++ show (vitalHunger vitals)
    , "Strength: " ++ show (statStrength stats)
    , "Intelligence: " ++ show (statIntelligence stats)
    , "Dexterity: " ++ show (statDexterity stats)
    , "Constitution: " ++ show (statConstitution stats)
    , "Items: " ++ show (itemStackSize inventory)
    ]
    ++ wieldedLines
    ++ wetLines
    ++ fightLines
    where
        player = vPlayer gv
        attributes = plAttributes player
        vitals = charVitals attributes
        stats = charStats attributes
        inventory = plInventory player
        wieldedLines =
            case plWielded player >>= (`lookupItem` inventory) of
                Just item -> ["Wielded: " ++ itemDescription item]
                Nothing -> []
        wetLines =
            case plWetStatus player of
                Dry -> []
                Wet -> ["Wet: " ++ show (plWetCountdown player)]
        fightLines =
            case plFightMode player of
                Exploring -> []
                Fighting participants ->
                    [ "Fight: "
                    ++ intercalate ", "
                        (map (npcName . npcKind)
                            $ Map.elems
                            $ Map.restrictKeys (vNpcs gv) participants)
                    ]

-- | The newest six messages, top-padded to keep the log area a fixed height.
messageLogLines :: [String] -> [String]
messageLogLines msgs =
    replicate (messageLogHeight - length recent) "" ++ recent
    where
        recent = drop (length msgs - messageLogHeight) msgs

-- | Character shown for one dungeon position in the current game view.
-- Items and containers overlay terrain without changing the dungeon map.
gameViewCell :: GameView -> Position -> Char
gameViewCell gv pos
    | Just (_, npc) <- npcAt pos (vNpcs gv) =
        npcGlyph (npcKind npc)
    | Just item <- firstItem pos (vFloorItems gv) =
        itemGlyph item
    | inRange (bounds dungeon) pos = dungeon ! pos
    | otherwise = ' '
    where
        dungeon = vDungeon gv
        firstItem itemPos =
            listToMaybe
            . itemStackItems
            . itemsAt itemPos
