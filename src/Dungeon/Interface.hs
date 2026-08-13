{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Dungeon.Interface (
    Turn(..),
    Direction(..),
    UIInput(..),
    GameScreen(..),
    Config(..),
    HasVty(..),
    parseInput,
    inputVty,
    outputVty,
    viewport,
    inventoryLines,
    messageLogLines,
    observe,
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
    ( gemColorName
    , indefiniteItemDescription
    , itemDescription
    , itemGlyph
    , npcGlyph
    , npcName
    )
import Dungeon.Item
    ( Container(..)
    , ContainerKind(..)
    , FloorItems
    , Inventory
    , Item(..)
    , itemStackItems
    , itemStackSize
    , itemsAt
    )
import Dungeon.Npc (Npc(..), npcAt)
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
    , Gem(..)
    , Turn(..)
    , Vitals(..)
    )


-- | Configuration for the application
data Config = Config
    { cfgVty :: Vty
    , cfgScreenDims :: (Int, Int)
    , cfgPadding :: (Int, Int)
    }

-- | A command received from the terminal frontend.
data UIInput
    = PlayTurn Turn
    | ShowInventory
    | Redraw
    | Quit
    deriving (Show, Eq)

-- | The current game view with a frontend-selected presentation.
data GameScreen
    = DungeonScreen GameView
    | InventoryScreen GameView
    deriving (Show, Eq)

screenView :: GameScreen -> GameView
screenView (DungeonScreen gv) = gv
screenView (InventoryScreen gv) = gv

-- | Type class for accessing Vty and other common configuration data
class HasVty r where
    getVty :: r -> Vty
    getScreenDims :: r -> (Int, Int)
    getPadding :: r -> (Int, Int)

instance HasVty Config where
    getVty = cfgVty
    getScreenDims = cfgScreenDims
    getPadding = cfgPadding


-- | An MSF reading keys frm the terminal.
inputVty :: (MonadReader r m, HasVty r, MonadIO m)
    => MSF m () (Maybe UIInput)
inputVty = arrM $ \_ -> do
    vty <- asks getVty
    evt <- liftIO $ nextEvent vty
    case evt of
        EvKey KEnter [] -> return $ parseInput '\n'
        EvKey (KChar c) [] -> return $ parseInput c
        _ -> return Nothing

-- | Translate an unmodified character key into a frontend command.
parseInput :: Char -> Maybe UIInput
parseInput 'h' = Just (PlayTurn (Move West))
parseInput 'j' = Just (PlayTurn (Move South))
parseInput 'k' = Just (PlayTurn (Move North))
parseInput 'l' = Just (PlayTurn (Move East))
parseInput 'p' = Just (PlayTurn Pick)
parseInput 'd' = Just (PlayTurn Drop)
parseInput '.' = Just (PlayTurn Wait)
parseInput ' ' = Just Redraw
parseInput 'i' = Just ShowInventory
parseInput '\n' = Just (PlayTurn Inspect)
parseInput 'q' = Just Quit
parseInput _ = Nothing

-- | An MSF that renders the game state to the terminal.
outputVty :: (MonadReader r m, HasVty r, MonadIO m)
    => MSF m GameScreen ()
outputVty = proc screen -> do
    let gv = screenView screen
    let pos = plPos (vPlayer gv)
    let dungeonDims = snd $ bounds $ vDungeon gv
    vp <- viewport -< (pos, dungeonDims)
    render -< (screen, vp)

-- | An MSF that calculates the viewport based on the player's position.
viewport :: (MonadReader r m, HasVty r)
    => MSF
         m
         ((Int, Int), (Int, Int))
         (Int, Int, Int, Int)
viewport =
    performOnFirstSample $ do
        (sw, sh) <- asks getScreenDims
        padding <- asks getPadding
        pure
            $ accumulateWith
                (updateViewport padding)
                (1, 1, sw, sh)
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

dungeonX, dungeonY, messageLogHeight :: Int
dungeonX = 10
dungeonY = 3
messageLogHeight = 6

-- | Renders the game state to the Vty terminal.
render :: (MonadReader r m, HasVty r, MonadIO m)
    => MSF m (GameScreen, (Int, Int, Int, Int)) ()
render = arrM $ \(screen, (x1, y1, x2, y2)) -> do
    vty <- asks getVty
    let gv = screenView screen
    let player = vPlayer gv
    let attrs = plAttributes player
    let vitals = charVitals attrs
    let inv = plInventory player
    let (px, py) = plPos player
    let drawRect =
            vertCat
                [ string defAttr [ gameViewCell gv (x, y) | x <- [x1 .. x2] ]
                | y <- [y1 .. y2]
                ]
    let vpW = x2 - x1 + 1
    let vpH = y2 - y1 + 1
    let inventory =
            vertCat
                [ string defAttr (padRight vpW line)
                | line <- padRows vpH (inventoryLines vpH inv)
                ]
    let dungeonLayer =
            case screen of
                DungeonScreen _ -> drawRect
                InventoryScreen _ -> inventory
    let logRows =
            vertCat (map (string defAttr) (messageLogLines (vMessages gv)))
    let logY = dungeonY + y2 - y1 + 1
    let status =
            "Player pos: " ++ show (px, py)
            ++ "  HP: " ++ show (vitalHealth vitals)
            ++ "  MP: " ++ show (vitalMana vitals)
            ++ "  Hunger: " ++ show (vitalHunger vitals)
            ++ "  Items: " ++ show (itemStackSize inv)
            ++ wetSummary player
            ++ fightSummary gv

    liftIO $ do
        update vty (picForLayers
            [ translate dungeonX dungeonY dungeonLayer
            , translate dungeonX logY logRows
            , string defAttr status
            ])
        case screen of
            DungeonScreen _ -> do
                setCursorPos
                    (outputIface vty)
                    (px - x1 + dungeonX)
                    (py - y1 + dungeonY)
                showCursor (outputIface vty)
            InventoryScreen _ ->
                hideCursor (outputIface vty)
    where
        padRight width line =
            take width line ++ replicate (max 0 (width - length line)) ' '
        padRows height rows =
            take height (rows ++ repeat "")
        wetSummary player =
            case plWetStatus player of
                Dry -> ""
                Wet -> "  Wet: " ++ show (plWetCountdown player)
        fightSummary gv =
            case fightNames gv of
                [] -> ""
                names -> "  Fight: " ++ intercalate ", " names
        fightNames gv =
            case plFightMode (vPlayer gv) of
                Exploring -> []
                Fighting participants ->
                    map (npcName . npcKind)
                        $ Map.elems
                        $ Map.restrictKeys (vNpcs gv) participants

-- | Non-scrollable inventory text clipped to the available dungeon rows.
inventoryLines :: Int -> Inventory -> [String]
inventoryLines height items =
    take (max 0 height) ("Inventory" : entries)
    where
        orderedItems = itemStackItems items
        entries =
            case orderedItems of
                [] -> ["(empty)"]
                _ ->
                    [ show number ++ ". " ++ itemDescription item
                    | (number, item) <-
                        zip [(1 :: Int) ..] orderedItems
                    ]

-- | The newest six messages, top-padded to keep the log area a fixed height.
messageLogLines :: [String] -> [String]
messageLogLines msgs =
    replicate (messageLogHeight - length recent) "" ++ recent
    where
        recent = drop (length msgs - messageLogHeight) msgs

-- | Item descriptions produced when the player observes a dungeon position.
observe :: Position -> FloorItems -> [String]
observe pos floorItems =
    gemMsgs ++ itemMsgs ++ containerMsgs
    where
        stack = itemStackItems (itemsAt pos floorItems)
        gems = [gem | GemItem gem <- stack]
        looseItems =
            [ item
            | item <- stack
            , case item of
                GemItem _ -> False
                ContainerItem _ -> False
                _ -> True
            ]
        containers = [container | ContainerItem container <- stack]
        gemMsgs =
            case gems of
                [] -> []
                _ ->
                    [ "you see "
                    ++ countNoun (length gems) "gem"
                    ++ " ("
                    ++ intercalate ", " (map (gemColorName . gemColor) gems)
                    ++ ") at "
                    ++ show pos
                    ]
        itemMsgs =
            [ "you see "
            ++ indefiniteItemDescription item
            ++ " at "
            ++ show pos
            | item <- looseItems
            ]
        containerMsgs =
            [ "you see "
            ++ containerObservation container
            ++ " containing "
            ++ countNoun
                (itemStackSize (containerItems container))
                "item"
            ++ " at "
            ++ show pos
            | container <- containers
            ]

        containerObservation container =
            case containerKind container of
                ChestContainer -> "a chest"
                CorpseContainer _ ->
                    "the " ++ itemDescription (ContainerItem container)

        countNoun quantity noun =
            show quantity
            ++ " "
            ++ noun
            ++ if quantity == 1 then "" else "s"

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
