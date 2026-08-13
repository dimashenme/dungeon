{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Dungeon.Logic
    ( Position
    , Direction(..)
    , Turn(..)
    , FightMode(..)
    , WetStatus(..)
    , TurnTick(..)
    , Player(..)
    , RandomSeed(..)
    , GameSettings(..)
    , defaultGameSettings
    , GameRuntimeState(..)
    , GameT
    , GameState(..)
    , GameView(..)
    , isWalkable
    , isWater
    , currentTileWet
    , initPlayer
    , initGameState
    , fightMode
    , turnNumber
    , wetStatusState
    , wetStatusMessages
    , playerState
    , wieldedState
    , gameState
    , toGameView
    ) where

import Prelude hiding (init)
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState, get)
import qualified Control.Monad.State.Class as State
import Control.Monad.State.Strict (runState)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.MSF.Except
    ( performOnFirstSample
    , throwOn
    )
import Control.Monad.Trans.MSF.Maybe (exceptToMaybeS)
import Control.Monad.Trans.RWS.Strict (RWST)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Bool (bool)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.MonadicStreamFunction hiding (count, next)
import qualified Data.MonadicStreamFunction as MSF
import qualified Data.Set as Set
import Data.Set (Set)
import Dungeon.Combinators
    ( countdownFrom
    , restartOn
    , runMaybeStateS
    , sampleAndHold
    )
import Dungeon.DungeonLayout (DungeonLayout(..))
import Dungeon.GameData
    ( defaultGameSettings
    , initialPlayerAttributes
    )
import Dungeon.Item
import Dungeon.ItemState
import Dungeon.Map
    ( Dungeon
    , Position
    , isWalkable
    , isWater
    , movePosition
    )
import Dungeon.Npc
import Dungeon.Random (RandomSeed(..), selectRandomSubset)
import Dungeon.Types
    ( CharAttributes
    , Direction(..)
    , GameSettings(..)
    , Turn(..)
    , TurnHoldUp(..)
    )

data FightMode
    = Exploring
    | Fighting (Set NpcId)
    deriving (Show, Eq)

data WetStatus = Dry | Wet
    deriving (Show, Eq)

data Player = Player
    { plPos :: Position
    , plAttributes :: CharAttributes
    , plInventory :: Inventory
    , plWielded :: Maybe ItemId
    , plWetStatus :: WetStatus
    , plWetCountdown :: Int
    , plFightMode :: FightMode
    }
    deriving (Show, Eq)

data GameRuntimeState = GameRuntimeState
    { rtRandomSeed :: RandomSeed
    , rtNextItemId :: ItemId
    }
    deriving (Show, Eq)

type GameT m = RWST GameSettings [String] GameRuntimeState m

initPlayer :: Position -> Player
initPlayer pos =
    Player
        { plPos = pos
        , plAttributes = initialPlayerAttributes
        , plInventory = emptyItemStack
        , plWielded = Nothing
        , plWetStatus = Dry
        , plWetCountdown = 0
        , plFightMode = Exploring
        }

nextPos
    :: MonadWriter [String] m
    => MSF (ExceptT TurnHoldUp m) (Turn, Dungeon, Position) Position
nextPos = arrM $ \(turn, dungeon, pos) ->
    case turn of
        Move dir ->
            let attempted = movePosition dir pos
            in if isWalkable dungeon attempted
                then pure attempted
                else do
                    tell ["*bump*"]
                    throwE TurnHoldUp
        _ -> pure pos

fightMode
    :: MonadReader GameSettings m
    => FightMode
    -> MSF m (Position, NpcPopulation) FightMode
fightMode initMode =
    performOnFirstSample $ do
        enter <- asks gsFightEnterDistance
        leave <- asks gsFightLeaveDistance
        pure (mealy (step enter leave) (participants initMode))
    where
        step enter leave (position, npcs) current =
            let next =
                    Map.foldlWithKey'
                        (include enter leave position current)
                        Set.empty
                        npcs
                mode
                    | Set.null next = Exploring
                    | otherwise = Fighting next
            in (mode, next)

        participants mode =
            case mode of
                Exploring -> Set.empty
                Fighting npcs -> npcs

        include enter leave position current included ident npc
            | distance position (npcPosition npc) <= enter =
                Set.insert ident included
            | ident `Set.member` current
                && distance position (npcPosition npc) <= leave =
                    Set.insert ident included
            | otherwise = included

        distance (x1, y1) (x2, y2) =
            abs (x1 - x2) + abs (y1 - y2)

data TurnTick = TurnTick
    deriving (Show, Eq)

currentTileWet :: Monad m => MSF m (Dungeon, Position) WetStatus
currentTileWet = arr (bool Dry Wet . uncurry isWater)

turnNumber :: Monad m => Int -> MSF m (Maybe TurnTick) Int
turnNumber init =
    sampleAndHold init (MSF.count >>> arr (+ init))

wetStatusState
    :: MonadReader GameSettings m
    => WetStatus
    -> Int
    -> MSF m (WetStatus, Maybe TurnTick) (WetStatus, Int)
wetStatusState initStatus initCount =
    performOnFirstSample $ do
        duration <- asks gsWetDurationTurns
        pure
            $ arr (\(tileStatus, tickEvt) -> tileStatus <$ tickEvt)
            >>> sampleAndHold
                (initStatus, max 0 initCount)
                ( arr (const () &&& (== Wet))
                    >>> restartOn
                        (countdownFrom $ max 0 initCount - 1)
                        (countdownFrom duration)
                    >>> (arr (bool Dry Wet . (> 0)) &&& arr id)
                )

wetStatusMessages
    :: MonadWriter [String] m
    => WetStatus
    -> MSF m WetStatus ()
wetStatusMessages init =
    feedback init (arrM step)
    where
        step (status', status) = do
            when (status == Dry && status' == Wet) $
                tell ["you are wet"]
            when (status == Wet && status' == Dry) $
                tell ["you are dry"]
            pure ((), status')

playerState
    :: ( MonadReader GameSettings m
       , MonadWriter [String] m
       )
    => Player
    -> MSF
         m
         (Dungeon, Player, Maybe TurnTick)
         Player
playerState init = proc (dungeon, player, tickEvt) -> do
    tileStatus <- currentTileWet -< (dungeon, plPos player)
    (wet, count) <-
        wetStatusState
            (plWetStatus init)
            (plWetCountdown init)
            -< (tileStatus, tickEvt)
    _ <- wetStatusMessages (plWetStatus init) -< wet
    returnA -<
        player
            { plWetStatus = wet
            , plWetCountdown = count
            }

wieldedState
    :: MonadError TurnHoldUp m
    => Maybe ItemId
    -> MSF m (Turn, Inventory) (Maybe ItemId)
wieldedState init = feedback init $ arrM $ \((turn, inventory), wielded) -> do
    wielded' <-
        case turn of
            Wield ident ->
                case lookupItem ident inventory of
                    Just (WeaponItem _) -> pure (Just ident)
                    _ -> throwError TurnHoldUp
            Drop ident
                | wielded == Just ident -> pure Nothing
            _ -> pure wielded
    pure (wielded', wielded')

data GameState = GameState
    { stPlayer :: Player
    , stDungeon :: Dungeon
    , stFloorItems :: FloorItems
    , stNpcs :: NpcPopulation
    , stTurnNumber :: Int
    , stRandomSeed :: RandomSeed
    , stNextItemId :: ItemId
    }
    deriving (Show, Eq)

data GameView = GameView
    { vPlayer :: Player
    , vDungeon :: Dungeon
    , vFloorItems :: FloorItems
    , vNpcs :: NpcPopulation
    , vTurnNumber :: Int
    , vMessages :: [String]
    }
    deriving (Show, Eq)

initGameState :: Player -> DungeonLayout -> RandomSeed -> GameState
initGameState player layout seed =
    GameState
        { stPlayer = player
        , stDungeon = layoutDungeon layout
        , stFloorItems = layoutItems layout
        , stNpcs = layoutNpcs layout
        , stTurnNumber = 0
        , stRandomSeed = seed
        , stNextItemId = layoutNextItemId layout
        }

gameState
    :: ( MonadFix m
       , MonadReader GameSettings m
       , MonadWriter [String] m
       , MonadState GameRuntimeState m
       )
    => GameState
    -> MSF m Turn GameState
gameState init = proc turn -> do
    attempted <-
        runMaybeStateS
            (exceptToMaybeS $ gameStateAttempt init)
            -< turn
    let tickEvt = TurnTick <$ attempted
    currentTurn <- turnNumber (stTurnNumber init) -< tickEvt
    state <- sampleAndHold init (arr id) -< attempted
    player <-
        playerState initPlayerState
            -< (stDungeon state, stPlayer state, tickEvt)
    returnA -<
        state
            { stPlayer = player
            , stTurnNumber = currentTurn
            }
    where
        initPlayerState = stPlayer init

gameStateAttempt
    :: ( MonadFix m
       , MonadReader GameSettings m
       , MonadWriter [String] m
       , MonadState GameRuntimeState m
       )
    => GameState
    -> MSF (ExceptT TurnHoldUp m) Turn GameState
gameStateAttempt init = proc turn -> do
    rec
        pos <- iPre (plPos initPlayerState) -< pos'
        npcs <- iPre (stNpcs init) -< npcs'
        floorItems <- iPre (stFloorItems init) -< floorItems'
        inventory <- iPre initInventory -< inventory'

        _ <- throwOn TurnHoldUp -< turn == Inspect
        pos_ <- nextPos -< (turn, stDungeon init, pos)
        killedEvt <- npcKilledEvts -< (pos_, npcs)
        pickupEvt <- pickupEvts -< (turn, pos, floorItems)
        dropEvt <- dropEvts -< (turn, pos, inventory)

        pos' <-
            if isJust killedEvt
                then returnA -< pos
                else returnA -< pos_

        floorRemoveEvt <- floorEvtsFromPickup -< pickupEvt
        inventoryAddEvt <- inventoryEvtsFromPickup -< pickupEvt
        floorPlaceEvt <- floorEvtsFromDrop -< dropEvt
        inventoryRemoveEvt <- inventoryEvtsFromDrop -< dropEvt
        corpsePlaceEvt <-
            floorEvtsFromKill freshItemId selectLoot -< killedEvt
        let floorEvt = floorRemoveEvt <|> floorPlaceEvt <|> corpsePlaceEvt
        let inventoryEvt = inventoryAddEvt <|> inventoryRemoveEvt
        floorItems' <-
            floorItemsState (stFloorItems init) -< floorEvt
        inventory' <-
            inventoryState initInventory -< inventoryEvt
        wielded' <-
            wieldedState (plWielded initPlayerState) -< (turn, inventory)

        npcs' <-
            npcsState (stDungeon init) (stNpcs init)
                -< ( pos'
                   , killedEvt
                   )

        fight <-
            fightMode (plFightMode initPlayerState) -< (pos', npcs')
        runtime <- arrM (const get) -< fight
        let player =
                initPlayerState
                    { plPos = pos'
                    , plInventory = inventory'
                    , plWielded = wielded'
                    , plFightMode = fight
                    }

    returnA -<
        GameState
            { stPlayer = player
            , stDungeon = stDungeon init
            , stFloorItems = floorItems'
            , stNpcs = npcs'
            , stTurnNumber = stTurnNumber init
            , stRandomSeed = rtRandomSeed runtime
            , stNextItemId = rtNextItemId runtime
            }
    where
        initPlayerState = stPlayer init
        initInventory = plInventory initPlayerState

freshItemId :: MonadState GameRuntimeState m => m ItemId
freshItemId = State.state $ \runtime ->
    let ident@(ItemId next) = rtNextItemId runtime
    in ( ident
       , runtime { rtNextItemId = ItemId (next + 1) }
       )

selectLoot :: MonadState GameRuntimeState m => Inventory -> m Inventory
selectLoot inventory = State.state $ \runtime ->
    let (loot, nextSeed) =
            runState
                (selectRandomSubset (itemStackToList inventory))
                (rtRandomSeed runtime)
    in ( itemStackFromList loot
       , runtime { rtRandomSeed = nextSeed }
       )

toGameView :: [String] -> GameState -> GameView
toGameView msgs state =
    GameView
        { vPlayer = stPlayer state
        , vDungeon = stDungeon state
        , vFloorItems = stFloorItems state
        , vNpcs = stNpcs state
        , vTurnNumber = stTurnNumber state
        , vMessages = msgs
        }
