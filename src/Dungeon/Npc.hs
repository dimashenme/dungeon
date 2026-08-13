{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module Dungeon.Npc
    ( NpcId(..)
    , PatrolAxis(..)
    , PatrolEnd(..)
    , NpcBehaviour(..)
    , NpcBehaviourState(..)
    , Npc(..)
    , NpcPopulation
    , NpcKilledEvt(..)
    , npcAt
    , initNpc
    , patrol
    , npcKilledEvts
    , floorEvtsFromKill
    , npcsState
    ) where

import Prelude hiding (foldl', init)
import Control.Arrow (arr, returnA, (>>>))
import Control.Monad (guard)
import Control.Monad.Trans.MSF.Except (switch)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MonadicStreamFunction
    ( MSF
    , arrM
    , feedback
    , mapMaybeS
    )
import Data.Set (Set)
import qualified Data.Set as Set
import Dungeon.Combinators (reconcileMSFs)
import Dungeon.GameData
    ( initialNpcAttributes
    , npcKilledMessage
    )
import Dungeon.Item
    ( Inventory
    , ItemId
    , corpseItem
    )
import Dungeon.ItemState (FloorItemsEvt(..))
import Dungeon.Map (Dungeon, Position, isWalkable)
import Dungeon.Types (CharAttributes, NpcKind)

newtype NpcId = NpcId Int
    deriving (Show, Eq, Ord)

data PatrolAxis
    = Horizontal
    | Vertical
    deriving (Show, Eq)

data PatrolEnd
    = LowerBound
    | UpperBound
    deriving (Show, Eq)

data NpcBehaviour
    = Stationary
    | Patrolling PatrolAxis (Int, Int)
    deriving (Show, Eq)

data NpcBehaviourState
    = StationaryState
    | PatrollingToward PatrolEnd
    deriving (Show, Eq)

data Npc = Npc
    { npcPosition :: Position
    , npcKind :: NpcKind
    , npcAttributes :: CharAttributes
    , npcInventory :: Inventory
    , npcBehaviour :: NpcBehaviour
    , npcBehaviourState :: NpcBehaviourState
    }
    deriving (Show, Eq)

type NpcPopulation = Map NpcId Npc

data NpcKilledEvt = NpcKilledEvt
    { killedNpcId :: NpcId
    , killedNpc :: Npc
    }
    deriving (Show, Eq)

-- | Find the NPC occupying a position. Identity order breaks an invalid
-- duplicate-position tie deterministically.
npcAt :: Position -> NpcPopulation -> Maybe (NpcId, Npc)
npcAt pos = find ((== pos) . npcPosition . snd) . Map.toAscList

initNpc :: Position -> NpcKind -> NpcBehaviour -> Inventory -> Npc
initNpc pos kind behaviour inv =
    Npc
        { npcPosition = pos
        , npcKind = kind
        , npcAttributes = initialNpcAttributes
        , npcInventory = inv
        , npcBehaviour = behaviour
        , npcBehaviourState =
            case behaviour of
                Stationary -> StationaryState
                Patrolling axis (_, upper)
                    | coordinate axis pos >= upper ->
                        PatrollingToward LowerBound
                    | otherwise ->
                        PatrollingToward UpperBound
        }
    where
        coordinate Horizontal = fst
        coordinate Vertical = snd

patrol :: PatrolAxis -> (Int, Int) -> NpcBehaviour
patrol axis (bound1, bound2) =
    Patrolling axis (min bound1 bound2, max bound1 bound2)

type NpcDecision = (Int, Int)

npcKilledEvts
    :: Monad m
    => MSF
         m
         (Position, NpcPopulation)
         (Maybe NpcKilledEvt)
npcKilledEvts = arr $ \(pos, population) -> do
    (ident, npc) <- npcAt pos population
    pure
        NpcKilledEvt
            { killedNpcId = ident
            , killedNpc = npc
            }

floorEvtsFromKill
    :: MonadWriter [String] m
    => m ItemId
    -> (Inventory -> m Inventory)
    -> MSF m (Maybe NpcKilledEvt) (Maybe FloorItemsEvt)
floorEvtsFromKill freshItemId selectLoot = mapMaybeS $ arrM $ \event -> do
    let npc = killedNpc event
        pos = npcPosition npc
    ident <- freshItemId
    loot <- selectLoot (npcInventory npc)
    tell [npcKilledMessage pos (npcKind npc)]
    pure
        (PlaceItemEvt
            pos
            Nothing
            ident
            (corpseItem (npcKind npc) loot))

npcsState
    :: Monad m
    => Dungeon
    -> NpcPopulation
    -> MSF
         m
         ( Position
         , Maybe NpcKilledEvt
         )
         NpcPopulation
npcsState dungeon init =
    feedback init $ proc ((playerPos, killedEvt), population) -> do
        let survivors =
                maybe population
                    (\evt -> Map.delete (killedNpcId evt) population)
                    killedEvt
        decisions <- npcDecisions -< survivors
        let decisions' =
                Map.intersectionWith
                    (\npc (decision, state) ->
                        (npc { npcBehaviourState = state }, decision))
                    survivors
                    decisions
            population' = resolveNpcDecisions dungeon playerPos decisions'
        returnA -< (population', population')

npcDecisions
    :: Monad m
    => MSF m NpcPopulation (Map NpcId (NpcDecision, NpcBehaviourState))
npcDecisions = reconcileMSFs start
    where
        start
            :: Monad m
            => NpcId
            -> Npc
            -> MSF m Npc (NpcDecision, NpcBehaviourState)
        start _ init =
            case npcBehaviour init of
                Stationary ->
                    arr $ const ((0, 0), StationaryState)
                Patrolling axis bounds ->
                    arr npcPosition
                        >>> patrolBehaviour axis bounds initialTarget
            where
                initialTarget =
                    case npcBehaviourState init of
                        PatrollingToward target -> target
                        StationaryState -> UpperBound

resolveNpcDecisions
    :: Dungeon
    -> Position
    -> Map NpcId (Npc, NpcDecision)
    -> NpcPopulation
resolveNpcDecisions dungeon playerPos decisions =
    snd $ Map.mapAccum moveOne initial decisions
    where
        initial =
            Set.fromList
                [ npcPosition npc
                | (npc, _) <- Map.elems decisions
                ]

        moveOne unavailable decision@(npc, _) =
            let pos = npcPosition npc
                withoutCurrent = Set.delete pos unavailable
                npc' =
                    resolveDecision
                        dungeon
                        playerPos
                        withoutCurrent
                        decision
            in (Set.insert (npcPosition npc') withoutCurrent, npc')

resolveDecision
    :: Dungeon
    -> Position
    -> Set Position
    -> (Npc, NpcDecision)
    -> Npc
resolveDecision dungeon playerPos unavailable (npc, (dx, dy)) =
    npc { npcPosition = destination }
    where
        pos@(x, y) = npcPosition npc
        pos' = (x + dx, y + dy)
        destination
            | pos' == playerPos = pos
            | not (isWalkable dungeon pos') = pos
            | pos' `Set.member` unavailable = pos
            | otherwise = pos'

patrolBehaviour
    :: Monad m
    => PatrolAxis
    -> (Int, Int)
    -> PatrolEnd
    -> MSF m Position (NpcDecision, NpcBehaviourState)
patrolBehaviour axis (lower, upper) initialTarget
    | lower == upper =
        arr $ const ((0, 0), PatrollingToward initialTarget)
    | otherwise = go initialTarget
    where
        go LowerBound =
            switch (toward lower LowerBound UpperBound) go
        go UpperBound =
            switch (toward upper UpperBound LowerBound) go

        toward target st st' = arr $ \pos ->
            let distance = target - coordinate pos
            in ( ( alongAxis $ signum distance
                 , PatrollingToward st
                 )
               , st' <$ guard (distance == 0)
               )

        coordinate =
            case axis of
                Horizontal -> fst
                Vertical -> snd

        alongAxis step =
            case axis of
                Horizontal -> (step, 0)
                Vertical -> (0, step)
