{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Dungeon.DungeonLayout (
    Position,
    Dungeon,
    DungeonLayout(..),
    DungeonLayoutM,
    ChestRef,
    NpcRef,
    ItemDestination,
    room,
    digX,
    digY,
    water,
    placeItem,
    placeChest,
    placeNpc,
    compose,
    isWalkable,
    isWater,
    buildDijkstra
) where

import Control.Monad.State.Strict (StateT, runStateT, state)
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Dungeon.Item
    ( FloorItems
    , Item
    , ItemId(..)
    , appendItemStacks
    , chestItem
    , itemStackFromList
    )
import Dungeon.Map
    ( Dungeon
    , Position
    , buildDijkstra
    , isWalkable
    , isWater
    )
import qualified Dungeon.Map as DungeonMap
import Dungeon.Npc
    ( NpcBehaviour(..)
    , NpcId(..)
    , NpcPopulation
    , initNpc
    )
import Dungeon.Types (NpcKind)

-- | Compiled terrain and the objects initially placed in it.
data DungeonLayout = DungeonLayout
    { layoutDungeon :: Dungeon
    , layoutItems :: FloorItems
    , layoutNpcs :: NpcPopulation
    , layoutNextItemId :: ItemId
    }
    deriving (Show, Eq)

-------------------------------------------------------------------------------
-- DungeonLayoutM
-------------------------------------------------------------------------------

data Command
    = Terrain DungeonMap.TerrainCommand
    | PlaceItem ItemPlacement ItemId Item
    | PlaceChest ChestRef ItemId Position
    | PlaceNpc NpcRef Position NpcKind NpcBehaviour

data ChestOwner

data NpcOwner

newtype LayoutRef owner = LayoutRef Int
    deriving (Show, Eq)

type ChestRef = LayoutRef ChestOwner

type NpcRef = LayoutRef NpcOwner

data ItemPlacement
    = AtFloor Position
    | InChest ChestRef
    | InNpc NpcRef

-- | A destination accepted by 'placeItem'.
class ItemDestination destination where
    itemPlacement :: destination -> ItemPlacement

instance (x ~ Int, y ~ Int) => ItemDestination (x, y) where
    itemPlacement = AtFloor

instance ItemDestination ChestRef where
    itemPlacement = InChest

instance ItemDestination NpcRef where
    itemPlacement = InNpc

data LayoutState = LayoutState
    { nextRef :: Int
    , nextItemId :: ItemId
    }

-- | Opaque monad for describing dungeon terrain and initial contents.
newtype DungeonLayoutM a = DungeonLayoutM
    { runDungeonLayoutM :: StateT LayoutState (Writer [Command]) a
    }
    deriving (Functor, Applicative, Monad)

emit :: Command -> DungeonLayoutM ()
emit cmd =
    DungeonLayoutM (lift (tell [cmd]))

freshRef :: DungeonLayoutM (LayoutRef owner)
freshRef =
    DungeonLayoutM $ state $ \layoutState ->
        let next = nextRef layoutState
        in ( LayoutRef next
           , layoutState { nextRef = next + 1 }
           )

freshItemId :: DungeonLayoutM ItemId
freshItemId =
    DungeonLayoutM $ state $ \layoutState ->
        let ident@(ItemId next) = nextItemId layoutState
        in ( ident
           , layoutState { nextItemId = ItemId (next + 1) }
           )

room :: Position -> Position -> DungeonLayoutM ()
room pos1 pos2 = emit (Terrain $ DungeonMap.room pos1 pos2)

-- | Dig an inclusive horizontal corridor.  If either endpoint meets a room
-- only at a corner, compose widens the endpoint into a traversable doorway.
digX :: Position -> Int -> DungeonLayoutM ()
digX pos len = emit (Terrain $ DungeonMap.digX pos len)

-- | Dig an inclusive vertical corridor.  If either endpoint meets a room only
-- at a corner, compose widens the endpoint into a traversable doorway.
digY :: Position -> Int -> DungeonLayoutM ()
digY pos len = emit (Terrain $ DungeonMap.digY pos len)

water :: Position -> DungeonLayoutM ()
water = emit . Terrain . DungeonMap.water

placeItem
    :: ItemDestination destination
    => destination
    -> Item
    -> DungeonLayoutM ()
placeItem destination item =
    do
        ident <- freshItemId
        emit (PlaceItem (itemPlacement destination) ident item)

placeChest :: Position -> DungeonLayoutM ChestRef
placeChest pos = do
    ref <- freshRef
    ident <- freshItemId
    emit (PlaceChest ref ident pos)
    pure ref

placeNpc
    :: Position
    -> NpcKind
    -> NpcBehaviour
    -> DungeonLayoutM NpcRef
placeNpc pos kind behaviour = do
    ref <- freshRef
    emit (PlaceNpc ref pos kind behaviour)
    pure ref

scaleCmds :: Float -> [Command] -> [Command]
scaleCmds scale =
    map scaleCmd
    where
        scaleCrd n = round (scale * fromIntegral n)
        scalePt (x, y) = (scaleCrd x, scaleCrd y)
        scaleCmd cmd =
            case cmd of
                Terrain terrain -> Terrain terrain
                PlaceItem placement ident item ->
                    PlaceItem (scalePlacement placement) ident item
                PlaceChest ref ident pos ->
                    PlaceChest ref ident (scalePt pos)
                PlaceNpc ref pos kind behaviour ->
                    PlaceNpc
                        ref
                        (scalePt pos)
                        kind
                        (scaleBehaviour behaviour)
        scalePlacement placement =
            case placement of
                AtFloor pos -> AtFloor (scalePt pos)
                InChest ref -> InChest ref
                InNpc ref -> InNpc ref
        scaleBehaviour behaviour =
            case behaviour of
                Stationary -> Stationary
                Patrolling axis (lower, upper) ->
                    Patrolling
                        axis
                        (scaleCrd lower, scaleCrd upper)

-- | Compose terrain and initial contents at the requested coordinate scale.
--
-- Every room and tunnel endpoint is rounded to the nearest integer after
-- scaling. Object positions use the same transformation. Tunnel lengths are
-- derived from their rounded endpoints, keeping doors aligned with the rooms
-- they connect at non-integer scales.
compose :: Float -> DungeonLayoutM a -> DungeonLayout
compose scale dung =
    let
        ((_, finalLayoutState), rawCmds) =
            runWriter
                (runStateT
                    (runDungeonLayoutM dung)
                    (LayoutState
                        { nextRef = 0
                        , nextItemId = ItemId 0
                        }))
        cmds = scaleCmds scale rawCmds
        dungeon =
            DungeonMap.generateDungeon
                scale
                [ terrain
                | Terrain terrain <- rawCmds
                ]
        chestContentsFor ref =
            itemStackFromList
            [ (ident, item)
            | PlaceItem (InChest ref') ident item <- cmds
            , ref' == ref
            ]
        placedItems =
            [ (pos, ident, item)
            | cmd <- cmds
            , (pos, ident, item) <-
                case cmd of
                    PlaceItem (AtFloor pos) ident item ->
                        [(pos, ident, item)]
                    PlaceChest ref ident pos ->
                        [(pos, ident, chestItem (chestContentsFor ref))]
                    _ -> []
            ]
        items =
            foldl'
                (\floorItems (pos, ident, item) ->
                    Map.insertWith
                        (flip appendItemStacks)
                        pos
                        (itemStackFromList [(ident, item)])
                        floorItems)
                Map.empty
                placedItems
        npcCommands =
            [ (ref, pos, kind, behaviour)
            | PlaceNpc ref pos kind behaviour <- cmds
            ]
        npcs = Map.fromList (zipWith compileNpc [0 ..] npcCommands)
        compileNpc ident (ref, pos, kind, behaviour) =
            ( NpcId ident
            , initNpc
                    pos
                    kind
                    behaviour
                    (npcContentsFor ref)
            )
        npcContentsFor ref =
            itemStackFromList
            [ (ident, item)
            | PlaceItem (InNpc ref') ident item <- cmds
            , ref' == ref
            ]
    in DungeonLayout
        { layoutDungeon = dungeon
        , layoutItems = items
        , layoutNpcs = npcs
        , layoutNextItemId = nextItemId finalLayoutState
        }
