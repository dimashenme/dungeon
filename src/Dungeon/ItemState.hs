{-# LANGUAGE FlexibleContexts #-}

module Dungeon.ItemState
    ( PickupEvt(..)
    , DropEvt(..)
    , FloorItemsEvt(..)
    , InventoryEvt(..)
    , pickupEvts
    , dropEvts
    , floorEvtsFromPickup
    , inventoryEvtsFromPickup
    , floorEvtsFromDrop
    , inventoryEvtsFromDrop
    , floorItemsState
    , inventoryState
    ) where

import Control.Arrow (arr)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Maybe (fromMaybe)
import Data.MonadicStreamFunction
    ( MSF
    , arrM
    , mapMaybeS
    )
import qualified Data.Map.Strict as Map
import Dungeon.Combinators (accumulateMaybe)
import Dungeon.GameData
    ( dropMessage
    , pickupMessage
    )
import Dungeon.Item
    ( FloorItems
    , Inventory
    , Item
    , ItemId
    , firstContainer
    , itemStackNull
    , itemsAt
    , placeInContainer
    , popItem
    , pickupCandidate
    , pushItem
    , removeFromFloorStack
    , removeItem
    )
import Dungeon.Map (Position)
import Dungeon.Types (Turn(..), TurnHoldUp(..))

data PickupEvt = PickupEvt
    { pickupPosition :: Position
    , pickupItemId :: ItemId
    , pickupItem :: Item
    }
    deriving (Show, Eq)

data DropEvt = DropEvt
    { dropPosition :: Position
    , dropItemId :: ItemId
    , dropItem :: Item
    }
    deriving (Show, Eq)

data FloorItemsEvt
    = PlaceItemEvt Position (Maybe ItemId) ItemId Item
    | RemoveItemEvt Position ItemId
    deriving (Show, Eq)

data InventoryEvt
    = AddItemEvt ItemId Item
    | RemoveEvt ItemId
    deriving (Show, Eq)

pickupEvts
    :: MonadError TurnHoldUp m
    => MSF
         m
         (Turn, Position, FloorItems)
         (Maybe PickupEvt)
pickupEvts = arrM $ \(turn, pos, floorItems) ->
    case turn of
        Pick ->
            maybe
                (throwError TurnHoldUp)
                (pure . Just . uncurry (PickupEvt pos))
                (pickupCandidate $ itemsAt pos floorItems)
        _ -> pure Nothing

dropEvts
    :: MonadError TurnHoldUp m
    => MSF
         m
         (Turn, Position, Inventory)
         (Maybe DropEvt)
dropEvts = arrM $ \(turn, pos, inventory) ->
    case turn of
        Drop ->
            maybe
                (throwError TurnHoldUp)
                (pure . Just . uncurry (DropEvt pos))
                (fst <$> popItem inventory)
        _ -> pure Nothing

floorEvtsFromPickup
    :: Monad m
    => MSF m (Maybe PickupEvt) (Maybe FloorItemsEvt)
floorEvtsFromPickup = mapMaybeS $ arr $ \event ->
    RemoveItemEvt
        (pickupPosition event)
        (pickupItemId event)

inventoryEvtsFromPickup
    :: MonadWriter [String] m
    => MSF m (Maybe PickupEvt) (Maybe InventoryEvt)
inventoryEvtsFromPickup = mapMaybeS $ arrM $ \event -> do
    tell [pickupMessage (pickupPosition event) (pickupItem event)]
    pure (AddItemEvt (pickupItemId event) (pickupItem event))

floorEvtsFromDrop
    :: MonadWriter [String] m
    => MSF
         m
         (Maybe (DropEvt, FloorItems))
         (Maybe FloorItemsEvt)
floorEvtsFromDrop = mapMaybeS $ arrM $ \(event, floorItems) -> do
    let pos = dropPosition event
        ident = dropItemId event
        item = dropItem event
        target = firstContainer (itemsAt pos floorItems)
    tell [dropMessage pos item (snd <$> target)]
    pure
        (PlaceItemEvt
            pos
            (fst <$> target)
            ident
            item)

inventoryEvtsFromDrop
    :: Monad m
    => MSF m (Maybe DropEvt) (Maybe InventoryEvt)
inventoryEvtsFromDrop = mapMaybeS $ arr (RemoveEvt . dropItemId)

floorItemsState
    :: Monad m
    => FloorItems
    -> MSF m (Maybe FloorItemsEvt) FloorItems
floorItemsState = accumulateMaybe applyFloorItemsEvt
    where
        applyFloorItemsEvt event floorItems =
            case event of
                RemoveItemEvt pos ident ->
                    case removeFromFloorStack ident (itemsAt pos floorItems) of
                        Just stack -> replaceAt pos stack floorItems
                        Nothing -> floorItems
                PlaceItemEvt pos target ident item ->
                    let stack = itemsAt pos floorItems
                        placeOnFloor = pushItem ident item stack
                        stack' =
                            fromMaybe placeOnFloor
                                $ target >>= \containerId ->
                                    placeInContainer
                                        containerId ident item stack
                    in replaceAt pos stack' floorItems

        replaceAt pos stack
            | itemStackNull stack = Map.delete pos
            | otherwise = Map.insert pos stack

inventoryState
    :: Monad m
    => Inventory
    -> MSF m (Maybe InventoryEvt) Inventory
inventoryState = accumulateMaybe applyInventoryEvt
    where
        applyInventoryEvt event inventory =
            case event of
                AddItemEvt ident item -> pushItem ident item inventory
                RemoveEvt ident -> maybe inventory snd (removeItem ident inventory)
