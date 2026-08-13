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
import Data.MonadicStreamFunction
    ( MSF
    , arrM
    , mapMaybeS
    )
import qualified Data.Map.Strict as Map
import Dungeon.Combinators (accumulateMaybe)
import Dungeon.GameData
    ( dropMessage
    , lootMessage
    , pickupMessage
    )
import Dungeon.Item
    ( Container(..)
    , FloorItems
    , Inventory
    , Item(..)
    , ItemId
    , itemStackNull
    , itemsAt
    , lookupItem
    , pushItem
    , removeFromContainer
    , removeItem
    )
import Dungeon.Map (Position)
import Dungeon.Types (Turn(..), TurnHoldUp(..))

data PickupEvt = PickupEvt
    { pickupPosition :: Position
    , pickupContainer :: Maybe (ItemId, Container)
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
    = PlaceItemEvt Position ItemId Item
    | RemoveItemEvt Position (Maybe ItemId) ItemId
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
    let stack = itemsAt pos floorItems
        emit source ident =
            maybe
                (throwError TurnHoldUp)
                (pure . Just . PickupEvt pos source ident)
                (lookupItem ident $ maybe stack (containerItems . snd) source)
    in case turn of
        Pick ident -> emit Nothing ident
        Loot containerId ident ->
            case lookupItem containerId stack of
                Just (ContainerItem container) ->
                    emit (Just (containerId, container)) ident
                _ -> throwError TurnHoldUp
        _ -> pure Nothing

dropEvts
    :: MonadError TurnHoldUp m
    => MSF
         m
         (Turn, Position, Inventory)
         (Maybe DropEvt)
dropEvts = arrM $ \(turn, pos, inventory) ->
    case turn of
        Drop ident ->
            maybe
                (throwError TurnHoldUp)
                (pure . Just . DropEvt pos ident)
                (lookupItem ident inventory)
        _ -> pure Nothing

floorEvtsFromPickup
    :: Monad m
    => MSF m (Maybe PickupEvt) (Maybe FloorItemsEvt)
floorEvtsFromPickup = mapMaybeS $ arr $ \event ->
    RemoveItemEvt
        (pickupPosition event)
        (fst <$> pickupContainer event)
        (pickupItemId event)

inventoryEvtsFromPickup
    :: MonadWriter [String] m
    => MSF m (Maybe PickupEvt) (Maybe InventoryEvt)
inventoryEvtsFromPickup = mapMaybeS $ arrM $ \event -> do
    let message =
            case pickupContainer event of
                Nothing -> pickupMessage
                Just (_, container) -> \pos item ->
                    lootMessage pos item container
    tell [message (pickupPosition event) (pickupItem event)]
    pure (AddItemEvt (pickupItemId event) (pickupItem event))

floorEvtsFromDrop
    :: MonadWriter [String] m
    => MSF m (Maybe DropEvt) (Maybe FloorItemsEvt)
floorEvtsFromDrop = mapMaybeS $ arrM $ \event -> do
    let pos = dropPosition event
        ident = dropItemId event
        item = dropItem event
    tell [dropMessage pos item]
    pure (PlaceItemEvt pos ident item)

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
                RemoveItemEvt pos source ident ->
                    let stack = itemsAt pos floorItems
                        removed =
                            case source of
                                Nothing -> snd <$> removeItem ident stack
                                Just containerId ->
                                    removeFromContainer containerId ident stack
                    in case removed of
                        Just stack' -> replaceAt pos stack' floorItems
                        Nothing -> floorItems
                PlaceItemEvt pos ident item ->
                    replaceAt
                        pos
                        (pushItem ident item $ itemsAt pos floorItems)
                        floorItems

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
