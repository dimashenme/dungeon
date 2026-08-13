module Dungeon.Item
    ( ItemId(..)
    , ContainerKind(..)
    , Container(..)
    , Item(..)
    , ItemStack
    , Inventory
    , FloorItems
    , emptyItemStack
    , itemStackFromList
    , itemStackToList
    , itemStackItems
    , itemStackIds
    , itemStackSize
    , itemStackNull
    , appendItemStacks
    , chestItem
    , corpseItem
    , itemsAt
    , pushItem
    , popItem
    , removeItem
    , lookupItem
    , replaceItem
    , pickupCandidate
    , removeFromFloorStack
    , placeInContainer
    , firstContainer
    ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Foldable (asum, toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq, ViewL(..), (<|))
import qualified Data.Sequence as Seq
import Data.Word (Word64)
import Dungeon.Map (Position)
import Dungeon.Types
    ( Armour
    , Book(..)
    , Gem
    , NpcKind
    , Potion
    , Ring
    , Scroll
    , Weapon
    )

newtype ItemId = ItemId Word64
    deriving (Show, Eq, Ord)

data ContainerKind
    = ChestContainer
    | CorpseContainer NpcKind
    deriving (Show, Eq)

data Container = Container
    { containerKind :: ContainerKind
    , containerPortable :: Bool
    , containerItems :: ItemStack
    }
    deriving (Show, Eq)

data Item
    = RingItem Ring
    | GemItem Gem
    | PotionItem Potion
    | BookItem Book
    | ScrollItem Scroll
    | WeaponItem Weapon
    | ArmourItem Armour
    | ContainerItem Container
    deriving (Show, Eq)

data ItemStack = ItemStack
    { stackItems :: Map ItemId Item
    , stackOrder :: Seq ItemId
    }
    deriving (Show, Eq)

type Inventory = ItemStack

type FloorItems = Map Position ItemStack

emptyItemStack :: ItemStack
emptyItemStack = ItemStack Map.empty Seq.empty

itemStackFromList :: [(ItemId, Item)] -> ItemStack
itemStackFromList = foldr (uncurry pushItem) emptyItemStack

itemStackToList :: ItemStack -> [(ItemId, Item)]
itemStackToList stack =
    [ (ident, item)
    | ident <- toList (stackOrder stack)
    , Just item <- [Map.lookup ident (stackItems stack)]
    ]

itemStackItems :: ItemStack -> [Item]
itemStackItems = map snd . itemStackToList

itemStackIds :: ItemStack -> [ItemId]
itemStackIds = map fst . itemStackToList

itemStackSize :: ItemStack -> Int
itemStackSize = Seq.length . stackOrder

itemStackNull :: ItemStack -> Bool
itemStackNull = Seq.null . stackOrder

appendItemStacks :: ItemStack -> ItemStack -> ItemStack
appendItemStacks left right =
    itemStackFromList (itemStackToList left ++ itemStackToList right)

chestItem :: ItemStack -> Item
chestItem = ContainerItem . Container ChestContainer True

corpseItem :: NpcKind -> ItemStack -> Item
corpseItem kind =
    ContainerItem . Container (CorpseContainer kind) False

itemsAt :: Position -> FloorItems -> ItemStack
itemsAt pos = Map.findWithDefault emptyItemStack pos

pushItem :: ItemId -> Item -> ItemStack -> ItemStack
pushItem ident item stack =
    ItemStack
        { stackItems = Map.insert ident item (stackItems stack)
        , stackOrder =
            ident <| Seq.filter (/= ident) (stackOrder stack)
        }

popItem :: ItemStack -> Maybe ((ItemId, Item), ItemStack)
popItem stack =
    case Seq.viewl (stackOrder stack) of
        EmptyL -> Nothing
        ident :< rest -> do
            item <- Map.lookup ident (stackItems stack)
            pure
                ( (ident, item)
                , ItemStack
                    { stackItems = Map.delete ident (stackItems stack)
                    , stackOrder = rest
                    }
                )

removeItem :: ItemId -> ItemStack -> Maybe (Item, ItemStack)
removeItem ident stack = do
    item <- Map.lookup ident (stackItems stack)
    pure
        ( item
        , ItemStack
            { stackItems = Map.delete ident (stackItems stack)
            , stackOrder = Seq.filter (/= ident) (stackOrder stack)
            }
        )

lookupItem :: ItemId -> ItemStack -> Maybe Item
lookupItem ident = Map.lookup ident . stackItems

replaceItem :: ItemId -> Item -> ItemStack -> Maybe ItemStack
replaceItem ident item stack = do
    guard (Map.member ident (stackItems stack))
    pure stack
        { stackItems = Map.insert ident item (stackItems stack)
        }

pickupCandidate :: ItemStack -> Maybe (ItemId, Item)
pickupCandidate stack =
    case firstContainer stack of
        Nothing -> fst <$> popItem stack
        Just (ident, container) ->
            fst <$> popItem (containerItems container)
            <|> (ident, ContainerItem container)
                <$ guard (containerPortable container)

removeFromFloorStack :: ItemId -> ItemStack -> Maybe ItemStack
removeFromFloorStack ident stack =
    snd <$> removeItem ident stack
    <|> asum
        [ removeFromContainer containerId container
        | (containerId, ContainerItem container) <- itemStackToList stack
        ]
    where
        removeFromContainer containerId container = do
            (_, contents') <- removeItem ident (containerItems container)
            replaceItem
                containerId
                (ContainerItem container { containerItems = contents' })
                stack

placeInContainer
    :: ItemId
    -> ItemId
    -> Item
    -> ItemStack
    -> Maybe ItemStack
placeInContainer containerId ident item stack = do
    ContainerItem container <- lookupItem containerId stack
    replaceItem
        containerId
        (ContainerItem
            container
                { containerItems =
                    pushItem ident item (containerItems container)
                })
        stack

firstContainer :: ItemStack -> Maybe (ItemId, Container)
firstContainer stack =
    listToMaybe
        [ (ident, container)
        | (ident, ContainerItem container) <- itemStackToList stack
        ]
