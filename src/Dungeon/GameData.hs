module Dungeon.GameData
    ( npcName
    , npcGlyph
    , gemColorName
    , redGem
    , greenGem
    , blueGem
    , yellowGem
    , ringOfProtection
    , potionOfHealing
    , bookOfForgottenPaths
    , scrollOfMapping
    , shortSword
    , greatAxe
    , shortBow
    , leatherJerkin
    , plateMail
    , initialPlayerAttributes
    , initialNpcAttributes
    , itemGlyph
    , itemDescription
    , indefiniteItemDescription
    , containerDescription
    , containerGlyph
    , observe
    , attackConfirmationMessage
    , nothingToPickupMessage
    , nothingToLootMessage
    , pickupMessage
    , lootMessage
    , dropMessage
    , npcKilledMessage
    , defaultGameSettings
    ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Dungeon.Item
    ( Container(..)
    , ContainerKind(..)
    , FloorItems
    , Item(..)
    , itemStackItems
    , itemStackSize
    , itemsAt
    )
import Dungeon.Map (Position)
import Dungeon.Types

npcName :: NpcKind -> String
npcName kind =
    case kind of
        Adder -> "adder"
        Goblin -> "goblin"
        Rat -> "rat"
        Kobold -> "kobold"

npcGlyph :: NpcKind -> Char
npcGlyph kind =
    case kind of
        Adder -> 'a'
        Goblin -> 'g'
        Rat -> 'r'
        Kobold -> 'k'

gemColorName :: GemColor -> String
gemColorName color =
    case color of
        Red -> "red"
        Green -> "green"
        Blue -> "blue"
        Yellow -> "yellow"

redGem, greenGem, blueGem, yellowGem :: Gem
redGem = Gem Red
greenGem = Gem Green
blueGem = Gem Blue
yellowGem = Gem Yellow

ringOfProtection :: Ring
ringOfProtection = Ring "ring of protection"

potionOfHealing :: Potion
potionOfHealing = Potion "potion of healing"

bookOfForgottenPaths :: Book
bookOfForgottenPaths = Book "book of forgotten paths"

scrollOfMapping :: Scroll
scrollOfMapping = Scroll "scroll of mapping"

shortSword, greatAxe, shortBow :: Weapon
shortSword = Weapon "short sword" (Melee OneHanded)
greatAxe = Weapon "great axe" (Melee TwoHanded)
shortBow = Weapon "short bow" Ranged

leatherJerkin, plateMail :: Armour
leatherJerkin = Armour "leather jerkin" Light
plateMail = Armour "plate mail" Heavy

initialPlayerAttributes :: CharAttributes
initialPlayerAttributes =
    CharAttributes
        { charVitals = Vitals 10 10 0
        , charStats = initialStats
        }

initialNpcAttributes :: CharAttributes
initialNpcAttributes =
    CharAttributes
        { charVitals = Vitals 10 0 0
        , charStats = initialStats
        }

initialStats :: Stats
initialStats = Stats 10 10 10 10

itemGlyph :: Item -> Char
itemGlyph item =
    case item of
        RingItem _ -> '='
        GemItem _ -> '%'
        PotionItem _ -> '!'
        BookItem _ -> '+'
        ScrollItem _ -> '?'
        WeaponItem _ -> ')'
        ArmourItem _ -> '['
        ContainerItem container -> containerGlyph container

itemDescription :: Item -> String
itemDescription item =
    case item of
        RingItem ring -> ringName ring
        GemItem gem -> gemColorName (gemColor gem) ++ " gem"
        PotionItem potion -> potionName potion
        BookItem book -> bookName book
        ScrollItem scroll -> scrollName scroll
        WeaponItem weapon ->
            weaponName weapon
            ++ " ("
            ++ weaponTypeDescription (weaponType weapon)
            ++ ")"
        ArmourItem armour ->
            armourName armour
            ++ " ("
            ++ armourWeightDescription (armourWeight armour)
            ++ " armour)"
        ContainerItem container -> containerDescription container

indefiniteItemDescription :: Item -> String
indefiniteItemDescription item =
    article description ++ " " ++ description
    where
        description = itemDescription item
        article [] = "a"
        article (c : _)
            | toLower c `elem` "aeiou" = "an"
            | otherwise = "a"

containerDescription :: Container -> String
containerDescription container =
    case containerKind container of
        ChestContainer -> "chest"
        CorpseContainer kind -> "corpse of the " ++ npcName kind

containerGlyph :: Container -> Char
containerGlyph container =
    case containerKind container of
        ChestContainer -> 'C'
        CorpseContainer _ -> ';'

-- | Item descriptions produced when the player observes a dungeon position.
observe :: Position -> FloorItems -> [String]
observe pos floorItems =
    map (atPosition pos . ("you see " ++)) descriptions
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
        descriptions =
            gemDescriptions
            ++ map indefiniteItemDescription looseItems
            ++ map containerObservation containers
        gemDescriptions =
            case gems of
                [] -> []
                _ ->
                    [ countNoun (length gems) "gem"
                    ++ " ("
                    ++ intercalate ", " (map (gemColorName . gemColor) gems)
                    ++ ")"
                    ]

        containerObservation container =
            article
            ++ " containing "
            ++ countNoun (itemStackSize (containerItems container)) "item"
            where
                article =
                    case containerKind container of
                        ChestContainer -> "a chest"
                        CorpseContainer _ ->
                            "the " ++ itemDescription (ContainerItem container)

        countNoun quantity noun =
            show quantity
            ++ " "
            ++ noun
            ++ if quantity == 1 then "" else "s"

nothingToLootMessage :: String
nothingToLootMessage = "nothing to loot"

nothingToPickupMessage :: String
nothingToPickupMessage = "nothing to pick up"

attackConfirmationMessage :: NpcKind -> String
attackConfirmationMessage kind =
    "really attack " ++ npcName kind ++ "? (yes/no)"

pickupMessage :: Position -> Item -> String
pickupMessage pos item =
    atPosition pos ("picked up " ++ indefiniteItemDescription item)

lootMessage :: Position -> Item -> Container -> String
lootMessage pos item container =
    atPosition pos
        ("looted "
        ++ indefiniteItemDescription item
        ++ " from the "
        ++ containerDescription container)

dropMessage :: Position -> Item -> String
dropMessage pos item =
    atPosition pos ("dropped " ++ indefiniteItemDescription item)

npcKilledMessage :: Position -> NpcKind -> String
npcKilledMessage pos kind =
    atPosition pos ("you kill the " ++ npcName kind)

atPosition :: Position -> String -> String
atPosition pos message = message ++ " at " ++ show pos

weaponTypeDescription :: WeaponType -> String
weaponTypeDescription weaponType' =
    case weaponType' of
        Melee OneHanded -> "one-handed melee weapon"
        Melee TwoHanded -> "two-handed melee weapon"
        Ranged -> "ranged weapon"

armourWeightDescription :: ArmourWeight -> String
armourWeightDescription weight =
    case weight of
        Light -> "light"
        Heavy -> "heavy"

-------------------------------------------------------------------------------
-- Game settings
-------------------------------------------------------------------------------

defaultGameSettings :: GameSettings
defaultGameSettings =
    GameSettings
        { gsWetDurationTurns = 5
        , gsFightEnterDistance = 1
        , gsFightLeaveDistance = 5
        }
