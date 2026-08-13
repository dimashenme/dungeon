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
    , pickupMessage
    , dropMessage
    , npcKilledMessage
    , defaultGameSettings
    ) where

import Data.Char (toLower)
import Dungeon.Item
    ( Container(..)
    , ContainerKind(..)
    , Item(..)
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

pickupMessage :: Position -> Item -> String
pickupMessage pos item =
    "picked up "
        ++ indefiniteItemDescription item
        ++ " at "
        ++ show pos

dropMessage :: Position -> Item -> Maybe Container -> String
dropMessage pos item target =
    "dropped "
        ++ indefiniteItemDescription item
        ++ destinationDescription target
        ++ " at "
        ++ show pos
    where
        destinationDescription destination =
            case destination of
                Just container ->
                    case containerKind container of
                        ChestContainer -> " into a chest"
                        CorpseContainer _ ->
                            " into the " ++ containerDescription container
                Nothing -> ""

npcKilledMessage :: Position -> NpcKind -> String
npcKilledMessage pos kind =
    "you kill the "
        ++ npcName kind
        ++ " at "
        ++ show pos

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
