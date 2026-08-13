module Dungeon.Types
    ( Direction(..)
    , Turn(..)
    , TurnHoldUp(..)
    , GameSettings(..)
    , NpcKind(..)
    , Vitals(..)
    , Stats(..)
    , CharAttributes(..)
    , GemColor(..)
    , Gem(..)
    , Ring(..)
    , Potion(..)
    , Book(..)
    , Scroll(..)
    , Handedness(..)
    , WeaponType(..)
    , Weapon(..)
    , ArmourWeight(..)
    , Armour(..)
    ) where

data Direction
    = North
    | South
    | West
    | East
    deriving (Show, Eq)

data Turn
    = Move Direction
    | Pick
    | Drop
    | Wait
    | Inspect
    deriving (Show, Eq)

data TurnHoldUp = TurnHoldUp

data GameSettings = GameSettings
    { gsWetDurationTurns :: Int
    , gsFightEnterDistance :: Int
    , gsFightLeaveDistance :: Int
    }
    deriving (Show, Eq)

data NpcKind
    = Adder
    | Goblin
    | Rat
    | Kobold
    deriving (Show, Eq)

data Vitals = Vitals
    { vitalHealth :: Int
    , vitalMana :: Int
    , vitalHunger :: Int
    }
    deriving (Show, Eq)

data Stats = Stats
    { statStrength :: Int
    , statIntelligence :: Int
    , statDexterity :: Int
    , statConstitution :: Int
    }
    deriving (Show, Eq)

data CharAttributes = CharAttributes
    { charVitals :: Vitals
    , charStats :: Stats
    }
    deriving (Show, Eq)

data GemColor
    = Red
    | Green
    | Blue
    | Yellow
    deriving (Show, Eq)

data Gem = Gem
    { gemColor :: GemColor
    }
    deriving (Show, Eq)

newtype Ring = Ring
    { ringName :: String
    }
    deriving (Show, Eq)

newtype Potion = Potion
    { potionName :: String
    }
    deriving (Show, Eq)

newtype Book = Book
    { bookName :: String
    }
    deriving (Show, Eq)

newtype Scroll = Scroll
    { scrollName :: String
    }
    deriving (Show, Eq)

data Handedness
    = OneHanded
    | TwoHanded
    deriving (Show, Eq)

data WeaponType
    = Melee Handedness
    | Ranged
    deriving (Show, Eq)

data Weapon = Weapon
    { weaponName :: String
    , weaponType :: WeaponType
    }
    deriving (Show, Eq)

data ArmourWeight
    = Light
    | Heavy
    deriving (Show, Eq)

data Armour = Armour
    { armourName :: String
    , armourWeight :: ArmourWeight
    }
    deriving (Show, Eq)
