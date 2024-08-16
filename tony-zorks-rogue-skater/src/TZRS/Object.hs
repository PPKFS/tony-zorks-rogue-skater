
module TZRS.Object where

import TZRS.Prelude

import TZRS.Entity
import Rogue.Colour
import Rogue.FieldOfView.Visibility
import Rogue.Property.Has
import Rogue.ObjectQuery ( HasSpecifics(..) )

-- | Pointed set class; Monoid without the operation, or the dreaded default typeclass.
class Pointed s where
  identityElement :: s

instance {-# OVERLAPPABLE #-} Monoid m => Pointed m where
  identityElement = mempty

newtype ObjectKind = ObjectKind
  { unObjectKind :: Text
  } deriving stock (Eq, Show)
    deriving newtype (Read, Ord, IsString, Monoid, Semigroup)

-- | A `Timestamp` is used to date events that modify, add, or remove objects.
-- Currently these aren't...used for anything.
newtype Timestamp = Timestamp
  { unTimestamp :: Int
  } deriving stock (Show, Read, Generic)
    deriving newtype (Eq, Num, Enum, Ord, Real, Integral)

type ObjectText = Text

data ObjectSpecifics =
  PlayerSpecifics PlayerSpecifics
  | MonsterSpecifics MonsterSpecifics
  deriving stock (Generic)

data CombatStats = CombatStats
  { maxHP :: Int
  , hp :: Int
  , defense :: Int
  , power :: Int
  }

data PlayerSpecifics = Player
  { viewshed :: Viewshed
  , combat :: CombatStats
  } deriving stock (Generic)

data MonsterSpecifics = Monster
  { viewshed :: Viewshed
  , combat :: CombatStats
  } deriving stock (Generic)

makePrisms ''ObjectSpecifics

instance MayHaveProperty ObjectSpecifics Viewshed where
  propertyAT = (_MonsterSpecifics % #viewshed) `thenATraverse` (_PlayerSpecifics % #viewshed)

instance MayHaveProperty ObjectSpecifics CombatStats where
  propertyAT = (_MonsterSpecifics % #combat) `thenATraverse` (_PlayerSpecifics % #combat)

data Renderable = Renderable
  { glyph :: Char
  , foreground :: Colour
  , background :: Colour
  } deriving stock (Show, Read, Generic)

data Object = Object
  { name :: ObjectText
  , description :: ObjectText
  , objectId :: Entity
  , objectType :: ObjectKind
  , creationTime :: Timestamp
  , modifiedTime :: Timestamp
  , position :: V2
  , renderable :: Renderable
  , objectData :: ObjectSpecifics
  , occupiesTile :: Bool
  } deriving stock (Generic)

makeFieldLabelsNoPrefix ''Object

instance HasID Object where
  type Id Object = Entity
  getID = objectId

instance Display Object where
  displayBuilder = const "object"

instance HasSpecifics Object where
  type Specifics Object = ObjectSpecifics
  specificsL = #objectData

-- | By generalising `Eq`, we can compare two objects of different kinds. Trivially this is always `False`,
-- but it does allow comparing a `Thing` and an `AnyObject`.
objectEquals ::
  (Id a ~ Id b)
  => (Eq (Id a))
  => HasID a
  => HasID b
  => a
  -> b
  -> Bool
objectEquals = (. getID) . (==) . getID

instance Eq Object where
  (==) = objectEquals

-- | Maybe I'll need this instance for something or other?
instance Ord Object where
  compare = (. creationTime) . compare . creationTime

forKind ::
  ObjectKind
  -> Object
  -> Eff es a
  -> Eff es (Maybe a)
forKind k o f = if objectType o == k then Just <$> f else return Nothing