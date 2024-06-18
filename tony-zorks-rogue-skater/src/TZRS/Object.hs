
module TZRS.Object where

import TZRS.Prelude

import TZRS.Entity
import Rogue.Colour
import Rogue.FieldOfView.Visibility
import Rogue.Property.Has
import Rogue.ObjectQuery

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

data PlayerSpecifics = Player
  { viewshed :: Viewshed
  } deriving stock (Generic)

data MonsterSpecifics = Monster
  { viewshed :: Viewshed
  } deriving stock (Generic)

makePrisms ''ObjectSpecifics

instance MayHaveProperty ObjectSpecifics Viewshed where
  propertyAT = (_MonsterSpecifics % #viewshed) `thenATraverse` (_PlayerSpecifics % #viewshed)

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
