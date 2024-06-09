
module TZRS.Object where

import TZRS.Prelude

import TZRS.Entity
import TZRS.Geometry

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

data ObjectSpecifics = ObjectSpecifics

newtype Colour = Colour { toWord32 :: Word32 }
  deriving stock (Show, Read, Generic)

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

instance Display Object where
  displayBuilder = const "object"

-- | By generalising `Eq`, we can compare two objects of different kinds. Trivially this is always `False`,
-- but it does allow comparing a `Thing` and an `AnyObject`.
objectEquals ::
  HasID a
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

instance HasID Object where
  getID = objectId
