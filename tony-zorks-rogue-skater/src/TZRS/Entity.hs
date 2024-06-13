module TZRS.Entity
  ( -- * Entities
    Entity(..)
  , HasID(..)
  , TaggedEntity(unTag)
  , unsafeTagEntity
  ) where

import TZRS.Prelude

-- | An object ID.
newtype Entity = Entity
  { unID :: Int
  } deriving stock (Show, Generic)
    deriving newtype (Eq, Num, Read, Bounded, Hashable, Enum, Ord, Real, Integral)

-- | Typeclass and function for extracting an entity from something (to store references)
class HasID n where
  -- | Get an ID.
  getID :: n -> Entity

-- | Trivial instance.
instance HasID Entity where
  getID = id

-- | For pretty printing in logs.
instance Display Entity where
  displayBuilder i = "(ID: " <> show i <> ")"

-- | An entity tagged with a phantom @tag@ for keeping some semblance of type safety
-- when indirectly storing references to other objects. The tagging mechanisms are in
-- @Yaifl.Model.Objects.Tag@.
newtype TaggedEntity tag = TaggedEntity { unTag :: Entity }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Num, Read, Bounded, Hashable, Enum, Ord, Real, Integral)

-- | Tag an entity without a witness.
unsafeTagEntity ::
  Entity -- ^ Entity to tag
  -> TaggedEntity tag
unsafeTagEntity = TaggedEntity

instance HasID (TaggedEntity t) where
  getID = unTag
