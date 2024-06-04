module TZRS.RuleEffects where

import Solitude

import Breadcrumbs ( Breadcrumbs )
import Effectful.TH ( makeEffect )
import Effectful.Writer.Static.Local
import TZRS.Store
import TZRS.Object
import qualified Data.Vector.Unboxed as V
data Action a b c
data ActionCollection

data World = World
  { objects :: Store Object
  , tileMap :: TileMap
  } deriving stock (Generic)

data TileInfo = TileInfo
  { name :: Text
  , renderable :: Renderable
  , walkable :: Bool
  } deriving stock (Generic, Show)

data TileMap = TileMap
  { tileKinds :: IntMap TileInfo
  , tileMap :: V.Vector Int
  , dimensions :: (Int, Int)
  } deriving stock (Generic, Show)

data Metadata = Metadata
  { pendingQuit :: Bool
  } deriving stock (Generic, Show)

makeFieldLabelsNoPrefix ''World
makeFieldLabelsNoPrefix ''Metadata


data ActionHandler :: Effect where
  ParseAction :: Lens' ActionCollection (Action args vars ret) -> args -> ActionHandler m (Either Text Bool)

makeEffect ''ActionHandler

type RuleEffects es = (
  State Metadata :> es
  , State World :> es
  , Breadcrumbs :> es
  )

class SayableValue s where
  sayTell :: (Writer Text :> es, RuleEffects es) => s -> Eff es ()
  say :: RuleEffects es => s -> Eff es ()
  default say :: RuleEffects es => s -> Eff es ()
  say s = do
    r <- execWriter (sayTell s)
    when (r /= "") $ pass --printText r

instance SayableValue Text where
  sayTell = tell

instance SayableValue String where
  sayTell = tell . toText

type RuleEffectStack = '[
  ActionHandler
  , State Metadata
  , Breadcrumbs
  ]