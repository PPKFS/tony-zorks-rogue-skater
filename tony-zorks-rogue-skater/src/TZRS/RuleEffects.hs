module TZRS.RuleEffects where

import Solitude

import Breadcrumbs ( Breadcrumbs )
import Effectful.TH ( makeEffect )
import Effectful.Writer.Static.Local
import TZRS.World
import TZRS.Object
import Rogue.ObjectQuery
data Action a b c
data ActionCollection


data Metadata = Metadata
  { pendingQuit :: Bool
  } deriving stock (Generic, Show)

makeFieldLabelsNoPrefix ''Metadata

data ActionHandler :: Effect where
  ParseAction :: Lens' ActionCollection (Action args vars ret) -> args -> ActionHandler m (Either Text Bool)

makeEffect ''ActionHandler

type RuleEffects es = (
  State Metadata :> es
  , ObjectQuery Object :> es
  , State World :> es
  , Breadcrumbs :> es
  --, TileMap :> es
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