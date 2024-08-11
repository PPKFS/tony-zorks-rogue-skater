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

type RuleEffectStack = '[
  ActionHandler
  , State Metadata
  , Breadcrumbs
  ]