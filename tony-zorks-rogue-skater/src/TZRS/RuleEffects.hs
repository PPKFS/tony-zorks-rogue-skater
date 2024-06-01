module TZRS.RuleEffects
  ( RuleEffects
  , ActionHandler(..)
  , parseAction
  , RuleEffectStack
  ) where

import Solitude

import Breadcrumbs ( Breadcrumbs )
import Effectful.TH ( makeEffect )
import Effectful.Writer.Static.Local
data Action a b c
data ActionCollection
data Metadata


data ActionHandler :: Effect where
  ParseAction :: Lens' ActionCollection (Action args vars ret) -> args -> ActionHandler m (Either Text Bool)

makeEffect ''ActionHandler

type RuleEffects es = (
  State Metadata :> es
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