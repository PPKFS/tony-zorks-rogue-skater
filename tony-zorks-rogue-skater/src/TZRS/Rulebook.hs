
module TZRS.Rulebook where

import TZRS.Prelude
import Breadcrumbs
import TZRS.RuleEffects

newtype RuleLimitedEffect es a = RuleLimitedEffect (Eff (es : RuleEffectStack) a)

-- | All of the objects in the arguments are READ-ONLY. Whilst they can be swapped out, the
-- refreshVariables function is called to replace and update the objects
class Refreshable av where
  refreshVariables :: forall es. RuleEffects es => av -> Eff es av

instance {-# OVERLAPPABLE #-} Refreshable av where
  refreshVariables = pure
{-}
instance {-# OVERLAPPING #-} Refreshable v => Refreshable (Args v) where
  refreshVariables av = do
    v <- refreshVariables (variables av)
    o <- getThing (tagThing $ source av)
    return $ av { source = o, variables = v }
-}

data Precondition v = Precondition
  { preconditionName :: forall es. RuleEffects es => Eff es Text
  , checkPrecondition :: forall es. RuleEffects es => v -> Eff es Bool
  }
{-}
forPlayer :: Precondition (Args v)
forPlayer = Precondition (pure "actor is the player") $ \v -> do
  p <- getPlayer
  pure $ p == v ^. #source

forPlayer' :: [Precondition (Args v)]
forPlayer' = [forPlayer]

forKind :: ObjectKind -> Precondition (Args (Thing wm))
forKind k = Precondition (pure $ "of kind " <> show k) $ \v -> variables v `isKind` k
-}
-- | A 'Rule' is a wrapped function with a name, that modifies the world (potentially)
-- and any rulebook variables, and might return an outcome (Just) or not (Nothing).
data Rule (x :: [Effect] -> Constraint) v r = Rule
  { name :: Text
  , preconditions :: [Precondition v]
  , runRule :: forall es. (RuleEffects es, Refreshable v, x es) => v -> Eff es (Maybe v, Maybe r)
  }

class Unconstrained t
instance Unconstrained t

-- | A helper for rules which are not implemented and therefore blank.
notImplementedRule ::
  Text
  -> Rule x v r
notImplementedRule n = makeRule' n (do
  ignoreSpan -- this will discard the rule span
  addAnnotation $ "Rule " <> n <> " needs implementing"
  return Nothing)

-- | Make a rule that does not modify the action arguments.
makeRule ::
  Text -- ^ Rule name.
  -> [Precondition v]
  -> (forall es. (RuleEffects es, Refreshable v, x es) => v -> Eff es (Maybe r)) -- ^ Rule function.
  -> Rule x v r
makeRule n c f = Rule n c (fmap (Nothing, ) . f)

-- | Make a rule that has no arguments. this is more convenient to avoid \() ->...
makeRule' ::
  Text -- ^ Rule name.
  -> (forall es. (RuleEffects es, x es) => Eff es (Maybe r)) -- ^ Rule function.
  -> Rule x v r
makeRule' n f = makeRule n [] (const f)

-- | Remove any unwanted return values from a `Rule`.
rulePass ::
  Monad m
  => m (Maybe a)
rulePass = return Nothing

stopTheAction ::
  Monad m
  => m (Maybe Bool)
stopTheAction = return (Just False)

ruleGuard ::
  Monad m
  => Bool
  -> m (Maybe b, Maybe r)
  -> m (Maybe b, Maybe r)
ruleGuard cond f = if cond then f else pure (Nothing, Nothing)

ruleGuardM ::
  Monad m
  => m Bool
  -> m (Maybe b, Maybe r)
  -> m (Maybe b, Maybe r)
ruleGuardM cond f = ifM cond f $ pure (Nothing, Nothing)

ruleWhenJustM ::
  Monad m
  => m (Maybe a)
  -> (a -> m (Maybe b, Maybe r))
  -> m (Maybe b, Maybe r)
ruleWhenJustM mb f = do
  m' <- mb
  maybe (pure (Nothing, Nothing)) f m'

makeFieldLabelsNoPrefix ''Rule

-- | A 'Rulebook' is a computation (ia -> m (Maybe r)) built out of an initialisation (ia -> Maybe v), a default `Maybe r`,
-- and component rules `[(Text, (v -> m (Maybe v, Maybe r))]`
data Rulebook x v r = Rulebook
  { name :: Text
  , defaultOutcome :: Maybe r
  , rules :: [Rule x v r]
  } deriving stock (Generic)

getRuleNames ::
  Rulebook x v r
  -> [Text]
getRuleNames r = map (\r' -> case r' ^. #name of
  "" -> r' ^. #name <> " blank rule"
  x -> x) (rules r)

blankRulebook ::
  Text
  -> Rulebook x v r
blankRulebook n = Rulebook n Nothing []

makeFieldLabelsNoPrefix ''Rulebook

-- | Add a rule to a rulebook last.
addRuleLast ::
  Rule x v r
  -> Rulebook x v r
  -> Rulebook x v r
addRuleLast r = #rules %~ (++ [r])

-- | Add a rule to a rulebook first.
addRuleFirst ::
  Rule x v r
  -> Rulebook x v r
  -> Rulebook x v r
addRuleFirst r = #rules %~ (r :)
