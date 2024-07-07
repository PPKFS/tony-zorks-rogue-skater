{-# LANGUAGE RecordWildCards #-}

module TZRS.Rulebook where

import TZRS.Prelude
import Breadcrumbs
import TZRS.RuleEffects
import qualified Data.Text as T

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

runRulebook ::
  HasCallStack
  => (Refreshable v, Display v, Display re)
  => RuleEffects es
  => x es
  => Maybe SpanID
  -> Rulebook x v re
  -> v
  -> Eff es (Maybe re)
runRulebook mbSpanId rb ia = do
  mvre <- runRulebookAndReturnVariables mbSpanId rb ia
  return $ mvre >>= snd

-- | Run a rulebook and return possibly an outcome; the two levels of `Maybe` are for:
-- Nothing -> the rulebook arguments were not parsed correctly
-- Just (v, Nothing) -> the rulebook ran successfully, but had no definite outcome
-- Just (v, Just re) -> the rulebook ran successfully with outcome re
runRulebookAndReturnVariables ::
  forall x v es re.
  HasCallStack
  => (Refreshable v, Display v, Display re)
  => x es
  => RuleEffects es
  => Maybe SpanID
  -> Rulebook x v re
  -> v
  -> Eff es (Maybe (v, Maybe re))
runRulebookAndReturnVariables mbSpanId Rulebook{..} args =
  -- ignore empty rulebooks to avoid logging spam
  if null rules
    then pure Nothing
    else maybe (withSpan "rulebook" name) (\f x -> x f) mbSpanId $ \rbSpan -> do
      addTagToSpan rbSpan "arguments" $ display args
      -- run the actual rules
      (v, r1) <- processRuleList rbSpan rules args
      let outcome = (v, r1 <|> defaultOutcome)
      addTagTo (Just rbSpan) "outcome" (display $ snd outcome)
      return (Just outcome)

-- | Mostly this is a very complicated "run a list of functions until you get
-- something that isn't a Nothing, or a default if you get to the end".
processRuleList ::
  (Refreshable v, Display v, Display re)
  => RuleEffects es
  => x es
  => SpanID
  -> [Rule x v re]
  -> v
  -> Eff es (v, Maybe re)
processRuleList _ [] v = return (v, Nothing)
processRuleList rbSpan (Rule{..} : xs) args = do
  mbRes <- (if T.null name then withSpan' "rule" "some unnamed rule" else withSpan' "rule" name) $ do
    reqsMet <- checkPreconditions args preconditions
    if reqsMet then do
        Right <$> runRule args
    else pure (Left ())
  case mbRes of
    -- we failed the precondition, so we just keep going
    Left _ -> processRuleList rbSpan xs args
    Right (v, res) -> do
      whenJust v (\v' -> addAnnotationTo (Just rbSpan) $ "Updated rulebook variables to " <> display v')
      newArgs <- refreshVariables $ fromMaybe args v
      case res of
        Nothing -> processRuleList rbSpan xs newArgs
        Just r -> do
          addAnnotationTo (Just rbSpan) ("Finished rulebook with result " <> display r)
          return (newArgs, Just r)

checkPreconditions :: RuleEffects es => v -> [Precondition v] -> Eff es Bool
checkPreconditions v conds = do
  failedConds <- filterM (\p -> fmap not $ inject $ p `checkPrecondition` v) conds
  if null failedConds then pure True else do
      ns <- mapM (\c -> inject $ preconditionName c) failedConds
      addAnnotation $ mconcat $ ["failed to meet preconditions: "] <> ns
      pure False

-- | Return a failure (Just False) from a rule and log a string to the
-- debug log.
failRuleWithError ::
  Breadcrumbs :> es
  => Text -- ^ Error message.
  -> Eff es (Maybe Bool)
failRuleWithError = const (return (Just False)) <=< addAnnotation
