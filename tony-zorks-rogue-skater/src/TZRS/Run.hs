{-# LANGUAGE RecordWildCards #-}

module TZRS.Run
  ( runRulebook
  , runRulebookAndReturnVariables
  , failRuleWithError
  ) where

import Solitude

import Breadcrumbs
import Data.Text.Display ( Display, display )
import TZRS.Rulebook
import qualified Data.Text as T
import TZRS.RuleEffects

-- | Run a rulebook. Mostly this just adds some logging baggage and tidies up the return type.
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
          addAnnotationTo (Just rbSpan) $ "Finished rulebook with result " <> display r
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
