{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.Rewrite (
    performRewrite,
    rewriteStep,
    RewriteFailed (..),
    RewriteResult (..),
    RewriteTrace (..),
    pattern CollectRewriteTraces,
    pattern NoCollectRewriteTraces,
    runRewriteT,
) where

import Control.Applicative ((<|>))
import Control.Exception qualified as Exception (throw)
import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (..), ask, asks, withReaderT)
import Control.Monad.Trans.State.Strict (StateT (runStateT), get, modify)
import Data.Aeson (object, (.=))
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Data (Proxy)
import Data.Hashable qualified as Hashable
import Data.List (intersperse, partition)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence (Seq, (|>))
import Data.Set qualified as Set
import Data.Text as Text (Text, pack)
import Numeric.Natural
import Prettyprinter

import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.LLVM as LLVM (API)
import Booster.Log
import Booster.Pattern.ApplyEquations (
    EquationFailure (..),
    SimplifierCache,
    evaluatePattern,
    simplifyConstraint,
 )
import Booster.Pattern.Base
import Booster.Pattern.Bool
import Booster.Pattern.Index qualified as Idx
import Booster.Pattern.Match (
    FailReason (ArgLengthsDiffer, SubsortingError),
    MatchResult (MatchFailed, MatchIndeterminate, MatchSuccess),
    MatchType (Rewrite),
    SortError,
    matchTerms,
 )
import Booster.Pattern.Pretty
import Booster.Pattern.Util
import Booster.Prettyprinter
import Booster.SMT.Interface qualified as SMT
import Booster.Syntax.Json.Externalise (externaliseTerm)
import Booster.Util (Flag (..))

newtype RewriteT io a = RewriteT
    { unRewriteT ::
        ReaderT RewriteConfig (StateT SimplifierCache (ExceptT (RewriteFailed "Rewrite") io)) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

data RewriteConfig = RewriteConfig
    { definition :: KoreDefinition
    , llvmApi :: Maybe LLVM.API
    , smtSolver :: Maybe SMT.SMTContext
    , doTracing :: Flag "CollectRewriteTraces"
    , logger :: Logger LogMessage
    , prettyModifiers :: ModifiersRep
    }

instance MonadIO io => LoggerMIO (RewriteT io) where
    getLogger = RewriteT $ asks logger
    getPrettyModifiers = RewriteT $ asks prettyModifiers

    withLogger modL (RewriteT m) = RewriteT $ withReaderT (\cfg@RewriteConfig{logger} -> cfg{logger = modL logger}) m

pattern CollectRewriteTraces :: Flag "CollectRewriteTraces"
pattern CollectRewriteTraces = Flag True

pattern NoCollectRewriteTraces :: Flag "CollectRewriteTraces"
pattern NoCollectRewriteTraces = Flag False

runRewriteT ::
    LoggerMIO io =>
    Flag "CollectRewriteTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    RewriteT io a ->
    io (Either (RewriteFailed "Rewrite") (a, SimplifierCache))
runRewriteT doTracing definition llvmApi smtSolver cache m = do
    logger <- getLogger
    prettyModifiers <- getPrettyModifiers
    runExceptT
        . flip runStateT cache
        . flip runReaderT RewriteConfig{definition, llvmApi, smtSolver, doTracing, logger, prettyModifiers}
        . unRewriteT
        $ m

throw :: LoggerMIO io => RewriteFailed "Rewrite" -> RewriteT io a
throw = RewriteT . lift . lift . throwE

getDefinition :: LoggerMIO io => RewriteT io KoreDefinition
getDefinition = RewriteT $ definition <$> ask

{- | Performs a rewrite step (using suitable rewrite rules from the
   definition).

  The result can be a failure (providing some context for why it
  failed), or a rewritten pattern with a new term and possibly new
  additional constraints.
-}
rewriteStep ::
    LoggerMIO io =>
    [Text] ->
    [Text] ->
    Pattern ->
    RewriteT io (RewriteResult Pattern)
rewriteStep cutLabels terminalLabels pat = do
    def <- getDefinition
    let getIndex =
            if null def.attributes.indexCells
                then Idx.kCellTermIndex
                else Idx.compositeTermIndex def.attributes.indexCells
        termIdx = getIndex pat.term
    when (Idx.hasNone termIdx) $ throw (TermIndexIsNone pat.term)
    let
        indexes = Set.toList $ Idx.coveringIndexes termIdx
        rulesFor i = fromMaybe Map.empty $ Map.lookup i def.rewriteTheory
        rules =
            map snd . Map.toAscList . Map.unionsWith (<>) $ map rulesFor indexes

    -- process one priority group at a time (descending priority),
    -- until a result is obtained or the entire rewrite fails.
    processGroups pat rules
  where
    processGroups ::
        LoggerMIO io =>
        Pattern ->
        [[RewriteRule "Rewrite"]] ->
        RewriteT io (RewriteResult Pattern)
    processGroups pattr [] =
        pure $ RewriteStuck pattr
    processGroups pattr (rules : rest) = do
        -- try all rules of the priority group. This will immediately
        -- fail the rewrite if anything is uncertain (unification,
        -- definedness, rule conditions)
        results <- filter (/= NotApplied) <$> mapM (applyRule pattr) rules

        -- simplify and filter out bottom states

        -- At the moment, there is no point in calling simplify on the conditions of the
        -- resulting patterns again, since we already pruned any rule applications
        -- which resulted in one of the conditions being bottom.
        -- Also, our current simplifier cannot deduce bottom from a combination of conditions,
        -- so unless the original pattern contained bottom, we won't gain anything from
        -- calling the simplifier on the original conditions which came with the term.

        let labelOf = fromMaybe "" . (.ruleLabel) . (.attributes)
            ruleLabelOrLocT = renderOneLineText . ruleLabelOrLoc
            uniqueId = (.uniqueId) . (.attributes)

        case results of
            -- no rules in this group were applicable
            [] -> processGroups pattr rest
            _ -> case concatMap (\case Applied x -> [x]; _ -> []) results of
                [] ->
                    -- all remaining branches are trivial, i.e. rules which did apply had an ensures condition which evaluated to false
                    -- if, all the other groups only generate a not applicable or trivial rewrites,
                    -- then we return a `RewriteTrivial`.
                    processGroups pattr rest >>= \case
                        RewriteStuck{} -> pure $ RewriteTrivial pat
                        other -> pure other
                -- all branches but one were either not applied or trivial
                [(r, x)]
                    | labelOf r `elem` cutLabels ->
                        pure $ RewriteCutPoint (labelOf r) (uniqueId r) pat x
                    | labelOf r `elem` terminalLabels ->
                        pure $ RewriteTerminal (labelOf r) (uniqueId r) x
                    | otherwise ->
                        pure $ RewriteFinished (Just $ ruleLabelOrLocT r) (Just $ uniqueId r) x
                -- at this point, there were some Applied rules and potentially some Trivial ones.
                -- here, we just return all the applied rules in a `RewriteBranch`
                rxs ->
                    pure $
                        RewriteBranch pat $
                            NE.fromList $
                                map (\(r, p) -> (ruleLabelOrLocT r, uniqueId r, p)) rxs

data RewriteRuleAppResult a
    = Applied a
    | NotApplied
    | Trivial
    deriving (Show, Eq, Functor)

newtype RewriteRuleAppT m a = RewriteRuleAppT {runRewriteRuleAppT :: m (RewriteRuleAppResult a)}
    deriving (Functor)

instance Monad m => Applicative (RewriteRuleAppT m) where
    pure = RewriteRuleAppT . return . Applied
    {-# INLINE pure #-}
    mf <*> mx = RewriteRuleAppT $ do
        mb_f <- runRewriteRuleAppT mf
        case mb_f of
            NotApplied -> return NotApplied
            Trivial -> return Trivial
            Applied f -> do
                mb_x <- runRewriteRuleAppT mx
                case mb_x of
                    NotApplied -> return NotApplied
                    Trivial -> return Trivial
                    Applied x -> return (Applied (f x))
    {-# INLINE (<*>) #-}
    m *> k = m >> k
    {-# INLINE (*>) #-}

instance Monad m => Monad (RewriteRuleAppT m) where
    return = pure
    {-# INLINE return #-}
    x >>= f = RewriteRuleAppT $ do
        v <- runRewriteRuleAppT x
        case v of
            Applied y -> runRewriteRuleAppT (f y)
            NotApplied -> return NotApplied
            Trivial -> return Trivial
    {-# INLINE (>>=) #-}

instance MonadTrans RewriteRuleAppT where
    lift :: Monad m => m a -> RewriteRuleAppT m a
    lift = RewriteRuleAppT . fmap Applied
    {-# INLINE lift #-}

instance Monad m => MonadFail (RewriteRuleAppT m) where
    fail _ = RewriteRuleAppT (return NotApplied)
    {-# INLINE fail #-}

instance MonadIO m => MonadIO (RewriteRuleAppT m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance LoggerMIO m => LoggerMIO (RewriteRuleAppT m) where
    withLogger l (RewriteRuleAppT m) = RewriteRuleAppT $ withLogger l m

{- | Tries to apply one rewrite rule:

 * Unifies the LHS term with the pattern term
 * Ensures that the unification is a _match_ (one-sided substitution)
 * prunes any rules that turn out to have trivially-false side conditions
 * returns the rule and the resulting pattern if successful, otherwise Nothing

If it cannot be determined whether the rule can be applied or not, an
exception is thrown which indicates the exact reason why (this will
abort the entire rewrite).
-}
applyRule ::
    forall io.
    LoggerMIO io =>
    Pattern ->
    RewriteRule "Rewrite" ->
    RewriteT io (RewriteRuleAppResult (RewriteRule "Rewrite", Pattern))
applyRule pat@Pattern{ceilConditions} rule =
    withRuleContext rule $
        runRewriteRuleAppT $
            getPrettyModifiers >>= \case
                ModifiersRep (_ :: FromModifiersT mods => Proxy mods) -> do
                    def <- lift getDefinition
                    -- unify terms
                    subst <- withContext CtxMatch $ case matchTerms Rewrite def rule.lhs pat.term of
                        MatchFailed (SubsortingError sortError) -> do
                            withContext CtxError $ logPretty' @mods sortError
                            failRewrite $ RewriteSortError rule pat.term sortError
                        MatchFailed err@ArgLengthsDiffer{} -> do
                            withContext CtxError $
                                logPretty' @mods err
                            failRewrite $ InternalMatchError $ renderText $ pretty' @mods err
                        MatchFailed reason -> do
                            withContext CtxFailure $ logPretty' @mods reason
                            fail "Rule matching failed"
                        MatchIndeterminate remainder -> do
                            withContext CtxIndeterminate $
                                logMessage $
                                    WithJsonMessage (object ["remainder" .= (bimap externaliseTerm externaliseTerm <$> remainder)]) $
                                        renderOneLineText $
                                            "Uncertain about match with rule. Remainder:"
                                                <+> ( hsep $
                                                        punctuate comma $
                                                            map (\(t1, t2) -> pretty' @mods t1 <+> "==" <+> pretty' @mods t2) $
                                                                NE.toList remainder
                                                    )
                            failRewrite $ RuleApplicationUnclear rule pat.term remainder
                        MatchSuccess substitution -> do
                            withContext CtxSuccess $ do
                                logMessage rule
                                withContext CtxSubstitution
                                    $ logMessage
                                    $ WithJsonMessage
                                        ( object
                                            ["substitution" .= (bimap (externaliseTerm . Var) externaliseTerm <$> Map.toList substitution)]
                                        )
                                    $ renderOneLineText
                                    $ "Substitution:"
                                        <+> ( hsep $
                                                intersperse "," $
                                                    map (\(k, v) -> pretty' @mods k <+> "->" <+> pretty' @mods v) $
                                                        Map.toList substitution
                                            )
                            pure substitution

                    -- Also fail the whole rewrite if a rule applies but may introduce
                    -- an undefined term.
                    unless (null rule.computedAttributes.notPreservesDefinednessReasons) $ do
                        withContext CtxDefinedness . withContext CtxAbort $
                            logMessage $
                                renderOneLineText $
                                    "Uncertain about definedness of rule due to:"
                                        <+> hsep (intersperse "," $ map pretty rule.computedAttributes.notPreservesDefinednessReasons)
                        failRewrite $
                            DefinednessUnclear
                                rule
                                pat
                                rule.computedAttributes.notPreservesDefinednessReasons

                    -- apply substitution to rule requires constraints and simplify (one by one
                    -- in isolation). Stop if false, abort rewrite if indeterminate.
                    let ruleRequires =
                            concatMap (splitBoolPredicates . coerce . substituteInTerm subst . coerce) rule.requires
                        notAppliedIfBottom = RewriteRuleAppT $ pure NotApplied
                    -- filter out any predicates known to be _syntactically_ present in the known prior
                    let prior = pat.constraints
                        (knownTrue, toCheck) = partition (`Set.member` prior) ruleRequires
                    unless (null knownTrue) $
                        logMessage $
                            renderOneLineText $
                                "Known true side conditions (won't check):"
                                    <+> (hsep . punctuate comma . map (pretty' @mods) $ knownTrue)

                    unclearRequires <-
                        catMaybes <$> mapM (checkConstraint id notAppliedIfBottom prior) toCheck

                    -- check unclear requires-clauses in the context of known constraints (prior)
                    mbSolver <- lift $ RewriteT $ (.smtSolver) <$> ask

                    let smtUnclear = do
                            withContext CtxConstraint . withContext CtxAbort . logMessage $
                                WithJsonMessage (object ["conditions" .= (externaliseTerm . coerce <$> unclearRequires)]) $
                                    renderOneLineText $
                                        "Uncertain about condition(s) in a rule:"
                                            <+> (hsep . punctuate comma . map (pretty' @mods) $ unclearRequires)
                            failRewrite $
                                RuleConditionUnclear rule . coerce . foldl1 AndTerm $
                                    map coerce unclearRequires
                    case mbSolver of
                        Just solver -> do
                            checkAllRequires <-
                                SMT.checkPredicates solver prior mempty (Set.fromList unclearRequires)

                            case checkAllRequires of
                                Left SMT.SMTSolverUnknown{} ->
                                    smtUnclear -- abort rewrite if a solver result was Unknown
                                Left other ->
                                    liftIO $ Exception.throw other -- fail hard on other SMT errors
                                Right (Just False) -> do
                                    -- requires is actually false given the prior
                                    withContext CtxFailure $ logMessage ("Required clauses evaluated to #Bottom." :: Text)
                                    RewriteRuleAppT $ pure NotApplied
                                Right (Just True) ->
                                    pure () -- can proceed
                                Right Nothing ->
                                    smtUnclear -- no implication could be determined
                        Nothing ->
                            unless (null unclearRequires) $ do
                                withContext CtxConstraint . withContext CtxAbort $
                                    logMessage $
                                        WithJsonMessage (object ["conditions" .= (externaliseTerm . coerce <$> unclearRequires)]) $
                                            renderOneLineText $
                                                "Uncertain about a condition(s) in rule, no SMT solver:"
                                                    <+> (hsep . punctuate comma . map (pretty' @mods) $ unclearRequires)
                                failRewrite $
                                    RuleConditionUnclear rule (head unclearRequires)

                    -- check ensures constraints (new) from rhs: stop and return `Trivial` if
                    -- any are false, remove all that are trivially true, return the rest
                    let ruleEnsures =
                            concatMap (splitBoolPredicates . coerce . substituteInTerm subst . coerce) $
                                Set.toList rule.ensures
                        trivialIfBottom = RewriteRuleAppT $ pure Trivial
                    newConstraints <-
                        catMaybes <$> mapM (checkConstraint id trivialIfBottom prior) ruleEnsures

                    -- check all new constraints together with the known side constraints
                    whenJust mbSolver $ \solver ->
                        (lift $ SMT.checkPredicates solver prior mempty (Set.fromList newConstraints)) >>= \case
                            Right (Just False) -> do
                                withContext CtxSuccess $ logMessage ("New constraints evaluated to #Bottom." :: Text)
                                RewriteRuleAppT $ pure Trivial
                            Right _other ->
                                pure ()
                            Left SMT.SMTSolverUnknown{} ->
                                pure ()
                            Left other ->
                                liftIO $ Exception.throw other

                    -- existential variables may be present in rule.rhs and rule.ensures,
                    -- need to strip prefixes and freshen their names with respect to variables already
                    -- present in the input pattern and in the unification substitution
                    let varsFromInput = freeVariables pat.term <> (Set.unions $ Set.map (freeVariables . coerce) pat.constraints)
                        varsFromSubst = Set.unions . map freeVariables . Map.elems $ subst
                        forbiddenVars = varsFromInput <> varsFromSubst
                        existentialSubst =
                            Map.fromSet
                                (\v -> Var $ freshenVar v{variableName = stripMarker v.variableName} forbiddenVars)
                                rule.existentials

                    -- modify the substitution to include the existentials
                    let substWithExistentials = subst `Map.union` existentialSubst

                    let rewritten =
                            Pattern
                                (substituteInTerm substWithExistentials rule.rhs)
                                -- adding new constraints that have not been trivially `Top`, substituting the Ex# variables
                                ( pat.constraints
                                    <> (Set.fromList $ map (coerce . substituteInTerm existentialSubst . coerce) newConstraints)
                                )
                                ceilConditions
                    withContext CtxSuccess $
                        withPatternContext rewritten $
                            return (rule, rewritten)
  where
    failRewrite :: RewriteFailed "Rewrite" -> RewriteRuleAppT (RewriteT io) a
    failRewrite = lift . (throw)

    checkConstraint ::
        (Predicate -> a) ->
        RewriteRuleAppT (RewriteT io) (Maybe a) ->
        Set.Set Predicate ->
        Predicate ->
        RewriteRuleAppT (RewriteT io) (Maybe a)
    checkConstraint onUnclear onBottom knownPredicates p = do
        RewriteConfig{definition, llvmApi, smtSolver} <- lift $ RewriteT ask
        oldCache <- lift . RewriteT . lift $ get
        (simplified, cache) <-
            withContext CtxConstraint $
                simplifyConstraint definition llvmApi smtSolver oldCache knownPredicates p
        -- update cache
        lift . RewriteT . lift . modify $ const cache
        case simplified of
            Right (Predicate FalseBool) -> onBottom
            Right (Predicate TrueBool) -> pure Nothing
            Right other -> pure $ Just $ onUnclear other
            Left UndefinedTerm{} -> onBottom
            Left _ -> pure $ Just $ onUnclear p

{- | Reason why a rewrite did not produce a result. Contains additional
   information for logging what happened during the rewrite.
-}
data RewriteFailed k
    = -- | All rules have been tried unsuccessfully (rewrite is stuck)
      NoApplicableRules Pattern
    | -- | It is uncertain whether or not a rule LHS unifies with the term
      RuleApplicationUnclear (RewriteRule k) Term (NonEmpty (Term, Term))
    | -- | A rule condition is indeterminate
      RuleConditionUnclear (RewriteRule k) Predicate
    | -- | A rewrite rule does not preserve definedness
      DefinednessUnclear (RewriteRule k) Pattern [NotPreservesDefinednessReason]
    | -- | A sort error was detected during m,atching
      RewriteSortError (RewriteRule k) Term SortError
    | -- | An error was detected during matching
      InternalMatchError Text
    | -- | Term has index 'None', no rule should apply
      TermIndexIsNone Term
    deriving stock (Eq, Show)

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods (RewriteFailed k)) where
    pretty (PrettyWithModifiers f) = case f of
        NoApplicableRules pat ->
            "No rules applicable for the pattern " <> pretty' @mods pat
        RuleApplicationUnclear rule term remainder ->
            hsep
                [ "Uncertain about unification of rule"
                , ruleLabelOrLoc rule
                , " with term "
                , pretty' @mods term
                , "Remainder:"
                , ( hsep $
                        punctuate comma $
                            map (\(t1, t2) -> pretty' @mods t1 <+> "==" <+> pretty' @mods t2) $
                                NE.toList remainder
                  )
                ]
        RuleConditionUnclear rule predicate ->
            hsep
                [ "Uncertain about a condition in rule"
                , ruleLabelOrLoc rule
                , ": "
                , pretty' @mods predicate
                ]
        DefinednessUnclear rule _pat reasons ->
            hsep $
                [ "Uncertain about definedness of rule "
                , ruleLabelOrLoc rule
                , "because of:"
                ]
                    ++ map pretty reasons
        RewriteSortError rule term sortError ->
            hsep
                [ "Sort error while unifying"
                , pretty' @mods term
                , "with rule"
                , ruleLabelOrLoc rule
                , ":"
                , pretty $ show sortError
                ]
        TermIndexIsNone term ->
            "Term index is None for term " <> pretty' @mods term
        InternalMatchError err -> "An internal error occured" <> pretty err

ruleLabelOrLoc :: RewriteRule k -> Doc a
ruleLabelOrLoc rule =
    fromMaybe "unknown rule" $
        fmap pretty rule.attributes.ruleLabel <|> fmap pretty rule.attributes.location

-- | Different rewrite results (returned from RPC execute endpoint)
data RewriteResult pat
    = -- | branch point
      RewriteBranch pat (NonEmpty (Text, UniqueId, pat))
    | -- | no rules could be applied, config is stuck
      RewriteStuck pat
    | -- | cut point rule, return current (lhs) and single next state
      RewriteCutPoint Text UniqueId pat pat
    | -- | terminal rule, return rhs (final state reached)
      RewriteTerminal Text UniqueId pat
    | -- | stopping because maximum depth has been reached (label and unique id may be empty if no steps were taken)
      RewriteFinished (Maybe Text) (Maybe UniqueId) pat
    | -- | unable to handle the current case with this rewriter
      -- (signalled by exceptions)
      RewriteAborted (RewriteFailed "Rewrite") pat
    | -- | All applicable rules returned a pattern with a False
      -- ensures clause
      RewriteTrivial pat
    deriving stock (Eq, Show)
    deriving (Functor, Foldable, Traversable)

data RewriteTrace pat
    = -- | single step of execution
      RewriteSingleStep Text UniqueId pat pat
    | -- | branching step of execution
      RewriteBranchingStep pat (NonEmpty (Text, UniqueId))
    | -- | attempted rewrite failed
      RewriteStepFailed (RewriteFailed "Rewrite")
    | -- | Applied simplification to the pattern
      RewriteSimplified (Maybe EquationFailure)

{- | For the given rewrite trace, construct a new one,
     removing the heavy-weight information (the states),
     but keeping the meta-data (rule labels).
-}
eraseStates :: RewriteTrace Pattern -> RewriteTrace ()
eraseStates = \case
    RewriteSingleStep rule_label mUniqueId _preState _postState -> RewriteSingleStep rule_label mUniqueId () ()
    RewriteBranchingStep _state branchMetadata -> RewriteBranchingStep () branchMetadata
    RewriteStepFailed failureInfo -> RewriteStepFailed failureInfo
    RewriteSimplified mbEquationFailure -> RewriteSimplified mbEquationFailure

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods (RewriteTrace Pattern)) where
    pretty (PrettyWithModifiers t) = case t of
        RewriteSingleStep lbl _uniqueId pat rewritten ->
            let
                (l, r) = diff pat rewritten
             in
                hang 4 . vsep $
                    [ "Rewriting configuration"
                    , pretty' @mods l.term
                    , "to"
                    , pretty' @mods r.term
                    , "Using rule:"
                    , pretty lbl
                    ]
        RewriteBranchingStep pat branches ->
            hang 4 . vsep $
                [ "Configuration"
                , pretty' @mods (term pat)
                , "branches on rules:"
                , hang 2 $ vsep [pretty lbl | (lbl, _) <- toList branches]
                ]
        RewriteSimplified{} -> "Applied simplification"
        RewriteStepFailed failure -> pretty' @mods failure

diff :: Pattern -> Pattern -> (Pattern, Pattern)
diff p1 p2 =
    let (t1, t2) = mkDiffTerms (p1.term, p2.term)
     in -- TODO print differences in predicates
        (p1{term = t1}, p2{term = t2})

mkDiffTerms :: (Term, Term) -> (Term, Term)
mkDiffTerms = \case
    (t1@(SymbolApplication s1 ss1 xs), t2@(SymbolApplication s2 ss2 ys)) ->
        if Hashable.hash t1 == Hashable.hash t2
            then (DotDotDot, DotDotDot)
            else
                let (xs', ys') =
                        unzip
                            $ foldr
                                ( \xy rest -> case mkDiffTerms xy of
                                    (DotDotDot, _) -> (DotDotDot, DotDotDot) : dropWhile (\(l, _) -> l == DotDotDot) rest
                                    r -> r : rest
                                )
                                []
                            $ zip xs ys
                 in (SymbolApplication s1 ss1 xs', SymbolApplication s2 ss2 ys')
    r -> r

{- | Interface for RPC execute: Rewrite given term as long as there is
   exactly one result in each step.

  * multiple results: a branch point, return current and all results
  * RewriteTrivial: config simplified to #Bottom, return current
  * RewriteCutPoint: a cut-point rule was applied, return lhs and rhs
  * RewriteTerminal: a terminal rule was applied, return rhs
  * RewriteStuck: config could not be re-written by any rule, return current
  * RewriteFailed: rewriter cannot handle the case, return current

  The actions are logged at the custom log level '"Rewrite"'.


    This flow chart should represent the actions of this function:


                                Receive pattern P (P /= _|_)

                                             |
                                             |   +--------------------------------------------------------------------------------------------------+
+----------------------------------------+   |   |                                                                                                  |
|                                        v   v   v                                                                                                  |
|                                                                                                                                                   |
|         +----------------------------  Apply rule  <-------------------------------------------------------------------------------------------+  |
|         |                                                                                                                                      |  |
|         |                                    |                                                                                                 |  |
|         |                                    +-------------+                                                                                   |  |
|         v                                                  v                                                                                   |  |
|                                                                                                                                                |  |
|  Rewrite aborted            +--------------------  Rewrite finished  -------------------------+                                                |  |
|                             |                                                                 |                                                |  |
|         |                   |                               |                                 |                                                |  |
|         |                   |                               |                                 |                                                |  |
|         v                   v                               v                                 v                                                |  |
|                                                                                                                                                |  |
|  Return aborted       No rules apply                 Rewrite to P'  ---+                  Rewrite to PS -----------------+-------+             |  |
|                                                                        |                                                 |       |             |  |
|                             |                           |              |                    |      |                     |       |             |  |
|                             |                           |              |                    |      +----------+          |       |             |  |
|                             |                           v              v                    v                 v          |       v             |  |
|                             |                                                                                            |                     |  |
|                             |                         P' == _|_    P' /= _|_           /\ PS == _|_      PS simplify to  |   PS simplify to  --+  |
|                             |                                                                                   []       |      single P'         |
|              +--------------+-------------+               |           | |                   |                            |                        |
|              |              |             |               |           | |                   |                   |        |                        |
|              v              v             v               |           | |                   |                   |        +-------+                |
|                                                           |           | |                   |                   |                v                |
|          Does not     Simplified      Simplifies          |  +--------+-+-------------------+                   |                                 |
|          simplify      already                            |  |        | |                                       |          PS simplify to         |
|                                                           |  |        | |                                       |                PS'              |
|              |              |             |               |  |        | |                                       |                                 |
|              |              |             |               v  v        | |                                       |                 |               |
|              |              |             |                           | +-----------------+                     |                 v               |
|              |              |             |        Return vacuous P   |                   |                     |                                 |
|              |              |             |                           |                   |                     |         Return branching        |
|              |              |             |                           |                   |                     |                                 |
|              +-------+      |             |                           v                   v                     |                                 |
|                      v      v             |                                                                     |                                 |
|                                           |                    Depth/rule bound       Unbounded  ---------------+---------------------------------+
|                     Return stuck P        |                                                                     |
|                                           |                           |                                         |
|                            ^              |                           |                                         |
|                            |              |                           |                                         |
+----------------------------+--------------+                           v                                         |
                             |                                                                                    |
                             |                                    Return simplified P'                            |
                             |                                                                                    |
                             |                                                                                    |
                             +------------------------------------------------------------------------------------+
-}
performRewrite ::
    forall io.
    LoggerMIO io =>
    Flag "CollectRewriteTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    -- | maximum depth
    Maybe Natural ->
    -- | cut point rule labels
    [Text] ->
    -- | terminal rule labels
    [Text] ->
    Pattern ->
    io (Natural, Seq (RewriteTrace ()), RewriteResult Pattern)
performRewrite doTracing def mLlvmLibrary mSolver mbMaxDepth cutLabels terminalLabels pat = do
    (rr, RewriteStepsState{counter, traces}) <-
        flip runStateT rewriteStart $ doSteps False pat
    pure (counter, traces, rr)
  where
    logDepth = withContext CtxDepth . logMessage

    depthReached n = maybe False (n >=) mbMaxDepth

    showCounter = (<> " steps.") . pack . show

    emitRewriteTrace :: RewriteTrace Pattern -> StateT RewriteStepsState io ()
    emitRewriteTrace t = do
        when (coerce doTracing) $
            modify $
                \rss@RewriteStepsState{traces} -> rss{traces = traces |> eraseStates t}
    incrementCounter =
        modify $ \rss@RewriteStepsState{counter} -> rss{counter = counter + 1}

    updateCache simplifierCache = modify $ \rss -> rss{simplifierCache}

    simplifyP :: Pattern -> StateT RewriteStepsState io (Maybe Pattern)
    simplifyP p = withContext CtxSimplify $ do
        st <- get
        let cache = st.simplifierCache
            smt = st.smtSolver
        evaluatePattern def mLlvmLibrary smt cache p >>= \(res, newCache) -> do
            updateCache newCache
            case res of
                Right newPattern -> do
                    emitRewriteTrace $ RewriteSimplified Nothing
                    pure $ Just newPattern
                Left r@SideConditionFalse{} -> do
                    emitRewriteTrace $ RewriteSimplified (Just r)
                    pure Nothing
                Left r@UndefinedTerm{} -> do
                    emitRewriteTrace $ RewriteSimplified (Just r)
                    pure Nothing
                Left other -> do
                    emitRewriteTrace $ RewriteSimplified (Just other)
                    pure $ Just p

    -- Results may change when simplification prunes a false side
    -- condition, otherwise this would mainly be fmap simplifyP
    simplifyResult ::
        Pattern ->
        RewriteResult Pattern ->
        StateT RewriteStepsState io (RewriteResult Pattern)
    simplifyResult orig = \case
        RewriteBranch p nexts -> do
            simplifyP p >>= \case
                Nothing -> pure $ RewriteTrivial orig
                Just p' -> do
                    let simplifyP3rd (a, b, c) =
                            fmap (a,b,) <$> simplifyP c
                    nexts' <- catMaybes <$> mapM simplifyP3rd (toList nexts)
                    pure $ case nexts' of
                        -- The `[]` case should be `Stuck` not `Trivial`, because `RewriteTrivial p'`
                        -- means the pattern `p'` is bottom, but we know that is not the case here.
                        [] -> RewriteStuck p'
                        [(lbl, uId, n)] -> RewriteFinished (Just lbl) (Just uId) n
                        ns -> RewriteBranch p' $ NE.fromList ns
        r@RewriteStuck{} -> pure r
        r@RewriteTrivial{} -> pure r
        RewriteCutPoint lbl uId p next -> do
            simplifyP p >>= \case
                Nothing -> pure $ RewriteTrivial orig
                Just p' -> do
                    next' <- simplifyP next
                    pure $ case next' of
                        Nothing -> RewriteTrivial next
                        Just n -> RewriteCutPoint lbl uId p' n
        RewriteTerminal lbl uId p ->
            maybe (RewriteTrivial orig) (RewriteTerminal lbl uId) <$> simplifyP p
        RewriteFinished lbl uId p ->
            maybe (RewriteTrivial orig) (RewriteFinished lbl uId) <$> simplifyP p
        RewriteAborted reason p ->
            maybe (RewriteTrivial orig) (RewriteAborted reason) <$> simplifyP p

    doSteps ::
        Bool -> Pattern -> StateT RewriteStepsState io (RewriteResult Pattern)
    doSteps wasSimplified pat' = do
        RewriteStepsState{counter, simplifierCache} <- get
        logDepth $ showCounter counter
        if depthReached counter
            then do
                logDepth $ "Reached maximum depth of " <> maybe "?" showCounter mbMaxDepth
                (if wasSimplified then pure else simplifyResult pat') $ RewriteFinished Nothing Nothing pat'
            else
                runRewriteT
                    doTracing
                    def
                    mLlvmLibrary
                    mSolver
                    simplifierCache
                    (withPatternContext pat' $ rewriteStep cutLabels terminalLabels pat')
                    >>= \case
                        Right (RewriteFinished mlbl mUniqueId single, cache) -> do
                            whenJust mlbl $ \lbl ->
                                whenJust mUniqueId $ \uniqueId ->
                                    emitRewriteTrace $ RewriteSingleStep lbl uniqueId pat' single
                            updateCache cache
                            incrementCounter
                            doSteps False single
                        Right (terminal@(RewriteTerminal lbl uniqueId single), _cache) -> withPatternContext pat' $ do
                            emitRewriteTrace $ RewriteSingleStep lbl uniqueId pat' single
                            incrementCounter
                            simplifyResult pat' terminal
                        Right (branching@RewriteBranch{}, cache) -> do
                            logMessage $ "Stopped due to branching after " <> showCounter counter
                            updateCache cache
                            simplified <- withPatternContext pat' $ simplifyResult pat' branching
                            case simplified of
                                RewriteStuck{} -> withPatternContext pat' $ do
                                    logMessage ("Rewrite stuck after pruning branches" :: Text)
                                    pure simplified
                                RewriteTrivial{} -> withPatternContext pat' $ do
                                    logMessage $ "Simplified to bottom after " <> showCounter counter
                                    pure simplified
                                RewriteFinished mlbl mUniqueId single -> do
                                    logMessage ("All but one branch pruned, continuing" :: Text)
                                    whenJust mlbl $ \lbl ->
                                        whenJust mUniqueId $ \uniqueId ->
                                            emitRewriteTrace $ RewriteSingleStep lbl uniqueId pat' single
                                    incrementCounter
                                    doSteps False single
                                RewriteBranch pat'' branches -> withPatternContext pat' $ do
                                    emitRewriteTrace $ RewriteBranchingStep pat'' $ fmap (\(lbl, uid, _) -> (lbl, uid)) branches
                                    pure simplified
                                _other -> withPatternContext pat' $ error "simplifyResult: Unexpected return value"
                        Right (cutPoint@(RewriteCutPoint lbl _ _ _), _) -> withPatternContext pat' $ do
                            simplified <- simplifyResult pat' cutPoint
                            case simplified of
                                RewriteCutPoint{} ->
                                    logMessage $ "Cut point " <> lbl <> " after " <> showCounter counter
                                RewriteStuck{} ->
                                    logMessage $ "Stuck after " <> showCounter counter
                                RewriteTrivial{} ->
                                    logMessage $ "Simplified to bottom after " <> showCounter counter
                                _other -> error "simplifyResult: Unexpected return value"
                            pure simplified
                        Right (stuck@RewriteStuck{}, cache) -> do
                            logMessage $ "Stopped after " <> showCounter counter
                            updateCache cache
                            emitRewriteTrace $ RewriteStepFailed $ NoApplicableRules pat'
                            if wasSimplified
                                then pure stuck
                                else withSimplified pat' "Retrying with simplified pattern" (doSteps True)
                        Right (trivial@RewriteTrivial{}, _) -> withPatternContext pat' $ do
                            logMessage $ "Simplified to bottom after " <> showCounter counter
                            pure trivial
                        Right (aborted@RewriteAborted{}, _) -> withPatternContext pat' $ do
                            logMessage $ "Aborted after " <> showCounter counter
                            simplifyResult pat' aborted
                        -- if unification was unclear and the pattern was
                        -- unsimplified, simplify and retry rewriting once
                        Left failure@(RuleApplicationUnclear rule _ remainder)
                            | not wasSimplified -> do
                                emitRewriteTrace $ RewriteStepFailed failure
                                -- simplify remainders, substitute and rerun.
                                -- If failed, do the pattern-wide simplfication and rerun again
                                withSimplified pat' "Retrying with simplified pattern" (doSteps True)
                            | otherwise -> do
                                -- was already simplified, emit an abort log entry
                                withRuleContext rule . withContext CtxMatch . withContext CtxAbort $
                                    getPrettyModifiers >>= \case
                                        ModifiersRep (_ :: FromModifiersT mods => Proxy mods) ->
                                            logMessage $
                                                WithJsonMessage (object ["remainder" .= (bimap externaliseTerm externaliseTerm <$> remainder)]) $
                                                    renderOneLineText $
                                                        "Uncertain about match with rule. Remainder:"
                                                            <+> ( hsep $
                                                                    punctuate comma $
                                                                        map (\(t1, t2) -> pretty' @mods t1 <+> "==" <+> pretty' @mods t2) $
                                                                            NE.toList remainder
                                                                )
                                emitRewriteTrace $ RewriteStepFailed failure
                                logMessage $ "Aborted after " <> showCounter counter
                                pure (RewriteAborted failure pat')
                        Left failure -> do
                            emitRewriteTrace $ RewriteStepFailed failure
                            let msg = "Aborted after " <> showCounter counter
                            if wasSimplified
                                then logMessage msg >> pure (RewriteAborted failure pat')
                                else withSimplified pat' msg (pure . RewriteAborted failure)
      where
        withSimplified p msg cont = do
            (withPatternContext p $ simplifyP p) >>= \case
                Nothing -> do
                    logMessage ("Rewrite stuck after simplification." :: Text)
                    pure $ RewriteStuck p
                Just simplifiedPat -> do
                    logMessage msg
                    cont simplifiedPat

data RewriteStepsState = RewriteStepsState
    { counter :: !Natural
    , traces :: !(Seq (RewriteTrace ()))
    , simplifierCache :: SimplifierCache
    , smtSolver :: Maybe SMT.SMTContext
    }

rewriteStart :: RewriteStepsState
rewriteStart =
    RewriteStepsState
        { counter = 0
        , traces = mempty
        , simplifierCache = mempty
        , smtSolver = Nothing
        }
