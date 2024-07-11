{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Simplify.Simplify (
    Env (..),
    Simplifier,
    runSimplifier,
    runSimplifierBranch,
    InternalVariable,
    MonadSimplify (..),
    askMetadataTools,
    simplifyPattern,
    simplifyTerm,
    askAxiomEquations,
    askInjSimplifier,
    askOverloadSimplifier,
    getCache,
    putCache,
    askHookedSymbols,
    mkHookedSymbols,
    simplifyPatternScatter,
    TermSimplifier,

    -- * Condition simplifiers
    ConditionSimplifier (..),
    emptyConditionSimplifier,

    -- * Builtin and axiom simplifiers
    SimplifierCache (attemptedEquationsCache),
    EvaluationAttempt (..),
    initCache,
    updateCache,
    lookupCache,
    BuiltinAndAxiomSimplifier (..),
    AttemptedAxiom (..),
    isApplicable,
    isNotApplicable,
    isNotApplicableUntilConditionChanges,
    AttemptedAxiomResults (..),
    CommonAttemptedAxiom,
    emptyAttemptedAxiom,
    hasRemainders,
    maybeNotApplicable,
    exceptNotApplicable,
    applicationAxiomSimplifier,
    notApplicableAxiomEvaluator,
    purePatternAxiomEvaluator,
    isConstructorOrOverloaded,
    applyFirstSimplifierThatWorks,
    firstFullEvaluation,

    -- * Term and predicate simplifiers
    makeEvaluateTermCeil,
    makeEvaluateCeil,

    -- * Re-exports
    MonadSMT,
    MonadLog,
    runSimplifierLogged,
    SimplifierTrace (..),
) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Counter
import Control.Monad.Morph (MFunctor)
import Control.Monad.Morph qualified as Monad.Morph
import Control.Monad.RWS.Strict (RWST)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Functor.Foldable qualified as Recursive
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom (UniqueId)
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Debug
import Kore.Equation.DebugEquation (AttemptEquationError)
import Kore.Equation.Equation (Equation)
import Kore.IndexedModule.IndexedModule (VerifiedModuleSyntax)
import Kore.IndexedModule.IndexedModule qualified as IndexedModule
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.Conditional (Conditional)
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.OrCondition (OrCondition)
import Kore.Internal.OrCondition qualified as OrCondition
import Kore.Internal.OrPattern (OrPattern)
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (Pattern)
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition (SideCondition, toRepresentation)
import Kore.Internal.SideCondition.SideCondition qualified as SideCondition (
    Representation,
 )
import Kore.Internal.Symbol
import Kore.Internal.TermLike (
    Sort,
    TermAttributes,
    TermLike,
    TermLikeF (..),
 )
import Kore.Internal.Variable (InternalVariable)
import Kore.Rewrite.Axiom.Identifier (AxiomIdentifier (..))
import Kore.Rewrite.Function.Memo qualified as Memo
import Kore.Rewrite.RewritingVariable (RewritingVariableName)
import Kore.Simplify.InjSimplifier (InjSimplifier)
import Kore.Simplify.OverloadSimplifier (OverloadSimplifier (..))
import Kore.Syntax (Id)
import Kore.Syntax.Application
import Kore.Unparser
import Log hiding (UniqueId)
import Logic
import Prelude.Kore
import Pretty qualified
import Prof (
    MonadProf,
 )
import SMT (
    MonadSMT (..),
    SMT,
 )

-- * Simplifier

data Env = Env
    { metadataTools :: !(SmtMetadataTools Attribute.Symbol)
    , simplifierCondition :: !(ConditionSimplifier Simplifier)
    , simplifierPattern ::
        SideCondition RewritingVariableName ->
        Pattern RewritingVariableName ->
        Simplifier (OrPattern RewritingVariableName)
    , simplifierTerm ::
        SideCondition RewritingVariableName ->
        TermLike RewritingVariableName ->
        Simplifier (OrPattern RewritingVariableName)
    , axiomEquations :: !(Map AxiomIdentifier [Equation RewritingVariableName])
    , memo :: !(Memo.Self Simplifier)
    , injSimplifier :: !InjSimplifier
    , overloadSimplifier :: !OverloadSimplifier
    , hookedSymbols :: !(Map Id Text)
    , tracingEnabled :: Bool
    }

data SimplifierTrace = SimplifierTrace
    { originalTerm :: TermLike RewritingVariableName
    , equationId :: UniqueId
    , rewrittenTerm :: Pattern RewritingVariableName
    }
    deriving stock (Eq, Show)

{- | @Simplifier@ represents a simplification action.

A @Simplifier@ can send constraints to the SMT solver through 'MonadSMT'.

A @Simplifier@ can write to the log through 'HasLog'.
-}
newtype Simplifier a
    = Simplifier (StateT (SimplifierCache, Seq SimplifierTrace) (ReaderT Env SMT) a)
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadSMT, MonadLog, MonadProf)
    deriving newtype (MonadIO, MonadCatch, MonadThrow, MonadMask)
    deriving newtype (MonadReader Env)
    deriving newtype (MonadState (SimplifierCache, Seq SimplifierTrace))
    deriving newtype (MonadBase IO, MonadBaseControl IO)

{- | Run a simplification, returning the result of only one branch.

__Warning__: @runSimplifier@ calls 'error' if the 'Simplifier' does not contain
exactly one branch. Use 'evalSimplifierBranch' to evaluation simplifications
that may branch.
-}
runSimplifier :: Env -> Simplifier a -> SMT a
runSimplifier env (Simplifier simplifier) =
    runReaderT (evalStateT simplifier (initCache, mempty)) env

runSimplifierLogged :: Env -> Simplifier a -> SMT (Seq SimplifierTrace, a)
runSimplifierLogged env (Simplifier simplifier) =
    runReaderT
        ( runStateT simplifier (initCache, mempty) >>= \(res, (_, logs)) ->
            pure (logs, res)
        )
        env

-- | Run a simplification, returning the results along all branches.
runSimplifierBranch ::
    Env ->
    -- | simplifier computation
    LogicT Simplifier a ->
    SMT [a]
runSimplifierBranch env = runSimplifier env . observeAllT

type TermSimplifier variable m =
    TermLike variable -> TermLike variable -> m (Pattern variable)

class (MonadIO m, MonadLog m, MonadSMT m) => MonadSimplify m where
    liftSimplifier :: Simplifier a -> m a
    default liftSimplifier ::
        (MonadTrans t, MonadSimplify n, m ~ t n) =>
        Simplifier a ->
        m a
    liftSimplifier = lift . liftSimplifier
    {-# INLINE liftSimplifier #-}

    simplifyCondition ::
        SideCondition RewritingVariableName ->
        Conditional RewritingVariableName term ->
        LogicT m (Conditional RewritingVariableName term)
    default simplifyCondition ::
        ( MonadTrans trans
        , MonadSimplify n
        , m ~ trans n
        ) =>
        SideCondition RewritingVariableName ->
        Conditional RewritingVariableName term ->
        LogicT m (Conditional RewritingVariableName term)
    simplifyCondition sideCondition conditional = do
        results <-
            lift . lift $
                observeAllT $
                    simplifyCondition sideCondition conditional
        scatter results
    {-# INLINE simplifyCondition #-}

    localAxiomEquations ::
        ( Map AxiomIdentifier [Equation RewritingVariableName] ->
          Map AxiomIdentifier [Equation RewritingVariableName]
        ) ->
        m a ->
        m a
    default localAxiomEquations ::
        (MFunctor t, MonadSimplify n, m ~ t n) =>
        ( Map AxiomIdentifier [Equation RewritingVariableName] ->
          Map AxiomIdentifier [Equation RewritingVariableName]
        ) ->
        m a ->
        m a
    localAxiomEquations locally =
        Monad.Morph.hoist (localAxiomEquations locally)
    {-# INLINE localAxiomEquations #-}

    askMemo :: m (Memo.Self m)
    default askMemo ::
        (MonadTrans t, MonadSimplify n, m ~ t n) =>
        m (Memo.Self m)
    askMemo = Memo.liftSelf lift <$> lift askMemo
    {-# INLINE askMemo #-}

-- | Retrieve the 'MetadataTools' for the Kore context.
askMetadataTools :: MonadSimplify m => m (SmtMetadataTools Attribute.Symbol)
askMetadataTools = liftSimplifier $ asks metadataTools

simplifyPattern ::
    SideCondition RewritingVariableName ->
    Pattern RewritingVariableName ->
    Simplifier (OrPattern RewritingVariableName)
simplifyPattern sideCondition patt =
    liftSimplifier $ do
        simplifier <- asks simplifierPattern
        simplifier sideCondition patt

simplifyTerm ::
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    Simplifier (OrPattern RewritingVariableName)
simplifyTerm sideCondition termLike =
    liftSimplifier $ do
        simplifier <- asks simplifierTerm
        simplifier sideCondition termLike

askAxiomEquations :: MonadSimplify m => m (Map AxiomIdentifier [Equation RewritingVariableName])
askAxiomEquations = liftSimplifier $ asks axiomEquations

-- | Retrieve the 'InjSimplifier' for the Kore context.
askInjSimplifier :: MonadSimplify m => m InjSimplifier
askInjSimplifier = liftSimplifier $ asks injSimplifier

-- | Retrieve the 'OverloadSimplifier' for the Kore context.
askOverloadSimplifier :: MonadSimplify m => m OverloadSimplifier
askOverloadSimplifier = liftSimplifier $ asks overloadSimplifier

getCache :: MonadSimplify m => m SimplifierCache
getCache = fst <$> liftSimplifier get

putCache :: MonadSimplify m => SimplifierCache -> m ()
putCache c = liftSimplifier $ modify $ \(_c, rules) -> (c, rules)

askHookedSymbols :: MonadSimplify m => m (Map Id Text)
askHookedSymbols = liftSimplifier $ asks hookedSymbols

mkHookedSymbols ::
    VerifiedModuleSyntax Attribute.StepperAttributes ->
    Map Id Text
mkHookedSymbols im =
    Map.union
        (Map.mapMaybe getHook $ IndexedModule.hookedObjectSymbolSentences im)
        ( Map.unions
            (importHookedSymbols <$> IndexedModule.indexedModuleImportsSyntax im)
        )
  where
    getHook :: (Attribute.Symbol, a) -> Maybe Text
    getHook (attrs, _) = Attribute.getHook $ Attribute.hook attrs

    importHookedSymbols ::
        (a, b, VerifiedModuleSyntax Attribute.StepperAttributes) ->
        Map Id Text
    importHookedSymbols (_, _, im') = mkHookedSymbols im'

instance MonadSimplify Simplifier where
    liftSimplifier = id
    {-# INLINE liftSimplifier #-}

    simplifyCondition topCondition conditional = do
        ConditionSimplifier simplify <- asks simplifierCondition
        simplify topCondition conditional
    {-# INLINE simplifyCondition #-}

    localAxiomEquations locally =
        local $ \env@Env{axiomEquations} ->
            env{axiomEquations = locally axiomEquations}
    {-# INLINE localAxiomEquations #-}

    askMemo = asks memo
    {-# INLINE askMemo #-}

instance
    (MonadSimplify m, Monoid w) =>
    MonadSimplify (AccumT w m)
    where
    localAxiomEquations locally =
        mapAccumT (localAxiomEquations locally)
    {-# INLINE localAxiomEquations #-}

instance MonadSimplify m => MonadSimplify (CounterT m)

instance MonadSimplify m => MonadSimplify (ExceptT e m)

instance MonadSimplify m => MonadSimplify (IdentityT m)

instance MonadSimplify m => MonadSimplify (LogicT m) where
    localAxiomEquations locally =
        mapLogicT (localAxiomEquations locally)
    {-# INLINE localAxiomEquations #-}

instance MonadSimplify m => MonadSimplify (MaybeT m)

instance MonadSimplify m => MonadSimplify (ReaderT r m)

instance MonadSimplify m => MonadSimplify (StateT s m)

instance MonadSimplify m => MonadSimplify (RWST r () s m)

-- * Term simplifiers

-- TODO (thomas.tuegel): Factor out these types.

simplifyPatternScatter ::
    SideCondition RewritingVariableName ->
    Pattern RewritingVariableName ->
    LogicT Simplifier (Pattern RewritingVariableName)
simplifyPatternScatter sideCondition patt =
    Logic.scatter
        =<< liftSimplifier (simplifyPattern sideCondition patt)
-- * Predicate simplifiers

{- | 'ConditionSimplifier' wraps a function that simplifies
'Predicate's. The minimal requirement from this function is
that it applies the substitution on the predicate.
-}
newtype ConditionSimplifier monad = ConditionSimplifier
    { getConditionSimplifier ::
        forall term.
        SideCondition RewritingVariableName ->
        Conditional RewritingVariableName term ->
        LogicT monad (Conditional RewritingVariableName term)
    }

emptyConditionSimplifier :: ConditionSimplifier monad
emptyConditionSimplifier =
    ConditionSimplifier (\_ predicate -> return predicate)

-- * Builtin and axiom simplifiers

{- | Used for keeping track of already attempted equations which failed to
 apply.
-}
newtype SimplifierCache = SimplifierCache
    { attemptedEquationsCache ::
        HashMap
            EvaluationAttempt
            (AttemptEquationError RewritingVariableName)
    }

{- | An evaluation attempt is determined by an equation-term pair, since the
 'AttemptEquationError' type contains any necessary information about the
 'SideCondition'.
-}
data EvaluationAttempt = EvaluationAttempt
    { cachedEquation :: Equation RewritingVariableName
    , cachedTerm :: TermLike RewritingVariableName
    }
    deriving stock (Eq, Ord)
    deriving stock (GHC.Generic)
    deriving anyclass (Hashable)

-- | Initialize with an empty cache.
initCache :: SimplifierCache
initCache = SimplifierCache HashMap.empty

-- | Update by inserting a new entry into the cache.
updateCache ::
    EvaluationAttempt ->
    AttemptEquationError RewritingVariableName ->
    SimplifierCache ->
    SimplifierCache
updateCache key value (SimplifierCache oldCache) =
    HashMap.insert key value oldCache
        & SimplifierCache

-- | Lookup an entry in the cache.
lookupCache ::
    EvaluationAttempt ->
    SimplifierCache ->
    Maybe (AttemptEquationError RewritingVariableName)
lookupCache key (SimplifierCache cache) =
    HashMap.lookup key cache

{- | 'BuiltinAndAxiomSimplifier' simplifies patterns using either an axiom
or builtin code.

Arguments:

* 'MetadataTools' are tools for finding additional information about
patterns such as their sorts, whether they are constructors or hooked.

* 'TermLikeSimplifier' is a Function for simplifying patterns, used for
the post-processing of the function application results.

* 'TermLike' is the pattern to be evaluated.

Return value:

It returns the result of simplifying the pattern with builtins and
axioms, together with a proof certifying that it was simplified correctly
(which is only a placeholder right now).
-}
newtype BuiltinAndAxiomSimplifier =
    -- TODO (thomas.tuegel): Rename me!
    BuiltinAndAxiomSimplifier
    { runBuiltinAndAxiomSimplifier ::
        TermLike RewritingVariableName ->
        SideCondition RewritingVariableName ->
        Simplifier (AttemptedAxiom RewritingVariableName)
    }

{- | Creates an evaluator that choses the result of the first evaluator that
returns Applicable.

If that result contains more than one pattern, or it contains a reminder,
the evaluation fails with 'error' (may change in the future).
-}
firstFullEvaluation ::
    [BuiltinAndAxiomSimplifier] ->
    BuiltinAndAxiomSimplifier
firstFullEvaluation simplifiers =
    BuiltinAndAxiomSimplifier
        (applyFirstSimplifierThatWorks simplifiers)

{- | Whether a term cannot be simplified regardless of the side condition,
or only with the current side condition.

Example usage for @applyFirstSimplifierThatWorksWorker@:

We start assuming that if we can't simplify the current term, we will never
be able to simplify it.

If we manage to apply one of the evaluators with an acceptable result
(e.g. without remainders), we just return the result and we ignore the
value of the @NonSimplifiability@ argument.

If the result is not acceptable, we continue trying other evaluators, but we
assume that, even if we are not able to simplify the term right now, that may
change when the current side condition changes (i.e. we send @Conditional@
as an argument to the next @applyFirstSimplifierThatWorksWorker@ call).

If we finished trying all the evaluators without an acceptable result,
we mark the term as simplified according to the 'NonSimplifiability' argument,
either as "always simplified", or as "simplified while the current side
condition is unchanged".
-}
data NonSimplifiability
    = Always
    | Conditional

applyFirstSimplifierThatWorks ::
    [BuiltinAndAxiomSimplifier] ->
    TermLike RewritingVariableName ->
    SideCondition RewritingVariableName ->
    Simplifier (AttemptedAxiom RewritingVariableName)
applyFirstSimplifierThatWorks evaluators =
    applyFirstSimplifierThatWorksWorker evaluators Always

applyFirstSimplifierThatWorksWorker ::
    [BuiltinAndAxiomSimplifier] ->
    NonSimplifiability ->
    TermLike RewritingVariableName ->
    SideCondition RewritingVariableName ->
    Simplifier (AttemptedAxiom RewritingVariableName)
applyFirstSimplifierThatWorksWorker [] Always _ _ =
    return NotApplicable
applyFirstSimplifierThatWorksWorker [] Conditional _ sideCondition =
    return $
        NotApplicableUntilConditionChanges $
            toRepresentation sideCondition
applyFirstSimplifierThatWorksWorker
    (BuiltinAndAxiomSimplifier evaluator : evaluators)
    nonSimplifiability
    patt
    sideCondition =
        do
            applicationResult <- evaluator patt sideCondition

            case applicationResult of
                Applied
                    AttemptedAxiomResults
                        { results = orResults
                        , remainders = orRemainders
                        }
                        | length orResults > 1 ->
                            -- We should only allow multiple simplification results
                            -- when they are created by unification splitting the
                            -- configuration.
                            -- However, right now, we shouldn't be able to get more
                            -- than one result, so we throw an error.
                            error . show . Pretty.vsep $
                                [ "Unexpected simplification result with more \
                                  \than one configuration:"
                                , Pretty.indent 2 "input:"
                                , Pretty.indent 4 (unparse patt)
                                , Pretty.indent 2 "results:"
                                , (Pretty.indent 4 . Pretty.vsep)
                                    (unparse <$> toList orResults)
                                , Pretty.indent 2 "remainders:"
                                , (Pretty.indent 4 . Pretty.vsep)
                                    (unparse <$> toList orRemainders)
                                ]
                        | not (OrPattern.isFalse orRemainders) ->
                            tryNextSimplifier Conditional
                        | otherwise -> return applicationResult
                NotApplicable -> tryNextSimplifier nonSimplifiability
                NotApplicableUntilConditionChanges _ ->
                    tryNextSimplifier Conditional
      where
        tryNextSimplifier nonSimplifiability' =
            applyFirstSimplifierThatWorksWorker
                evaluators
                nonSimplifiability'
                patt
                sideCondition

-- | A type holding the result of applying an axiom to a pattern.
data AttemptedAxiomResults variable = AttemptedAxiomResults
    { results :: !(OrPattern variable)
    -- ^ The result of applying the axiom
    , remainders :: !(OrPattern variable)
    -- ^ The part of the pattern that was not rewritten by the axiom.
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Semigroup (AttemptedAxiomResults RewritingVariableName) where
    (<>)
        AttemptedAxiomResults
            { results = firstResults
            , remainders = firstRemainders
            }
        AttemptedAxiomResults
            { results = secondResults
            , remainders = secondRemainders
            } =
            AttemptedAxiomResults
                { results = MultiOr.merge firstResults secondResults
                , remainders = MultiOr.merge firstRemainders secondRemainders
                }

instance Monoid (AttemptedAxiomResults RewritingVariableName) where
    mempty =
        AttemptedAxiomResults
            { results = OrPattern.bottom
            , remainders = OrPattern.bottom
            }

{- | 'AttemptedAxiom' holds the result of axiom-based simplification, with
a case for axioms that can't be applied.

If an axiom does not match, or the requires clause is not satisfiable, then
the result is NotApplicable.

Otherwise, if the requires clause is satisfiable, but it's not implied by the
side condition, then, for simplification axioms, the result is
NotApplicableUntilConditionChanges.

Otherwise, the result is Applied.
-}
data AttemptedAxiom variable
    = NotApplicable
    | -- | The axiom(s) can't be applied with the given side condition, but
      -- we may be able to apply them when the side condition changes.
      NotApplicableUntilConditionChanges !SideCondition.Representation
    | Applied !(AttemptedAxiomResults variable)
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

isApplicable
    , isNotApplicable
    , isNotApplicableUntilConditionChanges ::
        AttemptedAxiom RewritingVariableName -> Bool
isApplicable =
    \case
        Applied _ -> True
        _ -> False
isNotApplicable =
    \case
        NotApplicable -> True
        _ -> False
isNotApplicableUntilConditionChanges =
    \case
        NotApplicableUntilConditionChanges _ -> True
        _ -> False

{- | 'CommonAttemptedAxiom' particularizes 'AttemptedAxiom' to 'Variable',
following the same pattern as the other `Common*` types.
-}
type CommonAttemptedAxiom = AttemptedAxiom RewritingVariableName

emptyAttemptedAxiom :: AttemptedAxiom RewritingVariableName
emptyAttemptedAxiom = Applied mempty

{- | Does the 'AttemptedAxiom' have remainders?

A 'NotApplicable' result is not considered to have remainders.
-}
hasRemainders :: AttemptedAxiom RewritingVariableName -> Bool
hasRemainders (Applied axiomResults) = (not . null) (remainders axiomResults)
hasRemainders NotApplicable = False
hasRemainders (NotApplicableUntilConditionChanges _) = False

-- | Return a 'NotApplicable' result for a failing 'MaybeT' action.
maybeNotApplicable ::
    Functor m =>
    MaybeT m (AttemptedAxiomResults RewritingVariableName) ->
    m (AttemptedAxiom RewritingVariableName)
maybeNotApplicable =
    fmap (maybe NotApplicable Applied) . runMaybeT

-- | Return a 'NotApplicable' result for a failing 'ExceptT' action.
exceptNotApplicable ::
    Functor m =>
    ExceptT e m (AttemptedAxiomResults RewritingVariableName) ->
    m (AttemptedAxiom RewritingVariableName)
exceptNotApplicable =
    fmap (either (const NotApplicable) Applied) . runExceptT

-- | Yields a pure 'Simplifier' which always returns 'NotApplicable'
notApplicableAxiomEvaluator ::
    Applicative m => m (AttemptedAxiom RewritingVariableName)
notApplicableAxiomEvaluator = pure NotApplicable

-- | Yields a pure 'Simplifier' which produces a given 'TermLike'
purePatternAxiomEvaluator ::
    Applicative m =>
    TermLike RewritingVariableName ->
    m (AttemptedAxiom RewritingVariableName)
purePatternAxiomEvaluator p =
    pure
        ( Applied
            AttemptedAxiomResults
                { results = OrPattern.fromTermLike p
                , remainders = OrPattern.fromPatterns []
                }
        )

{- | Creates an 'BuiltinAndAxiomSimplifier' from a similar function that takes an
'Application'.
-}
applicationAxiomSimplifier ::
    ( SideCondition RewritingVariableName ->
      CofreeF
        (Application Symbol)
        (TermAttributes RewritingVariableName)
        (TermLike RewritingVariableName) ->
      Simplifier (AttemptedAxiom RewritingVariableName)
    ) ->
    BuiltinAndAxiomSimplifier
applicationAxiomSimplifier applicationSimplifier =
    BuiltinAndAxiomSimplifier helper
  where
    helper ::
        TermLike RewritingVariableName ->
        SideCondition RewritingVariableName ->
        Simplifier (AttemptedAxiom RewritingVariableName)
    helper termLike sideCondition =
        case Recursive.project termLike of
            (valid :< ApplySymbolF p) ->
                applicationSimplifier sideCondition (valid :< p)
            _ ->
                error
                    ("Expected an application pattern, but got: " ++ show termLike)

-- | Checks whether a symbol is a constructor or is overloaded.
isConstructorOrOverloaded ::
    Symbol ->
    Simplifier Bool
isConstructorOrOverloaded s =
    do
        OverloadSimplifier{isOverloaded} <- askOverloadSimplifier
        return (isConstructor s || isOverloaded s)

makeEvaluateTermCeil ::
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    Simplifier (OrCondition RewritingVariableName)
makeEvaluateTermCeil sideCondition child =
    Predicate.makeCeilPredicate child
        & Condition.fromPredicate
        & simplifyCondition sideCondition
        & OrCondition.observeAllT

makeEvaluateCeil ::
    Sort ->
    SideCondition RewritingVariableName ->
    Pattern RewritingVariableName ->
    Simplifier (OrPattern RewritingVariableName)
makeEvaluateCeil sort sideCondition child =
    do
        let (childTerm, childCondition) = Pattern.splitTerm child
        ceilCondition <-
            Predicate.makeCeilPredicate childTerm
                & Condition.fromPredicate
                & simplifyCondition sideCondition
        Pattern.andCondition (Pattern.topOf sort) (ceilCondition <> childCondition)
            & pure
        & OrPattern.observeAllT
