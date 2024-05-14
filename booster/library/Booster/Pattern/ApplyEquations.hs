{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.ApplyEquations (
    evaluateTerm,
    evaluatePattern,
    Direction (..),
    EquationT (..),
    runEquationT,
    EquationConfig (..),
    getConfig,
    EquationPreference (..),
    EquationFailure (..),
    EquationTrace (..),
    pattern CollectEquationTraces,
    pattern NoCollectEquationTraces,
    eraseStates,
    EquationMetadata (..),
    ApplyEquationResult (..),
    ApplyEquationFailure (..),
    applyEquations,
    handleSimplificationEquation,
    isMatchFailure,
    isSuccess,
    simplifyConstraint,
    simplifyConstraints,
    SimplifierCache,
    evaluateConstraints,
) where

import Control.Applicative (Alternative (..))
import Control.Monad
import Control.Monad.Extra (fromMaybeM, whenJust)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger.CallStack (
    LogLevel (..),
    MonadLogger,
    MonadLoggerIO,
    logOther,
    logOtherNS,
    logWarnNS,
 )
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (..), ask, asks, withReaderT)
import Control.Monad.Trans.State
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable (toList, traverse_)
import Data.List (intersperse, partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Sequence (Seq (..), pattern (:<|))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text (toStrict)
import GHC.TypeLits (KnownSymbol)
import Prettyprinter

import Booster.Builtin as Builtin
import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.GlobalState qualified as GlobalState
import Booster.LLVM (LlvmError (..))
import Booster.LLVM qualified as LLVM
import Booster.Log
import Booster.Pattern.Base
import Booster.Pattern.Bool
import Booster.Pattern.Index
import Booster.Pattern.Match
import Booster.Pattern.Util
import Booster.Prettyprinter (renderDefault, renderOneLineText)
import Booster.SMT.Interface qualified as SMT
import Booster.Util (Bound (..), Flag (..))
import Kore.JsonRpc.Types.Log qualified as KoreRpcLog
import Kore.Util (showHashHex)

newtype EquationT io a
    = EquationT (ReaderT EquationConfig (ExceptT EquationFailure (StateT EquationState io)) a)
    -- ~ EquationConfig -> EquationState -> io (Either EquationFailure a, EquationState)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO)

instance MonadLoggerIO io => LoggerMIO (EquationT io) where
    getLogger = EquationT $ asks logger
    withLogger modL (EquationT m) = EquationT $ withReaderT (\cfg@EquationConfig{logger} -> cfg{logger = modL logger}) m

throw :: MonadLoggerIO io => EquationFailure -> EquationT io a
throw = EquationT . lift . throwE

catch_ ::
    MonadLoggerIO io => EquationT io a -> (EquationFailure -> EquationT io a) -> EquationT io a
catch_ (EquationT op) hdlr = EquationT $ do
    cfg <- ask
    lift (runReaderT op cfg `catchE` (\e -> let EquationT fallBack = hdlr e in runReaderT fallBack cfg))

data EquationFailure
    = IndexIsNone Term
    | TooManyIterations Int Term Term
    | EquationLoop [Term]
    | TooManyRecursions [Term]
    | SideConditionFalse Predicate
    | InternalError Text
    | UndefinedTerm Term LLVM.LlvmError
    deriving stock (Eq, Show, Data)

instance Pretty EquationFailure where
    pretty = \case
        IndexIsNone t ->
            "Index 'None' for term " <> pretty t
        TooManyIterations count start end ->
            vsep
                [ "Unable to finish evaluation in " <> pretty count <> " iterations"
                , "Started with: " <> pretty start
                , "Stopped at: " <> pretty end
                ]
        EquationLoop ts ->
            vsep $ "Evaluation produced a loop:" : map pretty ts
        TooManyRecursions ts ->
            vsep $
                "Recursion limit exceeded. The following terms were evaluated:"
                    : map pretty ts
        SideConditionFalse p ->
            vsep
                [ "A side condition was found to be false during evaluation (pruning)"
                , pretty p
                ]
        UndefinedTerm t (LLVM.LlvmError err) ->
            vsep
                [ "Term"
                , pretty t
                , "is undefined: "
                , pretty (BS.unpack err)
                ]
        InternalError msg ->
            "Internal error during evaluation: " <> pretty msg

data EquationConfig = EquationConfig
    { definition :: KoreDefinition
    , llvmApi :: Maybe LLVM.API
    , smtSolver :: Maybe SMT.SMTContext
    , doTracing :: Flag "CollectEquationTraces"
    , maxRecursion :: Bound "Recursion"
    , maxIterations :: Bound "Iterations"
    , logger :: Logger LogMessage
    }

pattern CollectEquationTraces :: Flag "CollectEquationTraces"
pattern CollectEquationTraces = Flag True

pattern NoCollectEquationTraces :: Flag "CollectEquationTraces"
pattern NoCollectEquationTraces = Flag False

data EquationState = EquationState
    { termStack :: Seq Term
    , recursionStack :: [Term]
    , changed :: Bool
    , predicates :: Set Predicate
    , cache :: SimplifierCache
    }

data SimplifierCache = SimplifierCache {llvm, equations :: Map Term Term}
    deriving stock (Show)

instance Semigroup SimplifierCache where
    cache1 <> cache2 =
        SimplifierCache (cache1.llvm <> cache2.llvm) (cache1.equations <> cache2.equations)

instance Monoid SimplifierCache where
    mempty = SimplifierCache mempty mempty

data CacheTag = LLVM | Equations
    deriving stock (Show)

data EquationMetadata = EquationMetadata
    { location :: Maybe Location
    , label :: Maybe Label
    , ruleId :: Maybe UniqueId
    }
    deriving stock (Eq, Show)

data EquationTrace term
    = EquationApplied term EquationMetadata term
    | EquationNotApplied term EquationMetadata ApplyEquationFailure
    deriving stock (Eq, Show)

{- | For the given equation trace, construct a new one,
     removing the heavy-weight information (the states),
     but keeping the meta-data (rule labels).
-}
eraseStates :: EquationTrace Term -> EquationTrace ()
eraseStates = \case
    EquationApplied _ metadata _ -> EquationApplied () metadata ()
    EquationNotApplied _ metadata failureInfo -> EquationNotApplied () metadata failureInfo

instance Pretty (EquationTrace Term) where
    pretty (EquationApplied subjectTerm metadata rewritten) =
        vsep
            [ "Simplifying term"
            , prettyTerm
            , "to"
            , pretty rewritten
            , "using " <> locationInfo
            ]
      where
        locationInfo = pretty metadata.location <> " - " <> pretty metadata.label
        prettyTerm = pretty subjectTerm
    pretty (EquationNotApplied subjectTerm metadata result) = case result of
        FailedMatch reason ->
            vsep ["Term did not match rule " <> locationInfo, prettyTerm, pretty reason]
        IndeterminateMatch ->
            vsep ["Term had indeterminate match for rule " <> locationInfo, prettyTerm]
        RuleNotPreservingDefinedness ->
            vsep
                [ "Simplifying term"
                , prettyTerm
                , "failed because the rule at"
                , locationInfo
                , "does not preserve definedness"
                ]
        IndeterminateCondition cs ->
            vsep $
                [ "Simplifying term"
                , prettyTerm
                , "failed with indeterminate condition(s):"
                ]
                    ++ map pretty cs
                    ++ ["using " <> locationInfo]
        ConditionFalse p ->
            vsep
                [ "Simplifying term"
                , prettyTerm
                , "failed with false condition"
                , pretty p
                , "using " <> locationInfo
                ]
        EnsuresFalse p ->
            vsep
                [ "Simplifying term"
                , prettyTerm
                , "using " <> locationInfo
                , "resulted in ensuring false condition"
                , pretty p
                ]
        MatchConstraintViolated constrained varName ->
            vsep
                [ "Concreteness constraint violated: "
                , pretty $ show constrained <> " variable " <> show varName
                , " in rule " <> locationInfo
                , "Term:"
                , prettyTerm
                ]
      where
        locationInfo = pretty metadata.location <> " - " <> pretty metadata.label
        prettyTerm = pretty subjectTerm

isMatchFailure, isSuccess :: EquationTrace Term -> Bool
isMatchFailure (EquationNotApplied _ _ FailedMatch{}) = True
isMatchFailure (EquationNotApplied _ _ IndeterminateMatch{}) = True
isMatchFailure _ = False
isSuccess EquationApplied{} = True
isSuccess _ = False

{- | Attempt to get an equation's unique id, falling back to it's label or eventually to UNKNOWN.
  The fallbacks are useful in case of cached equation applications or the ones done via LLVM,
  as neither of these categories have unique IDs.
-}
equationRuleIdWithFallbacks :: EquationMetadata -> Text
equationRuleIdWithFallbacks metadata =
    fromMaybe "UNKNOWN" (fmap getUniqueId metadata.ruleId <|> metadata.label)

equationTraceToLogEntry :: EquationTrace Term -> KoreRpcLog.LogEntry
equationTraceToLogEntry = \case
    EquationApplied _subjectTerm metadata _rewritten ->
        KoreRpcLog.Simplification
            { origin
            , result =
                KoreRpcLog.Success _ruleId
            }
      where
        origin = KoreRpcLog.Booster
        _ruleId = equationRuleIdWithFallbacks metadata
    EquationNotApplied _subjectTerm metadata failure ->
        KoreRpcLog.Simplification
            { origin
            , result = KoreRpcLog.Failure (failureDescription failure) (Just _ruleId)
            }
      where
        origin = KoreRpcLog.Booster
        _ruleId = equationRuleIdWithFallbacks metadata

        failureDescription :: ApplyEquationFailure -> Text.Text
        failureDescription = \case
            FailedMatch{} -> "Failed match"
            IndeterminateMatch -> "IndeterminateMatch"
            IndeterminateCondition{} -> "IndeterminateCondition"
            ConditionFalse{} -> "ConditionFalse"
            EnsuresFalse{} -> "EnsuresFalse"
            RuleNotPreservingDefinedness -> "RuleNotPreservingDefinedness"
            MatchConstraintViolated{} -> "MatchConstraintViolated"

startState :: SimplifierCache -> EquationState
startState cache =
    EquationState
        { termStack = mempty
        , recursionStack = []
        , changed = False
        , predicates = mempty
        , cache
        }

eqState :: MonadLoggerIO io => StateT EquationState io a -> EquationT io a
eqState = EquationT . lift . lift

getState :: MonadLoggerIO io => EquationT io EquationState
getState = eqState get

getConfig :: MonadLoggerIO io => EquationT io EquationConfig
getConfig = EquationT ask

countSteps :: MonadLoggerIO io => EquationT io Int
countSteps = length . (.termStack) <$> getState

pushTerm :: MonadLoggerIO io => Term -> EquationT io ()
pushTerm t = eqState . modify $ \s -> s{termStack = t :<| s.termStack}

pushConstraints :: MonadLoggerIO io => Set Predicate -> EquationT io ()
pushConstraints ps = eqState . modify $ \s -> s{predicates = s.predicates <> ps}

setChanged, resetChanged :: MonadLoggerIO io => EquationT io ()
setChanged = eqState . modify $ \s -> s{changed = True}
resetChanged = eqState . modify $ \s -> s{changed = False}

getChanged :: MonadLoggerIO io => EquationT io Bool
getChanged = eqState $ gets (.changed)

pushRecursion :: MonadLoggerIO io => Term -> EquationT io (Bound "Recursion")
pushRecursion t = eqState $ do
    stk <- gets (.recursionStack)
    modify $ \s -> s{recursionStack = t : stk}
    pure (coerce $ 1 + length stk)

popRecursion :: MonadLoggerIO io => EquationT io ()
popRecursion = do
    s <- getState
    if null s.recursionStack
        then do
            withContext "abort" $
                logMessage ("Trying to pop an empty recursion stack" :: Text)
            throw $ InternalError "Trying to pop an empty recursion stack"
        else eqState $ put s{recursionStack = tail s.recursionStack}

toCache :: MonadLoggerIO io => CacheTag -> Term -> Term -> EquationT io ()
toCache tag orig result = eqState . modify $ \s -> s{cache = updateCache tag s.cache}
  where
    insertInto = Map.insert orig result
    updateCache LLVM cache = cache{llvm = insertInto cache.llvm}
    updateCache Equations cache = cache{equations = insertInto cache.equations}

fromCache :: MonadLoggerIO io => CacheTag -> Term -> EquationT io (Maybe Term)
fromCache tag t = eqState $ Map.lookup t <$> gets (select tag . (.cache))
  where
    select :: CacheTag -> SimplifierCache -> Map Term Term
    select LLVM = (.llvm)
    select Equations = (.equations)

logWarn :: MonadLogger m => Text -> m ()
logWarn msg =
    logWarnNS "booster" $
        msg <> " For more details, enable context logging '--log-context \"*abort*detail\"'"

checkForLoop :: MonadLoggerIO io => Term -> EquationT io ()
checkForLoop t = do
    EquationState{termStack} <- getState
    whenJust (Seq.elemIndexL t termStack) $ \i -> do
        withContext "abort" . withContext "detail" $
            logMessage $
                renderOneLineText $
                    "Equation loop detected:"
                        <+> hsep
                            ( intersperse "," $
                                map (\(Term attrs _) -> "term" <+> pretty (showHashHex attrs.hash)) $
                                    reverse $
                                        t : take (i + 1) (toList termStack)
                            )
        logWarn "Equation loop detected."
        throw (EquationLoop $ reverse $ t : take (i + 1) (toList termStack))

data Direction = TopDown | BottomUp
    deriving stock (Eq, Show)

data EquationPreference = PreferFunctions | PreferSimplifications
    deriving stock (Eq, Show)

runEquationT ::
    LoggerMIO io =>
    Flag "CollectEquationTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    EquationT io a ->
    io (Either EquationFailure a, SimplifierCache)
runEquationT doTracing definition llvmApi smtSolver sCache (EquationT m) = do
    globalEquationOptions <- liftIO GlobalState.readGlobalEquationOptions
    logger <- getLogger
    (res, endState) <-
        flip runStateT (startState sCache) $
            runExceptT $
                runReaderT
                    m
                    EquationConfig
                        { definition
                        , llvmApi
                        , smtSolver
                        , doTracing
                        , maxIterations = globalEquationOptions.maxIterations
                        , maxRecursion = globalEquationOptions.maxRecursion
                        , logger
                        }
    pure (res, endState.cache)

iterateEquations ::
    MonadLoggerIO io =>
    Direction ->
    EquationPreference ->
    Term ->
    EquationT io Term
iterateEquations direction preference startTerm = do
    result <- pushRecursion startTerm >>= checkCounter >> go startTerm <* popRecursion
    when (startTerm /= result) $ withContext "success" $ withTermContext result $ pure ()
    pure result
  where
    checkCounter counter = do
        config <- getConfig
        when (counter > config.maxRecursion) $ do
            let msg, details :: Text
                msg =
                    "Recursion limit exceeded. The limit can be increased by \
                    \ restarting the server with '--equation-max-recursion N'."
                details =
                    "Recursion limit exceeded. \
                    \Previous \"term*detail\" messages show the terms involved."
            withContext "abort" . withContext "detail" $ logMessage details
            logWarn msg
            throw . TooManyRecursions . (.recursionStack) =<< getState

    go :: MonadLoggerIO io => Term -> EquationT io Term
    go currentTerm
        | (getAttributes currentTerm).isEvaluated = pure currentTerm
        | otherwise = do
            config <- getConfig
            currentCount <- countSteps
            when (coerce currentCount > config.maxIterations) $ do
                let msg =
                        "Unable to finish evaluation in" <+> pretty currentCount <+> "iterations."
                withContext "abort" . withContext "detail" . logMessage . renderOneLineText $
                    msg <+> "Final term:" <+> pretty currentTerm
                logWarn $ renderOneLineText msg
                throw $
                    TooManyIterations currentCount startTerm currentTerm
            pushTerm currentTerm
            -- simplify the term using the LLVM backend first
            llvmResult <- llvmSimplify currentTerm
            -- NB llvmSimplify is idempotent. No need to iterate if
            -- the equation evaluation does not change the term any more.
            resetChanged
            -- evaluate functions and simplify (recursively at each level)
            newTerm <-
                let simp = cached Equations $ traverseTerm direction simp (applyHooksAndEquations preference)
                 in simp llvmResult
            changeFlag <- getChanged
            if changeFlag
                then checkForLoop newTerm >> resetChanged >> go newTerm
                else pure llvmResult

llvmSimplify :: forall io. MonadLoggerIO io => Term -> EquationT io Term
llvmSimplify term = do
    config <- getConfig
    case config.llvmApi of
        Nothing -> pure term
        Just api -> do
            logOtherNS "booster" (LevelOther "Simplify") "Calling LLVM simplification"
            let simp = cached LLVM $ evalLlvm config.definition api $ traverseTerm BottomUp simp pure
             in simp term
  where
    evalLlvm definition api cb t@(Term attributes _)
        | attributes.isEvaluated = pure t
        | isConcrete t && attributes.canBeEvaluated = withContext "llvm" $ do
            LLVM.simplifyTerm api definition t (sortOfTerm t)
                >>= \case
                    Left (LlvmError e) -> do
                        let msg = "LLVM backend error detected: " <> Text.decodeUtf8 e
                            details = msg <> " while evaluating " <> renderOneLineText (pretty t)
                        withContext "abort" . withContext "detail" $ logMessage details
                        logWarn msg
                        throw $ UndefinedTerm t $ LlvmError e
                    Right result -> do
                        when (result /= t) $ do
                            setChanged
                            withContext "success" $
                                withTermContext result $
                                    emitEquationTrace t Nothing (Just "LLVM") Nothing $
                                        Success result
                        toCache LLVM t result
                        pure result
        | otherwise = do
            result <- cb t
            toCache LLVM t result
            pure result

----------------------------------------
-- Interface functions

-- | Evaluate and simplify a term.
evaluateTerm ::
    LoggerMIO io =>
    Flag "CollectEquationTraces" ->
    Direction ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    Term ->
    io (Either EquationFailure Term, SimplifierCache)
evaluateTerm doTracing direction def llvmApi smtSolver =
    runEquationT doTracing def llvmApi smtSolver mempty
        . evaluateTerm' direction

-- version for internal nested evaluation
evaluateTerm' ::
    MonadLoggerIO io =>
    Direction ->
    Term ->
    EquationT io Term
evaluateTerm' direction = iterateEquations direction PreferFunctions

{- | Simplify a Pattern, processing its constraints independently.
     Returns either the first failure or the new pattern if no failure was encountered
-}
evaluatePattern ::
    LoggerMIO io =>
    Flag "CollectEquationTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    Pattern ->
    io (Either EquationFailure Pattern, SimplifierCache)
evaluatePattern doTracing def mLlvmLibrary smtSolver cache =
    runEquationT doTracing def mLlvmLibrary smtSolver cache . evaluatePattern'

-- version for internal nested evaluation
evaluatePattern' ::
    LoggerMIO io =>
    Pattern ->
    EquationT io Pattern
evaluatePattern' pat@Pattern{term, constraints, ceilConditions} = withPatternContext pat $ do
    pushConstraints constraints
    newTerm <- withTermContext term $ evaluateTerm' BottomUp term
    -- after evaluating the term, evaluate all (existing and
    -- newly-acquired) constraints, once
    traverse_ simplifyAssumedPredicate . predicates =<< getState
    -- this may yield additional new constraints, left unevaluated
    evaluatedConstraints <- predicates <$> getState
    pure Pattern{constraints = evaluatedConstraints, term = newTerm, ceilConditions}

-- evaluate the given predicate assuming all others
simplifyAssumedPredicate :: LoggerMIO io => Predicate -> EquationT io ()
simplifyAssumedPredicate p = do
    allPs <- predicates <$> getState
    let otherPs = Set.delete p allPs
    eqState $ modify $ \s -> s{predicates = otherPs}
    newP <- simplifyConstraint' True $ coerce p
    pushConstraints $ Set.singleton $ coerce newP

evaluateConstraints ::
    LoggerMIO io =>
    Flag "CollectEquationTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    Set Predicate ->
    io (Either EquationFailure (Set Predicate), SimplifierCache)
evaluateConstraints doTracing def mLlvmLibrary smtSolver cache =
    runEquationT doTracing def mLlvmLibrary smtSolver cache . evaluateConstraints'

evaluateConstraints' ::
    LoggerMIO io =>
    Set Predicate ->
    EquationT io (Set Predicate)
evaluateConstraints' constraints = do
    pushConstraints constraints
    -- evaluate all existing constraints, once
    traverse_ simplifyAssumedPredicate . predicates =<< getState
    -- this may yield additional new constraints, left unevaluated
    predicates <$> getState

----------------------------------------

{- | Apply function equations and simplifications at all levels of a
   term AST, in the given direction (bottom-up or top-down).

  No iteration happens at the same AST level inside these traversals,
  one equation will be applied per level (if any).
-}
traverseTerm ::
    MonadLoggerIO m => Direction -> (Term -> m Term) -> (Term -> m Term) -> Term -> m Term
traverseTerm direction onRecurse onEval trm = do
    case trm of
        dv@DomainValue{} ->
            pure dv
        v@Var{} ->
            pure v
        Injection src trg t ->
            Injection src trg <$> onRecurse t -- no injection simplification
        AndTerm arg1 arg2 ->
            AndTerm -- no \and simplification
                <$> onRecurse arg1
                <*> onRecurse arg2
        app@(SymbolApplication sym sorts args)
            | direction == BottomUp -> do
                -- evaluate arguments first
                args' <- mapM onRecurse args
                -- then try to apply equations
                onEval $ SymbolApplication sym sorts args'
            | otherwise {- direction == TopDown -} -> do
                -- try to apply equations
                t <- onEval app
                -- then recurse into arguments or re-evaluate (avoiding a loop)
                if t /= app
                    then do
                        case t of
                            SymbolApplication sym' sorts' args' ->
                                SymbolApplication sym' sorts'
                                    <$> mapM onRecurse args'
                            _otherwise ->
                                onRecurse t -- won't loop
                    else
                        SymbolApplication sym sorts
                            <$> mapM onRecurse args
        -- maps, lists, and sets, are not currently evaluated because
        -- the matcher does not provide matches on collections.
        KMap def keyVals rest ->
            KMap def
                <$> mapM (\(k, v) -> (,) <$> onRecurse k <*> onRecurse v) keyVals
                <*> maybe (pure Nothing) ((Just <$>) . onRecurse) rest
        KList def heads rest ->
            KList def
                <$> mapM onRecurse heads
                <*> maybe
                    (pure Nothing)
                    ( (Just <$>)
                        . \(mid, tails) ->
                            (,)
                                <$> onRecurse mid
                                <*> mapM onRecurse tails
                    )
                    rest
        KSet def keyVals rest ->
            KSet def
                <$> mapM onRecurse keyVals
                <*> maybe (pure Nothing) ((Just <$>) . onRecurse) rest

cached :: MonadLoggerIO io => CacheTag -> (Term -> EquationT io Term) -> Term -> EquationT io Term
cached cacheTag cb t@(Term attributes _)
    | attributes.isEvaluated = pure t
    | otherwise =
        fromCache cacheTag t >>= \case
            Nothing -> do
                simplified <- cb t
                toCache cacheTag t simplified
                pure simplified
            Just cachedTerm -> do
                when (t /= cachedTerm) $ do
                    setChanged
                    withContext "success" $
                        withContext "cached" $
                            withTermContext cachedTerm $
                                emitEquationTrace t Nothing (Just ("Cache" <> Text.pack (show cacheTag))) Nothing $
                                    Success cachedTerm
                pure cachedTerm

elseApply :: (Monad m, Eq b) => (b -> m b) -> (b -> m b) -> b -> m b
elseApply cb1 cb2 term = do
    fromCb1 <- cb1 term
    if fromCb1 /= term
        then pure fromCb1
        else cb2 term

{- | Try to apply function equations and simplifications to the given
   top-level term, in priority order and per group.
-}
applyHooksAndEquations ::
    forall io.
    MonadLoggerIO io =>
    EquationPreference ->
    Term ->
    EquationT io Term
applyHooksAndEquations pref term = do
    -- always try built-in (hooked) functions first, then equations
    tryBuiltins `whenNothing` tryEquations
  where
    whenNothing = flip fromMaybeM
    tryBuiltins :: EquationT io (Maybe Term)
    tryBuiltins = do
        case term of
            SymbolApplication sym _sorts args
                | Just hook <- flip Map.lookup Builtin.hooks =<< sym.attributes.hook -> do
                    let hookName = maybe "UNKNOWN" Text.decodeUtf8 sym.attributes.hook
                        onError e = do
                            withContext "abort" . withContext "detail" . logMessage $
                                e <> " while evaluating " <> renderOneLineText (pretty term)
                            logWarn e
                            throw (InternalError e)
                    withContext (LogContext $ "hook " <> hookName) $
                        either onError checkChanged $
                            runExcept (hook args)
            _other -> pure Nothing

    -- for the (unlikely) case that a built-in reproduces itself, we
    -- cannot blindly set the changed flag when we have applied a hook
    checkChanged :: Maybe Term -> EquationT io (Maybe Term)
    checkChanged Nothing =
        withContext "failure" (logMessage ("Hook returned no result" :: Text)) >> pure Nothing
    checkChanged (Just t) =
        withContext "success" $ withTermContext t $ do
            unless (t == term) setChanged
            pure (Just t)

    tryEquations :: EquationT io Term
    tryEquations = do
        def <- (.definition) <$> getConfig
        ( case pref of
                PreferFunctions -> do
                    applyEquations def.functionEquations handleFunctionEquation
                        `elseApply` applyEquations def.simplifications handleSimplificationEquation
                PreferSimplifications -> do
                    applyEquations def.simplifications handleSimplificationEquation
                        `elseApply` applyEquations def.functionEquations handleFunctionEquation
            )
            term

data ApplyEquationResult
    = Success Term
    | Failure ApplyEquationFailure
    deriving stock (Eq, Show)

data ApplyEquationFailure
    = FailedMatch FailReason
    | IndeterminateMatch
    | IndeterminateCondition [Predicate]
    | ConditionFalse Predicate
    | EnsuresFalse Predicate
    | RuleNotPreservingDefinedness
    | MatchConstraintViolated Constrained VarName
    deriving stock (Eq, Show)

type ResultHandler io =
    -- | action on successful equation application
    (Term -> EquationT io Term) ->
    -- | action on failed match
    EquationT io Term ->
    -- | action on aborted equation application
    EquationT io Term ->
    ApplyEquationResult ->
    EquationT io Term

handleFunctionEquation :: MonadLoggerIO io => ResultHandler io
handleFunctionEquation success continue abort = \case
    Success rewritten -> success rewritten
    Failure (FailedMatch _) -> continue
    Failure IndeterminateMatch{} -> abort
    Failure (IndeterminateCondition{}) -> abort
    Failure (ConditionFalse _) -> continue
    Failure (EnsuresFalse p) -> do
        withContext "abort" $
            logMessage ("A side condition was found to be false during evaluation (pruning)" :: Text)
        throw $ SideConditionFalse p
    Failure RuleNotPreservingDefinedness -> abort
    Failure (MatchConstraintViolated{}) -> continue

handleSimplificationEquation :: MonadLoggerIO io => ResultHandler io
handleSimplificationEquation success continue _abort = \case
    Success rewritten -> success rewritten
    Failure (FailedMatch _) -> continue
    Failure IndeterminateMatch{} -> continue
    Failure (IndeterminateCondition{}) -> continue
    Failure (ConditionFalse _) -> continue
    Failure (EnsuresFalse p) -> do
        withContext "failure" $
            logMessage ("A side condition was found to be false during evaluation (pruning)" :: Text)
        throw $ SideConditionFalse p
    Failure RuleNotPreservingDefinedness -> continue
    Failure (MatchConstraintViolated{}) -> continue

applyEquations ::
    forall io tag.
    KnownSymbol tag =>
    MonadLoggerIO io =>
    Theory (RewriteRule tag) ->
    ResultHandler io ->
    Term ->
    EquationT io Term
applyEquations theory handler term = do
    let index = termTopIndex term
    when (index == None) $ do
        withContext "abort" $ logMessage ("Index is 'None'" :: Text)
        throw (IndexIsNone term)
    let idxEquations, anyEquations :: Map Priority [RewriteRule tag]
        idxEquations = fromMaybe Map.empty $ Map.lookup index theory
        anyEquations = fromMaybe Map.empty $ Map.lookup Anything theory
        -- neither simplification nor function equations should need groups,
        -- since simplification priority is just a suggestion and function equations
        -- should not contain non-determinism except for the [owise] equation,
        -- which should be attempted last. Thus, sorting and then applying sequentially is fine.
        -- Doing this loses the runtime check of InconsistentFunctionRules, however,
        -- most function rules are in the same priority group and thus,
        -- we would be applying all of them before checking for inconsistency,
        -- which is inefficient
        equations :: [RewriteRule tag]
        equations =
            concatMap snd . Map.toAscList $
                if index == Anything
                    then idxEquations
                    else Map.unionWith (<>) idxEquations anyEquations

    processEquations equations
  where
    -- process one equation at a time, until something has happened
    processEquations ::
        [RewriteRule tag] ->
        EquationT io Term
    processEquations [] =
        pure term -- nothing to do, term stays the same
    processEquations (eq : rest) = do
        res <- applyEquation term eq
        emitEquationTrace term eq.attributes.location eq.attributes.ruleLabel eq.attributes.uniqueId res
        handler
            (\t -> setChanged >> pure t)
            (processEquations rest)
            ( withContext "abort" $
                logMessage ("Aborting simplification/function evaluation" :: Text) >> pure term
            )
            res

{- | Trace application or failure to apply an equation
     * log into stderr
     * accumulate the trace into the state
-}
emitEquationTrace ::
    MonadLoggerIO io =>
    Term ->
    Maybe Location ->
    Maybe Label ->
    Maybe UniqueId ->
    ApplyEquationResult ->
    EquationT io ()
emitEquationTrace t loc lbl uid res = do
    let newTraceItem =
            case res of
                Success rewritten -> EquationApplied t (EquationMetadata loc lbl uid) rewritten
                Failure failure -> EquationNotApplied t (EquationMetadata loc lbl uid) failure
        prettyItem = pack . renderDefault . pretty $ newTraceItem
    logOther (LevelOther "Simplify") prettyItem
    logOther
        (LevelOther "SimplifyJson")
        (Text.toStrict . encodeToLazyText $ equationTraceToLogEntry newTraceItem)
    case res of
        Success{} -> logOther (LevelOther "SimplifySuccess") prettyItem
        _ -> pure ()

applyEquation ::
    forall io tag.
    KnownSymbol tag =>
    MonadLoggerIO io =>
    Term ->
    RewriteRule tag ->
    EquationT io ApplyEquationResult
applyEquation term rule = withRuleContext rule $ fmap (either Failure Success) $ runExceptT $ do
    -- ensured by internalisation: no existentials in equations
    unless (null rule.existentials) $ do
        withContext "abort" $
            logMessage ("Equation with existentials" :: Text)
        lift . throw . InternalError $
            "Equation with existentials: " <> Text.pack (show rule)
    -- immediately cancel if not preserving definedness
    unless (null rule.computedAttributes.notPreservesDefinednessReasons) $ do
        withContext "failure" $
            logMessage $
                renderOneLineText $
                    "Uncertain about definedness of rule due to:"
                        <+> hsep (intersperse "," $ map pretty rule.computedAttributes.notPreservesDefinednessReasons)
        throwE RuleNotPreservingDefinedness
    -- immediately cancel if rule has concrete() flag and term has variables
    when (allMustBeConcrete rule.attributes.concreteness && not (Set.null (freeVariables term))) $ do
        withContext "failure" $ logMessage ("Concreteness constraint violated: term has variables" :: Text)
        throwE (MatchConstraintViolated Concrete "* (term has variables)")
    -- match lhs
    koreDef <- (.definition) <$> lift getConfig
    case matchTerms Eval koreDef rule.lhs term of
        MatchFailed failReason -> do
            withContext "match" $ withContext "failure" $ logPretty failReason
            throwE $ FailedMatch failReason
        MatchIndeterminate remainder -> do
            withContext "match" $
                withContext "failure" $
                    logMessage $
                        renderOneLineText $
                            "Uncertain about match with rule. Remainder:" <+> pretty remainder
            throwE IndeterminateMatch
        MatchSuccess subst -> do
            -- cancel if condition
            -- forall (v, t) : subst. concrete(v) -> isConstructorLike(t) /\
            --                        symbolic(v) -> not $ t isConstructorLike(t)
            -- is violated
            withContext "match" $ checkConcreteness rule.attributes.concreteness subst

            withContext "match" $
                withContext "success" $
                    logMessage $
                        renderOneLineText $
                            "Substitution:"
                                <+> (hsep $ intersperse "," $ map (\(k, v) -> pretty k <+> "->" <+> pretty v) $ Map.toList subst)

            -- check required conditions, using substitution
            let required =
                    concatMap
                        (splitBoolPredicates . coerce . substituteInTerm subst . coerce)
                        rule.requires
            -- If the required condition is _syntactically_ present in
            -- the prior (known constraints), we don't check it.
            knownPredicates <- (.predicates) <$> lift getState
            let (knownTrue, toCheck) = partition (`Set.member` knownPredicates) required
            unless (null knownTrue) $
                logMessage $
                    renderOneLineText $
                        "Known true side conditions (won't check):" <+> hsep (intersperse "," $ map pretty knownTrue)

            unclearConditions' <- catMaybes <$> mapM (checkConstraint ConditionFalse) toCheck

            case unclearConditions' of
                [] -> do
                    -- check ensured conditions, filter any
                    -- true ones, prune if any is false
                    let ensured =
                            concatMap
                                (splitBoolPredicates . substituteInPredicate subst)
                                (Set.toList rule.ensures)
                    ensuredConditions <-
                        -- throws if an ensured condition found to be false
                        catMaybes <$> mapM (checkConstraint EnsuresFalse) ensured
                    lift $ pushConstraints $ Set.fromList ensuredConditions
                    pure $ substituteInTerm subst rule.rhs
                unclearConditions -> do
                    withContext "failure" $
                        logMessage $
                            renderOneLineText $
                                "Uncertain about a condition(s) in rule:" <+> hsep (intersperse "," $ map pretty unclearConditions)
                    throwE $ IndeterminateCondition unclearConditions
  where
    -- Simplify given predicate in a nested EquationT execution.
    -- Call 'whenBottom' if it is Bottom, return Nothing if it is Top,
    -- otherwise return the simplified remaining predicate.
    checkConstraint ::
        (Predicate -> ApplyEquationFailure) ->
        Predicate ->
        ExceptT ApplyEquationFailure (EquationT io) (Maybe Predicate)
    checkConstraint whenBottom (Predicate p) = withContext "constraint" $ do
        let fallBackToUnsimplifiedOrBottom :: EquationFailure -> EquationT io Term
            fallBackToUnsimplifiedOrBottom = \case
                e@UndefinedTerm{} -> do
                    logOther (LevelOther "Simplify") . pack . renderDefault $ pretty e
                    pure FalseBool
                e -> do
                    logOther (LevelOther "Simplify") . pack . renderDefault $
                        "Aborting recursive simplification:" <> pretty e
                    pure p
        -- exceptions need to be handled differently in the recursion,
        -- falling back to the unsimplified constraint instead of aborting.
        simplified <-
            lift $ simplifyConstraint' True p `catch_` fallBackToUnsimplifiedOrBottom
        case simplified of
            FalseBool -> do
                withContext "failure" $ logMessage ("Simplified to #Bottom." :: Text)
                throwE . whenBottom $ coerce p
            TrueBool -> pure Nothing
            other -> pure . Just $ coerce other

    allMustBeConcrete (AllConstrained Concrete) = True
    allMustBeConcrete _ = False

    checkConcreteness ::
        Concreteness ->
        Map Variable Term ->
        ExceptT ApplyEquationFailure (EquationT io) ()
    checkConcreteness Unconstrained _ = pure ()
    checkConcreteness (AllConstrained constrained) subst =
        mapM_ (\(var, t) -> mkCheck (toPair var) constrained t) $ Map.assocs subst
    checkConcreteness (SomeConstrained mapping) subst =
        void $ Map.traverseWithKey (verifyVar subst) (Map.mapWithKey mkCheck mapping)

    toPair Variable{variableSort, variableName} =
        case variableSort of
            SortApp sortName _ -> (variableName, sortName)
            SortVar varName -> (variableName, varName)

    mkCheck ::
        (VarName, SortName) ->
        Constrained ->
        Term ->
        ExceptT ApplyEquationFailure (EquationT io) ()
    mkCheck (varName, _) constrained (Term attributes _)
        | not test = do
            withContext "failure" $
                logMessage $
                    renderOneLineText $
                        hsep
                            [ "Concreteness constraint violated: "
                            , pretty $ show constrained <> " variable " <> show varName
                            ]
            throwE $ MatchConstraintViolated constrained varName
        | otherwise = pure ()
      where
        test = case constrained of
            Concrete -> attributes.isConstructorLike
            Symbolic -> not attributes.isConstructorLike

    verifyVar ::
        Map Variable Term ->
        (VarName, SortName) ->
        (Term -> ExceptT ApplyEquationFailure (EquationT io) ()) ->
        ExceptT ApplyEquationFailure (EquationT io) ()
    verifyVar subst (variableName, sortName) check =
        maybe
            ( lift . throw . InternalError . Text.pack $
                "Variable not found: " <> show (variableName, sortName)
            )
            check
            $ Map.lookup Variable{variableSort = SortApp sortName [], variableName} subst

--------------------------------------------------------------------

{- Simplification for boolean predicates

    This is used during rewriting to simplify side conditions of rules
    (to decide whether or not a rule can apply, not to retain the
    ensured conditions).

    If and as soon as this function is used inside equation
    application, it needs to run within the same 'EquationT' context
    so we can detect simplification loops and avoid monad nesting.
-}
simplifyConstraint ::
    LoggerMIO io =>
    Flag "CollectEquationTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    Predicate ->
    io (Either EquationFailure Predicate, SimplifierCache)
simplifyConstraint doTracing def mbApi mbSMT cache (Predicate p) = do
    runEquationT doTracing def mbApi mbSMT cache $ (coerce <$>) . simplifyConstraint' True $ p

simplifyConstraints ::
    LoggerMIO io =>
    Flag "CollectEquationTraces" ->
    KoreDefinition ->
    Maybe LLVM.API ->
    Maybe SMT.SMTContext ->
    SimplifierCache ->
    [Predicate] ->
    io (Either EquationFailure [Predicate], SimplifierCache)
simplifyConstraints doTracing def mbApi mbSMT cache ps =
    runEquationT doTracing def mbApi mbSMT cache $
        concatMap splitAndBools
            <$> mapM ((coerce <$>) . simplifyConstraint' True . coerce) ps

-- version for internal nested evaluation
simplifyConstraint' :: MonadLoggerIO io => Bool -> Term -> EquationT io Term
-- Evaluates terms of boolean sort (coming from predicates of the form
-- 'true \equals P' using simplifyBool if they are concrete, or using
-- evaluateTerm.
simplifyConstraint' recurseIntoEvalBool = \case
    t@(Term TermAttributes{canBeEvaluated} _)
        | isConcrete t && canBeEvaluated -> withTermContext t $ do
            mbApi <- (.llvmApi) <$> getConfig
            case mbApi of
                Just api ->
                    withContext "llvm" $
                        LLVM.simplifyBool api t >>= \case
                            Left (LlvmError e) -> do
                                let msg =
                                        "LLVM backend error detected: " <> Text.decodeUtf8 e
                                    details =
                                        msg <> " while evaluating " <> renderOneLineText (pretty t)
                                withContext "abort" . withContext "detail" $ logMessage details
                                logWarn msg
                                throw $ UndefinedTerm t $ LlvmError e
                            Right res -> do
                                let result =
                                        if res
                                            then TrueBool
                                            else FalseBool
                                withContext "success" $
                                    withTermContext result $
                                        pure result
                Nothing -> if recurseIntoEvalBool then evalBool t else pure t
        | otherwise ->
            withTermContext t $
                if recurseIntoEvalBool then evalBool t else pure t
  where
    evalBool :: MonadLoggerIO io => Term -> EquationT io Term
    evalBool t = do
        prior <- getState -- save prior state so we can revert
        eqState $ put prior{termStack = mempty, changed = False}
        result <- iterateEquations BottomUp PreferFunctions t
        eqState $ put prior
        pure result
