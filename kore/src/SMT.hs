{- |
Module      : SMT
Description : Thread-safe SMT interface
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module SMT (
    SMT,
    runNoSMT,
    runSMT,
    Solver,
    SolverSetup (..),
    newSolver,
    initSolver,
    runWithSolver,
    stopSolver,
    MonadSMT (..),
    declare,
    declareFun,
    declareSort,
    declareDatatype,
    declareDatatypes,
    assert,
    check,
    checkUsing,
    getValue,
    ackCommand,
    loadFile,
    reinit,
    askRetryLimit,
    localTimeOut,
    Config (..),
    z3Config,
    cvc5Config,
    TimeOut (..),
    RetryLimit (..),
    RLimit (..),
    ResetInterval (..),
    Prelude (..),
    Result (..),
    Constructor (..),
    ConstructorArgument (..),
    DataTypeDeclaration (..),
    SmtDataTypeDeclaration,
    FunctionDeclaration (..),
    SortDeclaration (..),
    SmtSortDeclaration,
    escapeId,
    declareFun_,
    setInfo,
    setOption,
    SimpleSMT.SolverException (..),

    -- * Expressions
    SExpr (..),
    SimpleSMT.Logger,
    SimpleSMT.showSExpr,
    SimpleSMT.tBool,
    SimpleSMT.tInt,
    SimpleSMT.and,
    SimpleSMT.bool,
    SimpleSMT.eq,
    SimpleSMT.lt,
    SimpleSMT.gt,
    SimpleSMT.implies,
    SimpleSMT.int,
    SimpleSMT.not,
    SimpleSMT.or,
    SimpleSMT.forallQ,
    SimpleSMT.existsQ,
) where

import Control.Concurrent.MVar
import Control.Exception (
    Exception,
    IOException,
    SomeException,
 )
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (
    MonadCatch,
    MonadMask,
    MonadThrow,
 )
import Control.Monad.Catch qualified as Exception
import Control.Monad.Counter qualified as Counter
import Control.Monad.Morph qualified as Morph
import Control.Monad.RWS.Strict (
    RWST,
 )
import Control.Monad.Reader (
    ReaderT (ReaderT),
 )
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Lazy qualified as State.Lazy
import Control.Monad.State.Strict (
    StateT,
    runStateT,
 )
import Control.Monad.State.Strict qualified as State.Strict
import Control.Monad.Trans qualified as Trans
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe qualified as Maybe
import Data.Bifunctor (second)
import Data.Generics.Product (
    field,
 )
import Data.Limit
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Kore.Log.WarnRestartSolver (warnRestartSolver)
import Log (
    LoggerT,
    MonadLog (..),
 )
import Log qualified
import Logic (
    LogicT,
    mapLogicT,
 )
import Prelude.Kore hiding (
    assert,
 )
import Prof
import SMT.SimpleSMT (
    Constructor (..),
    ConstructorArgument (..),
    DataTypeDeclaration (..),
    FunctionDeclaration (..),
    Result (..),
    SExpr (..),
    SmtDataTypeDeclaration,
    SmtFunctionDeclaration,
    SmtSortDeclaration,
    Solver (..),
    SolverException,
    SolverHandle (..),
    SortDeclaration (..),
    pop,
    push,
    showSExpr,
 )
import SMT.SimpleSMT qualified as SimpleSMT

-- * Interface

-- | Access 'SMT' through monad transformers.
class Monad m => MonadSMT m where
    withSolver :: m a -> m a
    default withSolver ::
        ( Morph.MFunctor t
        , MonadSMT n
        , m ~ t n
        ) =>
        m a ->
        m a
    withSolver action = Morph.hoist withSolver action
    {-# INLINE withSolver #-}

    -- | Lift an SMT action.
    liftSMT :: SMT a -> m a
    default liftSMT ::
        ( MonadTrans t
        , MonadSMT n
        , m ~ t n
        ) =>
        SMT a ->
        m a
    liftSMT = Trans.lift . liftSMT

-- | Declares a general SExpr to SMT.
declare :: MonadSMT m => Text -> Text -> SExpr -> m SExpr
declare name origName = liftSMT . declareSMT name origName
{-# INLINE declare #-}

-- | Declares a function symbol to SMT.
declareFun :: MonadSMT m => SmtFunctionDeclaration -> Text -> m SExpr
declareFun smtFD = liftSMT . declareFunSMT smtFD
{-# INLINE declareFun #-}

-- | Declares a sort to SMT.
declareSort :: MonadSMT m => SmtSortDeclaration -> m SExpr
declareSort = liftSMT . declareSortSMT
{-# INLINE declareSort #-}

-- | Declares a constructor-based sort to SMT.
declareDatatype :: MonadSMT m => SmtDataTypeDeclaration -> m ()
declareDatatype = liftSMT . declareDatatypeSMT
{-# INLINE declareDatatype #-}

-- | Declares a constructor-based sort to SMT.
declareDatatypes :: MonadSMT m => [SmtDataTypeDeclaration] -> m ()
declareDatatypes = liftSMT . declareDatatypesSMT
{-# INLINE declareDatatypes #-}

-- | Assume a fact.
assert :: MonadSMT m => SExpr -> m ()
assert = liftSMT . assertSMT
{-# INLINE assert #-}

-- | Check if the current set of assertions is satisfiable.
check :: MonadSMT m => m (Maybe Result)
check = liftSMT checkSMT
{-# INLINE check #-}

-- | Check if the current set of assertions is satisfiable, using the tactic provided
checkUsing :: MonadSMT m => SExpr -> m (Maybe Result)
checkUsing = liftSMT . checkSMTUsing
{-# INLINE checkUsing #-}

-- | Retrieve the given variables from the model (only valid if satisfiable)
getValue :: MonadSMT m => [SExpr] -> m (Either Text [(SExpr, SExpr)])
getValue = liftSMT . getValueSMT
{-# INLINE getValue #-}

-- | A command with an uninteresting result.
ackCommand :: MonadSMT m => SExpr -> m ()
ackCommand = liftSMT . ackCommandSMT
{-# INLINE ackCommand #-}

-- | Load a .smt2 file
loadFile :: MonadSMT m => FilePath -> m ()
loadFile = liftSMT . loadFileSMT
{-# INLINE loadFile #-}

-- | Reinitialize the SMT
reinit :: MonadSMT m => m ()
reinit = liftSMT reinitSMT
{-# INLINE reinit #-}

-- | Get the retry limit from the SMT
askRetryLimit :: MonadSMT m => m RetryLimit
askRetryLimit = liftSMT askRetryLimitSMT
{-# INLINE askRetryLimit #-}

-- * Implementation

data SolverSetup = SolverSetup
    { userInit :: !(SolverSetup -> LoggerT IO ())
    , refSolverHandle :: !(MVar SolverHandle)
    , config :: !Config
    }
    deriving stock (GHC.Generic)

reinitSMT' :: SolverSetup -> LoggerT IO ()
reinitSMT' sharedSolverSetup =
    unshareSolverHandle sharedSolverSetup $ \solverSetup -> do
        withSolver' solverSetup $ \solver -> SimpleSMT.simpleCommand solver ["reset"]
        initSolver solverSetup
        modifySolverHandle solverSetup $ Lens.assign (field @"queryCounter") 0

withSolverHandle :: SolverSetup -> (SolverHandle -> LoggerT IO a) -> LoggerT IO a
withSolverHandle solverSetup action = do
    let mvar = refSolverHandle solverSetup
    Exception.bracket
        (Trans.liftIO $ takeMVar mvar)
        (Trans.liftIO . putMVar mvar)
        action

withSolverHandleWithRestart :: SolverSetup -> (SolverHandle -> LoggerT IO a) -> LoggerT IO a
withSolverHandleWithRestart solverSetup action = do
    bracketWithExceptions
        (Trans.liftIO $ takeMVar mvar)
        (Trans.liftIO . putMVar mvar)
        handleExceptions
        action
  where
    mvar = refSolverHandle solverSetup
    Config{executable = exe, arguments = args} = config solverSetup

    handleExceptions originalHandle exception =
        case castToSolverException exception of
            Just solverException -> do
                warnRestartSolver solverException
                restartSolverAndRetry
            Nothing -> do
                Trans.liftIO $ putMVar mvar originalHandle
                Exception.throwM exception

    restartSolverAndRetry = do
        logAction <- Log.askLogAction
        newSolverHandle <-
            Trans.liftIO $
                Exception.handle handleIOException $
                    SimpleSMT.newSolver exe args logAction
        _ <- Trans.liftIO $ putMVar mvar newSolverHandle
        initSolver solverSetup
        action newSolverHandle

    handleIOException :: IOException -> IO SolverHandle
    handleIOException e =
        (error . unlines)
            [ Exception.displayException e
            , "Could not start Z3; is it installed?"
            ]

    castToSolverException :: SomeException -> Maybe SolverException
    castToSolverException = Exception.fromException

bracketWithExceptions ::
    Exception e =>
    MonadMask m =>
    m t ->
    (t -> m a) ->
    (t -> e -> m b) ->
    (t -> m b) ->
    m b
bracketWithExceptions acquire release handleException use =
    Exception.mask $ \unmasked -> do
        resource <- acquire
        result <- Exception.try (unmasked $ use resource)
        case result of
            Left e -> do
                handleException resource e
            Right v -> do
                _ <- release resource
                return v

withSolver' :: SolverSetup -> (Solver -> IO a) -> LoggerT IO a
withSolver' solverSetup action =
    withSolverHandle solverSetup $ \solverHandle -> do
        logAction <- Log.askLogAction
        Trans.liftIO $ action (Solver solverHandle logAction)

withSolverWithRestart :: SolverSetup -> (Solver -> IO a) -> LoggerT IO a
withSolverWithRestart solverSetup action =
    withSolverHandleWithRestart solverSetup $ \solverHandle -> do
        logAction <- Log.askLogAction
        Trans.liftIO $ action (Solver solverHandle logAction)

modifySolverHandle :: SolverSetup -> StateT SolverHandle (LoggerT IO) a -> LoggerT IO a
modifySolverHandle solverSetup action = do
    let mvar = refSolverHandle solverSetup
    solverHandle <- Trans.liftIO $ takeMVar mvar
    Exception.onException
        ( do
            (a, solverHandle') <- runStateT action solverHandle
            Trans.liftIO $ putMVar mvar solverHandle'
            pure a
        )
        (Trans.liftIO $ putMVar mvar solverHandle)

{- | Run an 'SMT' action with a 'SolverHandle' that is not shared.

@unshareSolverHandle@ works by locking the received 'MVar' and running the
action with a new 'MVar' that is not shared with any other thread.
-}
unshareSolverHandle :: SolverSetup -> (SolverSetup -> LoggerT IO a) -> LoggerT IO a
unshareSolverHandle solverSetup action = do
    let mvarShared = refSolverHandle solverSetup
    mvarUnshared <- Trans.liftIO $ takeMVar mvarShared >>= newMVar
    let unshare = Lens.set (field @"refSolverHandle") mvarUnshared
        replaceMVar =
            Trans.liftIO $ takeMVar mvarUnshared >>= putMVar mvarShared
    Exception.finally (action (unshare solverSetup)) replaceMVar

-- | Increase the 'queryCounter' and indicate if the solver should be reset.
incrementQueryCounter ::
    Monad monad => ResetInterval -> StateT SolverHandle monad Bool
incrementQueryCounter (ResetInterval resetInterval) = do
    Lens.modifying (field @"queryCounter") (+ 1)
    counter <- Lens.use (field @"queryCounter")
    -- Due to an issue with the SMT solver, we need to reinitialise it after a
    -- number of runs, specified here. This number can be adjusted based on
    -- experimentation.
    pure (toInteger counter >= resetInterval)

instance (MonadSMT m, Monoid w) => MonadSMT (AccumT w m) where
    withSolver = mapAccumT withSolver
    {-# INLINE withSolver #-}

instance MonadSMT m => MonadSMT (IdentityT m)

instance MonadSMT m => MonadSMT (LogicT m) where
    withSolver = mapLogicT withSolver
    {-# INLINE withSolver #-}

instance MonadSMT m => MonadSMT (Reader.ReaderT r m)

instance MonadSMT m => MonadSMT (Maybe.MaybeT m)

instance MonadSMT m => MonadSMT (State.Lazy.StateT s m)

instance MonadSMT m => MonadSMT (Counter.CounterT m)

instance MonadSMT m => MonadSMT (State.Strict.StateT s m)

instance MonadSMT m => MonadSMT (ExceptT e m)

instance MonadSMT m => MonadSMT (RWST r () s m)

-- | Time-limit for SMT queries.
newtype TimeOut = TimeOut {getTimeOut :: Limit Integer}
    deriving stock (Eq, Ord, Read, Show)

-- | Retry-limit for SMT queries.
newtype RetryLimit = RetryLimit {getRetryLimit :: Limit Integer}
    deriving stock (Eq, Ord, Read, Show)

-- | Resource-limit for SMT queries.
newtype RLimit = RLimit {getRLimit :: Limit Integer}
    deriving stock (Eq, Ord, Read, Show)

-- | Reset interval for solver.
newtype ResetInterval = ResetInterval {getResetInterval :: Integer}
    deriving stock (Eq, Ord, Read, Show)

-- | Optional filepath for the SMT prelude.
newtype Prelude = Prelude {getPrelude :: Maybe FilePath}

-- | Solver configuration
data Config = Config
    { executable :: !FilePath
    -- ^ solver executable file name
    , arguments :: ![String]
    -- ^ default command-line arguments to solver
    , prelude :: !Prelude
    -- ^ prelude of definitions to initialize solver
    , logFile :: !(Maybe FilePath)
    -- ^ optional log file name
    , timeOut :: !TimeOut
    , timeOutOption :: !Text
    -- ^ query time limit
    , retryLimit :: !RetryLimit
    -- ^ query retry limit
    , rLimit :: !RLimit
    -- ^ query resource limit
    , resetInterval :: !ResetInterval
    -- ^ reset solver after this number of queries
    , tactic :: !(Maybe SExpr)
    -- ^ Z3 tactic to use when checking satisfiability
    }

-- | Default configuration using the Z3 solver.
z3Config :: Config
z3Config =
    Config
        { executable = "z3"
        , arguments =
            [ "-smt2" -- use SMT-LIB2 format
            , "-in" -- read from standard input
            ]
        , prelude = Prelude Nothing
        , logFile = Nothing
        , timeOut = TimeOut (Limit 125)
        , timeOutOption = "timeout"
        , retryLimit = RetryLimit (Limit 3)
        , rLimit = RLimit Unlimited
        , resetInterval = ResetInterval 100
        , tactic = Nothing
        }

-- | Default configuration using the CVC5 solver.
cvc5Config :: Config
cvc5Config =
    Config
        { executable = "cvc5"
        , arguments = [ "--incremental", "--full-saturate-quant"]
        , prelude = Prelude Nothing
        , logFile = Nothing
        , timeOut = TimeOut (Limit 125)
        , timeOutOption = "tlimit-per"
        , retryLimit = RetryLimit (Limit 3)
        , rLimit = RLimit Unlimited
        , resetInterval = ResetInterval 100
        , tactic = Nothing
        }

initSolver :: SolverSetup -> LoggerT IO ()
initSolver solverSetup = do
    let Config{timeOut, rLimit, prelude} = config solverSetup
        preludeFile = getPrelude prelude
    runWithSolver (setTimeOut timeOut) solverSetup
    runWithSolver (setRLimit rLimit) solverSetup
    runWithSolver (traverse_ loadFile preludeFile) solverSetup
    userInit solverSetup solverSetup

{- | Initialize a new solverHandle with the given 'Config'.

The new solverHandle is returned in an 'MVar' for thread-safety.
-}
newSolver :: Config -> LoggerT IO (MVar SolverHandle)
newSolver config =
    Exception.handle handleIOException $ do
        someLogAction <- Log.askLogAction
        Trans.liftIO $ do
            solverHandle <- SimpleSMT.newSolver exe args someLogAction
            newMVar solverHandle
  where
    Config{executable = exe, arguments = args} = config
    handleIOException :: IOException -> LoggerT IO a
    handleIOException e =
        (error . unlines)
            [ Exception.displayException e
            , "Could not start Z3; is it installed?"
            ]

{- | Shut down a solver.

@stopSolver@ should not be called until all threads are done with the solver:
the 'Solver' is never returned to the 'MVar', so any threads waiting for the
solver will hang.
-}
stopSolver :: MVar SolverHandle -> LoggerT IO ()
stopSolver mvar = do
    logAction <- Log.askLogAction
    Trans.liftIO $ do
        solverHandle <- takeMVar mvar
        let solver = Solver solverHandle logAction
        _ <- SimpleSMT.stop solver
        return ()

-- Need to quote every identifier in SMT between pipes
-- to escape special chars
escapeId :: Text -> Text
escapeId name = "|" <> name <> "|"

-- | Declares a function symbol to SMT, returning ().
declareFun_ :: MonadSMT m => SmtFunctionDeclaration -> m ()
declareFun_ declaration =
    Monad.void $ declareFun declaration ""

-- | SMT-LIB @set-info@ command.
setInfo :: MonadSMT m => Text -> SExpr -> m ()
setInfo infoFlag expr =
    ackCommand $ List (Atom "set-info" : Atom infoFlag : [expr])

-- | SMT-LIB @set-option@ command.
setOption :: MonadSMT m => Text -> SExpr -> m ()
setOption infoFlag expr =
    ackCommand $ List (Atom "set-option" : Atom infoFlag : [expr])

{- | Query an external SMT solver (if available).

The solver may be shared among multiple threads. Individual commands will
acquire and release the solver as needed, but sequences of commands from
different threads may be interleaved; use 'withSolver' to acquire exclusive
access to the solver for a sequence of commands.
-}
newtype SMT a = SMT (Maybe SolverSetup -> LoggerT IO a)
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadLog
        , MonadProf
        , MonadCatch
        , MonadThrow
        , MonadMask
        , MonadBase IO
        , MonadBaseControl IO
        )
        via ReaderT (Maybe SolverSetup) (LoggerT IO)

runWithSolver :: SMT a -> SolverSetup -> LoggerT IO a
runWithSolver (SMT action) = action . Just

runNoSMT :: SMT a -> LoggerT IO a
runNoSMT (SMT action) = action Nothing

-- | Run an external SMT solver.
runSMT :: Config -> SMT () -> SMT a -> LoggerT IO a
runSMT config userInit action =
    Exception.bracket (newSolver config) stopSolver $ \refSolverHandle -> do
        let solverSetup = SolverSetup{userInit = runWithSolver userInit, refSolverHandle, config}
        initSolver solverSetup
        runWithSolver action solverSetup

instance MonadSMT SMT where
    withSolver (SMT action) =
        SMT $ \case
            Nothing -> action Nothing
            Just sharedSolverSetup -> do
                unshareSolverHandle sharedSolverSetup $ \solverSetup -> do
                    withSolverWithRestart solverSetup push
                    action (Just solverSetup) `Exception.finally` do
                        withSolverWithRestart solverSetup pop
                        needReset <-
                            modifySolverHandle solverSetup $
                                incrementQueryCounter (resetInterval (config solverSetup))
                        when needReset (reinitSMT' solverSetup)
    liftSMT = id

declareSMT :: Text -> Text -> SExpr -> SMT SExpr
declareSMT name origName typ =
    SMT $ \case
        Nothing -> return (Atom name)
        Just solverSetup ->
            withSolver' solverSetup $ \solver ->
                SimpleSMT.declare solver (Atom name) comment typ
  where
    comment = mkComment origName name

declareFunSMT :: FunctionDeclaration SExpr SExpr -> Text -> SMT SExpr
declareFunSMT declaration@FunctionDeclaration{name} origName =
    SMT $ \case
        Nothing -> return name
        Just solverSetup ->
            withSolver' solverSetup $ \solver ->
                SimpleSMT.declareFun solver declaration comment
  where
    comment = mkComment origName (Text.pack . showSExpr $ name)

mkComment :: Text -> Text -> Maybe SExpr
mkComment origName name =
    Atom
        <$> if Text.null origName
            then Nothing
            else
                Just $
                    (Text.init . Text.unlines . map (Text.append "; ") . Text.lines) $
                        origName <> " -> " <> name

declareSortSMT :: SortDeclaration SExpr -> SMT SExpr
declareSortSMT declaration@SortDeclaration{name} =
    SMT $ \case
        Nothing -> return name
        Just solverSetup ->
            withSolver' solverSetup $ \solver ->
                SimpleSMT.declareSort solver declaration

declareDatatypeSMT :: SmtDataTypeDeclaration -> SMT ()
declareDatatypeSMT declaration =
    SMT $ \case
        Nothing -> return ()
        Just solverSetup ->
            withSolver' solverSetup $ \solver ->
                SimpleSMT.declareDatatype solver declaration

declareDatatypesSMT :: [SmtDataTypeDeclaration] -> SMT ()
declareDatatypesSMT datatypes =
    SMT $ \case
        Nothing -> return ()
        Just solverSetup ->
            withSolver' solverSetup $ \solver ->
                SimpleSMT.declareDatatypes solver datatypes

assertSMT :: SExpr -> SMT ()
assertSMT fact =
    SMT $ \case
        Nothing -> return ()
        Just solverSetup ->
            traceProf ":solver:assert" $
                withSolver' solverSetup $ \solver ->
                    SimpleSMT.assert solver fact

checkSMT :: SMT (Maybe Result)
checkSMT =
    SMT $ \case
        Nothing -> return Nothing
        Just solverSetup -> do
            result <-
                traceProf ":solver:check" $
                    withSolver' solverSetup (\solver -> SimpleSMT.checkUsing solver (tactic . config $ solverSetup))
            return (Just result)

checkSMTUsing :: SExpr -> SMT (Maybe Result)
checkSMTUsing tactic =
    SMT $ \case
        Nothing -> return Nothing
        Just solverSetup -> do
            result <-
                traceProf ":solver:check" $
                    withSolver' solverSetup (\solver -> SimpleSMT.checkUsing solver (Just tactic))
            return (Just result)

getValueSMT :: [SExpr] -> SMT (Either Text [(SExpr, SExpr)])
getValueSMT targets =
    SMT $ \case
        Nothing -> return $ Left "no-solver"
        Just solverSetup ->
            traceProf ":solver:get-value" $
                onErrorLeft $
                    fmap (map (second SimpleSMT.value)) $
                        withSolver' solverSetup (flip SimpleSMT.getExprs targets)
  where
    onErrorLeft :: (MonadIO m, MonadCatch m) => m a -> m (Either Text a)
    onErrorLeft action =
        fmap Right action `Exception.catch` \(e :: IOException) -> pure $ Left $ Text.pack $ show e

ackCommandSMT :: SExpr -> SMT ()
ackCommandSMT command =
    SMT $ \case
        Nothing -> return ()
        Just solverSetup ->
            withSolver' solverSetup $ \solver ->
                SimpleSMT.ackCommand solver Nothing command

loadFileSMT :: FilePath -> SMT ()
loadFileSMT path =
    SMT $ \case
        Nothing -> return ()
        Just solverSetup ->
            withSolver' solverSetup $ \solver ->
                SimpleSMT.loadFile solver path

reinitSMT :: SMT ()
reinitSMT =
    SMT $ \case
        Nothing -> return ()
        Just solverSetup -> reinitSMT' solverSetup

{- | Run a solver action with an adjusted timeout,
 and reset the timeout when it's done.
-}
localTimeOut :: MonadSMT m => (TimeOut -> TimeOut) -> m a -> m a
localTimeOut adjust isolated = do
    originalTimeOut <- liftSMT extractTimeOut
    setTimeOut $ adjust originalTimeOut
    isolated <* setTimeOut originalTimeOut
  where
    extractTimeOut =
        SMT $
            pure . \case
                Nothing -> TimeOut Unlimited
                Just setup -> timeOut $ config setup

-- | Get the retry limit for SMT queries.
askRetryLimitSMT :: SMT RetryLimit
askRetryLimitSMT =
    SMT $
        pure . \case
            Nothing -> RetryLimit (Limit 0)
            Just setup -> retryLimit $ config setup

-- --------------------------------
-- Internal

-- | Set the query time limit.
setTimeOut :: MonadSMT m => TimeOut -> m ()
setTimeOut TimeOut{getTimeOut} = do
    timeOutOption <- liftSMT extractTimeOutOption
    case getTimeOut of
        Limit timeOut ->
            setOption (":" <> timeOutOption) (SimpleSMT.int timeOut)
        Unlimited ->
            return ()

    where
    extractTimeOutOption =
        SMT $
            pure . \case
                Nothing -> ""
                Just setup -> timeOutOption $ config setup


-- | Set the query resource limit.
setRLimit :: MonadSMT m => RLimit -> m ()
setRLimit RLimit{getRLimit} =
    case getRLimit of
        Limit rLimit ->
            setOption ":rlimit" (SimpleSMT.int rLimit)
        Unlimited ->
            return ()
