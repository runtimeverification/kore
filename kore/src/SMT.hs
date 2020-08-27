{-|
Module      : SMT
Description : Thread-safe SMT interface
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
-}

module SMT
    ( SMT, getSMT
    , Solver
    , stopSolver
    , runSMT
    , MonadSMT (..)
    , Config (..)
    , defaultConfig
    , TimeOut (..)
    , Result (..)
    , Constructor (..)
    , ConstructorArgument (..)
    , DataTypeDeclaration (..)
    , SmtDataTypeDeclaration
    , FunctionDeclaration (..)
    , SortDeclaration (..)
    , SmtSortDeclaration
    , escapeId
    , declareFun_
    , setInfo
    , setOption
    , NoSMT (..), runNoSMT
    , SimpleSMT.SolverException (..)
    -- * Expressions
    , SExpr (..)
    , SimpleSMT.Logger
    , SimpleSMT.showSExpr
    , SimpleSMT.tBool
    , SimpleSMT.tInt
    , SimpleSMT.and
    , SimpleSMT.bool
    , SimpleSMT.eq
    , SimpleSMT.lt
    , SimpleSMT.gt
    , SimpleSMT.implies
    , SimpleSMT.int
    , SimpleSMT.not
    , SimpleSMT.or
    , SimpleSMT.forallQ
    , SimpleSMT.existsQ
    ) where

import Prelude.Kore hiding
    ( assert
    )

import Control.Concurrent.MVar
import Control.Exception
    ( IOException
    )
import qualified Control.Monad as Monad
import Control.Monad.Catch
    ( MonadCatch
    , MonadMask
    , MonadThrow
    )
import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.Counter as Counter
import qualified Control.Monad.Morph as Morph
import Control.Monad.Reader
    ( ReaderT (..)
    , runReaderT
    )
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Lazy as State.Lazy
import qualified Control.Monad.State.Strict as State.Strict
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans.Accum
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.Maybe as Maybe
import Data.Limit
import Data.Text
    ( Text
    )

import Control.Monad
    ( join
    )
import qualified Data.Foldable as Foldable
import Log
    ( LogAction
    , LoggerT
    , MonadLog (..)
    , SomeEntry
    )
import qualified Log
import Logic
    ( LogicT
    , mapLogicT
    )
import Prof
import SMT.SimpleSMT
    ( Constructor (..)
    , ConstructorArgument (..)
    , DataTypeDeclaration (..)
    , FunctionDeclaration (..)
    , Result (..)
    , SExpr (..)
    , SmtDataTypeDeclaration
    , SmtFunctionDeclaration
    , SmtSortDeclaration
    , Solver (..)
    , SolverHandle (..)
    , SortDeclaration (..)
    , pop
    , push
    )
import qualified SMT.SimpleSMT as SimpleSMT

-- * Interface

-- | Access 'SMT' through monad transformers.
class Monad m => MonadSMT m where
    withSolver :: m a -> m a
    default withSolver
        ::  ( Morph.MFunctor t
            , MonadSMT n
            , m ~ t n
            )
        => m a
        -> m a
    withSolver action = Morph.hoist withSolver action
    {-# INLINE withSolver #-}

    -- | Declares a general SExpr to SMT.
    declare :: Text -> SExpr -> m SExpr
    default declare
        :: (Trans.MonadTrans t, MonadSMT n, m ~ t n)
        => Text
        -> SExpr
        -> m SExpr
    declare text = Trans.lift . declare text
    {-# INLINE declare #-}

    -- | Declares a function symbol to SMT.
    declareFun :: SmtFunctionDeclaration -> m SExpr
    default declareFun
        :: (Trans.MonadTrans t, MonadSMT n, m ~ t n)
        => SmtFunctionDeclaration
        -> m SExpr
    declareFun = Trans.lift . declareFun
    {-# INLINE declareFun #-}

    -- | Declares a sort to SMT.
    declareSort :: SmtSortDeclaration -> m SExpr
    default declareSort
        :: (Trans.MonadTrans t, MonadSMT n, m ~ t n)
        => SmtSortDeclaration
        -> m SExpr
    declareSort = Trans.lift . declareSort
    {-# INLINE declareSort #-}

    -- | Declares a constructor-based sort to SMT.
    declareDatatype :: SmtDataTypeDeclaration -> m ()
    default declareDatatype
        :: (Trans.MonadTrans t, MonadSMT n, m ~ t n)
        => SmtDataTypeDeclaration
        -> m ()
    declareDatatype = Trans.lift . declareDatatype
    {-# INLINE declareDatatype #-}

    -- | Declares a constructor-based sort to SMT.
    declareDatatypes ::  [SmtDataTypeDeclaration] -> m ()
    default declareDatatypes
        :: (Trans.MonadTrans t, MonadSMT n, m ~ t n)
        => [SmtDataTypeDeclaration]
        -> m ()
    declareDatatypes = Trans.lift . declareDatatypes
    {-# INLINE declareDatatypes #-}

    -- | Assume a fact.
    assert :: SExpr -> m ()
    default assert
        :: (Trans.MonadTrans t, MonadSMT n, m ~ t n)
        => SExpr
        -> m ()
    assert = Trans.lift . assert
    {-# INLINE assert #-}

    {- | Check if the current set of assertions is satisfiable.

    See also: 'assert'

    -}
    check :: m Result
    default check
        :: (Trans.MonadTrans t, MonadSMT n, m ~ t n)
        => m Result
    check = Trans.lift check
    {-# INLINE check #-}

    -- | A command with an uninteresting result.
    ackCommand :: SExpr -> m ()
    default ackCommand
        :: (Trans.MonadTrans t, MonadSMT n, m ~ t n)
        => SExpr
        -> m ()
    ackCommand = Trans.lift . ackCommand
    {-# INLINE ackCommand #-}

    -- | Load a .smt2 file
    loadFile :: FilePath -> m ()
    default loadFile
        :: (Trans.MonadTrans t, MonadSMT n, m ~ t n)
        => FilePath
        -> m ()
    loadFile = Trans.lift . loadFile
    {-# INLINE loadFile #-}

-- * Dummy implementation

newtype NoSMT a = NoSMT { getNoSMT :: LoggerT IO a }
    deriving (Functor, Applicative, Monad, MonadIO)
    deriving (MonadCatch, MonadThrow, MonadMask)

runNoSMT :: NoSMT a -> LoggerT IO a
runNoSMT = getNoSMT

instance MonadProf NoSMT where
    traceEvent name = NoSMT (traceEvent name)
    {-# INLINE traceEvent #-}

instance MonadLog NoSMT where
    logEntry entry = NoSMT $ logEntry entry
    {-# INLINE logEntry #-}

    logWhile entry2 action = NoSMT $ logWhile entry2 $ getNoSMT action
    {-# INLINE logWhile #-}

instance MonadSMT NoSMT where
    withSolver = id
    declare name _ = return (Atom name)
    declareFun FunctionDeclaration { name } = return name
    declareSort SortDeclaration { name } = return name
    declareDatatype _ = return ()
    declareDatatypes _ = return ()
    loadFile _ = return ()
    ackCommand _ = return ()
    assert _ = return ()
    check = return Unknown

-- * Implementation

data SolverInitAndHandle =
    SolverInitAndHandle
        { userInit :: !(SMT ())
        , mSolverHandle :: !(MVar SolverHandle)
        }

{- | Query an external SMT solver.

The solver may be shared among multiple threads. Individual commands will
acquire and release the solver as needed, but sequences of commands from
different threads may be interleaved; use 'inNewScope' to acquire exclusive
access to the solver for a sequence of commands.

 -}
newtype SMT a = SMT { getSMT :: ReaderT SolverInitAndHandle (LoggerT IO) a }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadLog
        )
    deriving
        ( MonadCatch
        , MonadThrow
        , MonadMask
        )

instance MonadProf SMT where
    traceEvent name = SMT (traceEvent name)
    {-# INLINE traceEvent #-}

withSolverHandle :: (SolverHandle -> SMT a) -> SMT a
withSolverHandle action = do
    mvar <- SMT (Reader.asks mSolverHandle)
    Exception.bracket
        (Trans.liftIO $ takeMVar mvar)
        (Trans.liftIO . putMVar mvar)
        action

askLogAction :: SMT (LogAction IO SomeEntry)
askLogAction = SMT $ Trans.lift Log.askLogAction
{-# INLINE askLogAction #-}

withSolver' :: (Solver -> IO a) -> SMT a
withSolver' action =
    withSolverHandle $ \solverHandle -> do
        logAction <- askLogAction
        Trans.liftIO $ action (Solver solverHandle logAction)

instance MonadSMT SMT where
    withSolver smt =
        withSolverHandle $ \solverHandle -> do
            -- Create an unshared "dummy" mutex for the solverHandle.
            mvar <- Trans.liftIO $ newMVar solverHandle
            logAction <- askLogAction
            userInit <- SMT (Reader.asks userInit)
            let solver = Solver solverHandle logAction
            -- Run the SMT with the unshared mutex.
            -- The SMT will never block waiting to acquire the solver.
            Trans.liftIO $ push solver
            (SMT . Trans.lift)
                (Exception.finally
                    (runReaderT
                        (getSMT smt)
                        (SolverInitAndHandle userInit mvar)
                    )
                    (Trans.liftIO $ pop solver)
                )

    declare name typ =
        withSolver' $ \solver -> SimpleSMT.declare solver (Atom name) typ

    declareFun declaration =
        withSolver' $ \solver -> SimpleSMT.declareFun solver declaration

    declareSort declaration =
        withSolver' $ \solver -> SimpleSMT.declareSort solver declaration

    declareDatatype declaration =
        withSolver' $ \solver -> SimpleSMT.declareDatatype solver declaration

    declareDatatypes datatypes =
        withSolver' $ \solver -> SimpleSMT.declareDatatypes solver datatypes

    assert fact =
        traceProf ":solver:assert"
        $ withSolver' $ \solver -> SimpleSMT.assert solver fact

    check = traceProf ":solver:check" $ withSolver' SimpleSMT.check

    ackCommand command =
        withSolver' $ \solver -> SimpleSMT.ackCommand solver command

    loadFile path =
        withSolver' $ \solver -> SimpleSMT.loadFile solver path

instance (MonadSMT m, Monoid w) => MonadSMT (AccumT w m) where
    withSolver = mapAccumT withSolver
    {-# INLINE withSolver #-}

instance MonadSMT m => MonadSMT (IdentityT m)

instance MonadSMT m => MonadSMT (LogicT m) where
    withSolver = mapLogicT withSolver
    {-# INLINE withSolver #-}

instance MonadSMT m => MonadSMT (ReaderT r m)

instance MonadSMT m => MonadSMT (Maybe.MaybeT m)

instance MonadSMT m => MonadSMT (State.Lazy.StateT s m)

instance MonadSMT m => MonadSMT (Counter.CounterT m)

instance MonadSMT m => MonadSMT (State.Strict.StateT s m)

instance MonadSMT m => MonadSMT (ExceptT e m)

-- | Time-limit for SMT queries.
newtype TimeOut = TimeOut { getTimeOut :: Limit Integer }
    deriving (Eq, Ord, Read, Show)

-- | Solver configuration
data Config =
    Config
        { executable :: FilePath
        -- ^ solver executable file name
        , arguments :: [String]
        -- ^ default command-line arguments to solver
        , preludeFile :: Maybe FilePath
        -- ^ prelude of definitions to initialize solver
        , logFile :: Maybe FilePath
        -- ^ optional log file name
        , timeOut :: TimeOut
        -- ^ query time limit
        }

-- | Default configuration using the Z3 solver.
defaultConfig :: Config
defaultConfig =
    Config
        { executable = "z3"
        , arguments =
            [ "-smt2"  -- use SMT-LIB2 format
            , "-in"    -- read from standard input
            ]
        , preludeFile = Nothing
        , logFile = Nothing
        , timeOut = TimeOut (Limit 40)
        }

initSolver :: Config -> SMT ()
initSolver Config { timeOut, preludeFile } = do
    join $ SMT (Reader.asks userInit)
    setTimeOut timeOut
    Foldable.traverse_ loadFile preludeFile

{- | Initialize a new solverHandle with the given 'Config'.

The new solverHandle is returned in an 'MVar' for thread-safety.

 -}
newSolver :: Config -> SMT () -> LoggerT IO (MVar SolverHandle)
newSolver config userInit =
    Exception.handle handleIOException $ do
        someLogAction <- Log.askLogAction
        mvar <- Trans.liftIO $ do
            solverHandle <- SimpleSMT.newSolver exe args someLogAction
            newMVar solverHandle
        runReaderT getSMT (SolverInitAndHandle userInit mvar)
        return mvar
  where
    Config { executable = exe, arguments = args } = config
    SMT { getSMT } = initSolver config
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

-- | Run an external SMT solver.
runSMT :: Config -> SMT () -> SMT a -> LoggerT IO a
runSMT config userInit smt =
    Exception.bracket (newSolver config userInit) stopSolver
        (\mvar -> runSMT' config mvar smt)

runSMT' :: Config -> MVar SolverHandle -> SMT a -> LoggerT IO a
runSMT' config mvar SMT { getSMT } =
    runReaderT getSMT (SolverInitAndHandle (initSolver config) mvar)

-- Need to quote every identifier in SMT between pipes
-- to escape special chars
escapeId :: Text -> Text
escapeId name = "|" <> name <> "|"


-- | Declares a function symbol to SMT, returning ().
declareFun_ :: MonadSMT m => SmtFunctionDeclaration -> m ()
declareFun_ declaration =
    Monad.void $ declareFun declaration

-- | SMT-LIB @set-info@ command.
setInfo :: MonadSMT m => Text -> SExpr -> m ()
setInfo infoFlag expr =
    ackCommand $ List (Atom "set-info" : Atom infoFlag : [expr])

-- | SMT-LIB @set-option@ command.
setOption :: MonadSMT m => Text -> SExpr -> m ()
setOption infoFlag expr =
    ackCommand $ List (Atom "set-option" : Atom infoFlag : [expr])

-- --------------------------------
-- Internal

-- | Set the query time limit.
setTimeOut :: MonadSMT m => TimeOut -> m ()
setTimeOut TimeOut { getTimeOut } =
    case getTimeOut of
        Limit timeOut ->
            setOption ":timeout" (SimpleSMT.int timeOut)
        Unlimited ->
            return ()
