{-# LANGUAGE FlexibleContexts #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Runner (
    SMTOptions (..),
    defaultSMTOptions,
    SMTContext (..),
    SMT (..),
    SMTEncode (..),
    mkContext,
    connectToSolver,
    closeContext,
    destroyContext,
    runSMT,
    evalSMT,
    declare,
    runCmd,
    runCmd_,
    runCheck,
) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.State
import Data.ByteString.Builder qualified as BS
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text, pack)
import SMTLIB.Backends qualified as Backend
import SMTLIB.Backends.Z3 qualified as Backend
import System.IO (
    BufferMode (..),
    Handle,
    IOMode (..),
    hClose,
    hSetBinaryMode,
    hSetBuffering,
    openFile,
 )

import Booster.Log (LoggerMIO (..), logMessage)
import Booster.SMT.Base
import Booster.SMT.LowLevelCodec

-- Includes all options from kore-rpc used by current clients. The
-- parser in CLOptions uses compatible names and we use the same
-- defaults. Not all options are supported in booster.
data SMTOptions = SMTOptions
    { transcript :: Maybe FilePath
    -- ^ optional log file
    , timeout :: Int
    -- ^ optional timeout for requests, 0 for none
    , retryLimit :: Maybe Int
    -- ^ optional retry. Nothing for no retry, 0 for unlimited
    , tactic :: Maybe SExpr
    -- ^ optional tactic (used verbatim) to replace (check-sat)
    }
    deriving (Eq, Show)

defaultSMTOptions :: SMTOptions
defaultSMTOptions =
    SMTOptions
        { transcript = Nothing
        , timeout = 125
        , retryLimit = Just 3
        , tactic = Nothing
        }

data SMTContext = SMTContext
    { options :: SMTOptions
    , solver :: Backend.Solver
    , solverClose :: IO ()
    , mbTranscriptHandle :: Maybe Handle
    , prelude :: [DeclareCommand]
    }

----------------------------------------
{- TODO (later)
- error handling and retries
  - retry counter in context
- (possibly) run `get-info` on Unknown responses and enhance Unknown constructor
  - smtlib2: reason-unknown = memout | incomplete | SExpr
-}

mkContext ::
    LoggerMIO io =>
    SMTOptions ->
    [DeclareCommand] ->
    io SMTContext
mkContext opts prelude = do
    logMessage ("Starting SMT solver" :: Text)
    (solver, handle) <- connectToSolver
    mbTranscriptHandle <- forM opts.transcript $ \path -> do
        logMessage $ "Transcript in file " <> pack path
        liftIO $ do
            h <- openFile path AppendMode
            hSetBuffering h (BlockBuffering Nothing)
            hSetBinaryMode h True
            BS.hPutStrLn h "; starting solver process"
            pure h
    whenJust mbTranscriptHandle $ \h ->
        liftIO $ BS.hPutStrLn h "; solver initialised\n;;;;;;;;;;;;;;;;;;;;;;;"
    pure
        SMTContext
            { solver
            , solverClose = Backend.close handle
            , mbTranscriptHandle
            , prelude
            , options = opts
            }

{- | Close the connection to the SMT solver process, but hold on to the other resources in @SMTContext@,
such as the transcript handle. This function is used in 'Booster.SMT.Interface.hardResetSolver'
to reset the solver connection while keeping the same transcript handle.
-}
closeContext :: LoggerMIO io => SMTContext -> io ()
closeContext ctxt = do
    logMessage ("Stopping SMT solver" :: Text)
    whenJust ctxt.mbTranscriptHandle $ \h -> liftIO $ do
        BS.hPutStrLn h "; stopping solver\n;;;;;;;;;;;;;;;;;;;;;;;"
    liftIO ctxt.solverClose

{- | Close the connection to the SMT solver process and all other resources in  @SMTContext@.
 Using this function means completely stopping the solver with no intention of using it any more.
-}
destroyContext :: LoggerMIO io => SMTContext -> io ()
destroyContext ctxt = do
    logMessage ("Permanently stopping SMT solver" :: Text)
    whenJust ctxt.mbTranscriptHandle $ \h -> liftIO $ do
        BS.hPutStrLn h "; permanently stopping solver\n;;;;;;;;;;;;;;;;;;;;;;;"
        hClose h
    liftIO ctxt.solverClose

connectToSolver :: LoggerMIO io => io (Backend.Solver, Backend.Handle)
connectToSolver = do
    let config = Backend.defaultConfig
    handle <- liftIO $ Backend.new config
    solver <- liftIO $ Backend.initSolver Backend.Queuing $ Backend.toBackend handle
    pure (solver, handle)

newtype SMT m a = SMT (StateT SMTContext m a)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO, LoggerMIO)

runSMT :: SMTContext -> SMT io a -> io (a, SMTContext)
runSMT ctxt (SMT action) = runStateT action ctxt

evalSMT :: Monad io => SMTContext -> SMT io a -> io a
evalSMT ctxt (SMT action) = evalStateT action ctxt

declare :: LoggerMIO io => [DeclareCommand] -> SMT io ()
declare = mapM_ runCmd

class SMTEncode cmd where
    encode :: cmd -> BS.Builder

    comment :: cmd -> Maybe BS.Builder
    comment _ = Nothing

    -- selecting the actual runner (command_ for Declare and Control, command for query)
    run_ ::
        LoggerMIO io =>
        cmd ->
        Backend.Solver ->
        BS.Builder ->
        SMT io BS.ByteString

runCmd_ :: (SMTEncode cmd, LoggerMIO io) => cmd -> SMT io ()
runCmd_ = void . runCmd

runCmd :: forall cmd io. (SMTEncode cmd, LoggerMIO io) => cmd -> SMT io Response
runCmd cmd = do
    let cmdBS = encode cmd
    ctxt <- SMT get
    whenJust ctxt.mbTranscriptHandle $ \h -> do
        whenJust (comment cmd) $ \c ->
            liftIO (BS.hPutBuilder h c)
        liftIO (BS.hPutBuilder h $ cmdBS <> "\n")
    output <- run_ cmd ctxt.solver cmdBS
    let result = readResponse output
    whenJust ctxt.mbTranscriptHandle $
        liftIO . flip BS.hPutStrLn (BS.pack $ "; " <> show output <> ", parsed as " <> show result <> "\n")
    when (isError result) $
        logMessage $
            "SMT solver reports: " <> pack (show result)
    pure result
  where
    isError :: Response -> Bool
    isError Error{} = True
    isError _other = False

instance SMTEncode DeclareCommand where
    encode = encodeDeclaration

    comment cmd =
        case getComment cmd of
            "" -> Nothing
            bs ->
                Just
                    . foldl1 (<>)
                    . map (\b -> "; " <> BS.byteString b <> "\n")
                    $ BS.lines bs

    run_ _ s = fmap (const "success") . liftIO . Backend.command_ s

instance SMTEncode QueryCommand where
    encode = encodeQuery

    run_ _ s = fmap BS.toStrict . liftIO . Backend.command s

instance SMTEncode ControlCommand where
    encode Push = "(push)"
    encode Pop = "(pop)"
    encode (SetTimeout n)
        | n > 0 = "(set-option :timeout " <> BS.string7 (show n) <> ")"
        | otherwise = error $ "Illegal SMT timeout value " <> show n
    encode Exit = "(exit)"

    comment _ = Just ";;;;;;;\n"

    run_ _ s = fmap (const "success") . liftIO . Backend.command_ s

instance SMTEncode SMTCommand where
    encode (Query q) = encode q
    encode (Declare d) = encode d
    encode (Control c) = encode c

    run_ (Query q) = run_ q
    run_ (Declare d) = run_ d
    run_ (Control c) = run_ c

-- typical interaction function for checking satisfiability
runCheck :: LoggerMIO io => [DeclareCommand] -> SMT io Response
runCheck decls =
    void (runCmd Push)
        >> mapM_ runCmd decls
        >> runCmd CheckSat
            <* runCmd Pop
