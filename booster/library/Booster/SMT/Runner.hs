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
import Data.IORef
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text, pack)
import SMTLIB.Backends qualified as Backend
import SMTLIB.Backends.Process qualified as Backend
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
    , args :: [String]
    }
    deriving (Eq, Show)

defaultSMTOptions :: SMTOptions
defaultSMTOptions =
    SMTOptions
        { transcript = Nothing
        , timeout = 125
        , retryLimit = Just 3
        , tactic = Nothing
        , args = []
        }

data SMTContext = SMTContext
    { options :: SMTOptions
    , -- use IORef here to ensure we only ever retain one pointer to the solver,
      -- otherwise the solverClose action does not actually terminate the solver instance
      mbSolver :: Maybe (IORef Backend.Solver)
    , solverClose :: IORef (IO ())
    , mbTranscriptHandle :: Maybe Handle
    , prelude :: [DeclareCommand]
    , lemmas :: Map SymbolName (Set DeclareCommand)
    }

type SymbolName = BS.ByteString -- replicated from Booster.Pattern.Base

----------------------------------------

mkContext ::
    LoggerMIO io =>
    SMTOptions ->
    [DeclareCommand] ->
    Map SymbolName (Set DeclareCommand) ->
    io SMTContext
mkContext opts prelude lemmas = do
    logMessage ("Starting SMT solver" :: Text)
    (solver', handle) <- connectToSolver opts.args
    solver <- liftIO $ newIORef solver'
    solverClose <- liftIO $ newIORef $ Backend.close handle
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
            { mbSolver = Just solver
            , solverClose
            , mbTranscriptHandle
            , prelude
            , lemmas
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
    liftIO $ join $ readIORef ctxt.solverClose

{- | Close the connection to the SMT solver process and all other resources in  @SMTContext@.
 Using this function means completely stopping the solver with no intention of using it any more.
-}
destroyContext :: LoggerMIO io => SMTContext -> io ()
destroyContext ctxt = do
    logMessage ("Permanently stopping SMT solver" :: Text)
    whenJust ctxt.mbTranscriptHandle $ \h -> liftIO $ do
        BS.hPutStrLn h "; permanently stopping solver\n;;;;;;;;;;;;;;;;;;;;;;;"
        hClose h
    liftIO $ join $ readIORef ctxt.solverClose

connectToSolver :: LoggerMIO io => [String] -> io (Backend.Solver, Backend.Handle)
connectToSolver args = do
    let config = Backend.defaultConfig{Backend.args = args <> Backend.defaultConfig.args}
    handle <- liftIO $ Backend.new config
    solver <- liftIO $ Backend.initSolver Backend.Queuing $ Backend.toBackend handle
    pure (solver, handle)

newtype SMT m a = SMT (StateT SMTContext m a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadLogger
        , MonadLoggerIO
        , LoggerMIO
        )

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
runCmd c =
    fmap (fromMaybe "Could not determine reason when running GetReasonUnknown") <$> runCmdMaybe c
  where
    runCmdMaybe :: forall cmd'. SMTEncode cmd' => cmd' -> SMT io ResponseUnresolved
    runCmdMaybe cmd = do
        let cmdBS = encode cmd
        ctxt <- SMT get
        case ctxt.mbSolver of
            Nothing -> pure $ Unknown (Just "server started without SMT solver")
            Just solverRef -> do
                whenJust ctxt.mbTranscriptHandle $ \h -> do
                    whenJust (comment cmd) $ \c' ->
                        liftIO (BS.hPutBuilder h c')
                    liftIO (BS.hPutBuilder h $ cmdBS <> "\n")
                output <- (liftIO $ readIORef solverRef) >>= \solver -> run_ cmd solver cmdBS
                let result = readResponse output
                whenJust ctxt.mbTranscriptHandle $
                    liftIO . flip BS.hPutStrLn (BS.pack $ "; " <> show output <> ", parsed as " <> show result <> "\n")
                case result of
                    Error{} -> do
                        logMessage $
                            "SMT solver reports: " <> pack (show result)
                        pure result
                    Unknown Nothing ->
                        runCmdMaybe GetReasonUnknown >>= \case
                            unknownWithReason@(Unknown (Just _)) -> pure unknownWithReason
                            _ -> pure result
                    _ -> pure result

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
