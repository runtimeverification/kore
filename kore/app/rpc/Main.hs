{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Concurrent.MVar as MVar
import Control.Exception (AsyncException (..))
import Control.Monad.Catch (
    bracket,
    handle,
    handleJust,
 )
import Control.Monad.Reader (
    ReaderT (..),
 )
import Data.IORef (writeIORef)
import Data.InternedText (globalInternedTextCache)
import Data.Map.Strict as Map
import GlobalMain qualified
import Kore.Attribute.Symbol (StepperAttributes)
import Kore.BugReport (
    BugReportOption,
    ExitCode,
    parseBugReportOption,
    withBugReport,
 )
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Internal.TermLike (TermLike)
import Kore.JsonRpc (
    ServerState (..),
    runServer,
 )
import Kore.Log (
    KoreLogOptions (..),
    LoggerT,
    logInfo,
    parseKoreLogOptions,
    runKoreLogThreadSafeLegacy,
 )
import Kore.Log.ErrorException (
    handleSomeException,
 )
import Kore.Rewrite.SMT.Lemma (declareSMTLemmas)
import Kore.Syntax (VariableName)
import Kore.Syntax.Definition (
    ModuleName (..),
 )
import Kore.Syntax.Sentence (SentenceAxiom)
import Log qualified
import Options.Applicative (
    InfoMod,
    Parser,
    argument,
    auto,
    fullDesc,
    header,
    help,
    long,
    metavar,
    option,
    str,
 )
import Options.SMT (
    KoreSolverOptions (..),
    ensureSmtPreludeExists,
    parseKoreSolverOptions,
    Solver(..),
 )
import Prelude.Kore
import SMT qualified
import System.Clock (
    Clock (Monotonic),
    TimeSpec,
    getTime,
 )
import System.Exit (
    ExitCode (ExitSuccess),
    exitWith,
 )

data KoreRpcServerOptions = KoreRpcServerOptions
    { definitionFileName :: !FilePath
    , mainModuleName :: !ModuleName
    , koreSolverOptions :: !KoreSolverOptions
    , koreLogOptions :: !KoreLogOptions
    , bugReportOption :: !BugReportOption
    , port :: !Int
    }

-- | Command Line Argument Parser
parseKoreRpcServerOptions :: TimeSpec -> Parser KoreRpcServerOptions
parseKoreRpcServerOptions startTime =
    KoreRpcServerOptions
        <$> argument
            str
            ( metavar "DEFINITION_FILE"
                <> help "Kore definition file to verify and use for execution"
            )
        <*> parseMainModuleName
        <*> parseKoreSolverOptions
        <*> parseKoreLogOptions Main.exeName startTime
        <*> parseBugReportOption
        <*> option
            auto
            ( metavar "SERVER_PORT"
                <> long "server-port"
                <> help "Port for the RPC server to bind to"
            )
  where
    parseMainModuleName =
        GlobalMain.parseModuleName
            "MODULE"
            "module"
            "The name of the main module in the Kore definition."

-- | modifiers for the Command line parser description
parserInfoModifiers :: InfoMod options
parserInfoModifiers =
    fullDesc
        <> header "kore-rpc - a JSON RPC server for symbolically executing Kore definitions"

exeName :: GlobalMain.ExeName
exeName = GlobalMain.ExeName "kore-rpc"

-- | Environment variable name for extra arguments
envName :: String
envName = "KORE_RPC_OPTS"

main :: IO ()
main = do
    startTime <- getTime Monotonic
    options <-
        GlobalMain.mainGlobal
            Main.exeName
            (Just envName)
            (parseKoreRpcServerOptions startTime)
            parserInfoModifiers
    for_ (GlobalMain.localOptions options) mainWithOptions

mainWithOptions :: GlobalMain.LocalOptions KoreRpcServerOptions -> IO ()
mainWithOptions
    localOptions@GlobalMain.LocalOptions
        { execOptions = KoreRpcServerOptions{koreSolverOptions, koreLogOptions, bugReportOption}
        } = do
        ensureSmtPreludeExists koreSolverOptions
        exitWith
            =<< withBugReport
                Main.exeName
                bugReportOption
                ( \tmpDir ->
                    koreRpcServerRun localOptions
                        & handleJust isInterrupt handleInterrupt
                        & handle handleSomeException
                        & runKoreLogThreadSafeLegacy
                            tmpDir
                            koreLogOptions
                )
      where
        isInterrupt :: AsyncException -> Maybe ()
        isInterrupt UserInterrupt = Just ()
        isInterrupt _other = Nothing

        handleInterrupt :: () -> LoggerT IO ExitCode
        handleInterrupt () = do
            logInfo "RPC server shutting down"
            pure ExitSuccess

koreRpcServerRun ::
    GlobalMain.LocalOptions KoreRpcServerOptions ->
    GlobalMain.Main ExitCode
koreRpcServerRun GlobalMain.LocalOptions{execOptions} = do
    sd@GlobalMain.SerializedDefinition{internedTextCache} <-
        GlobalMain.deserializeDefinition
            koreSolverOptions
            definitionFileName
            mainModuleName
    lift $ writeIORef globalInternedTextCache internedTextCache

    loadedDefinition <- GlobalMain.loadDefinitions [definitionFileName]
    serverState <-
        lift $
            MVar.newMVar
                ServerState
                    { serializedModules = Map.singleton mainModuleName sd
                    , loadedDefinition
                    , receivedModules = mempty
                    }
    GlobalMain.clockSomethingIO "Executing" $
        -- wrap the call to runServer in the logger monad
        Log.LoggerT $
            ReaderT $
                \loggerEnv -> runServer port serverState mainModuleName (runSMT loggerEnv) loggerEnv

    pure ExitSuccess
  where
    KoreRpcServerOptions{definitionFileName, mainModuleName, koreSolverOptions, port} = execOptions
    KoreSolverOptions{timeOut, rLimit, resetInterval, prelude, solver, tactic} = koreSolverOptions
    smtConfig =
        (if solver == CVC5 then SMT.cvc5Config else SMT.z3Config)
            { SMT.timeOut = timeOut
            , SMT.rLimit = rLimit
            , SMT.resetInterval = resetInterval
            , SMT.prelude = prelude
            , SMT.tactic = tactic
            }
    -- SMT solver with user declared lemmas
    runSMT ::
        forall a.
        Log.LoggerEnv IO ->
        SmtMetadataTools StepperAttributes ->
        [SentenceAxiom (TermLike VariableName)] ->
        SMT.SMT a ->
        IO a
    runSMT Log.LoggerEnv{logAction} metadataTools lemmas m =
        flip Log.runLoggerT logAction $
            bracket (SMT.newSolver smtConfig) SMT.stopSolver $ \refSolverHandle -> do
                let userInit = SMT.runWithSolver $ declareSMTLemmas metadataTools lemmas
                    solverSetup =
                        SMT.SolverSetup
                            { userInit
                            , refSolverHandle
                            , config = smtConfig
                            }
                SMT.initSolver solverSetup
                SMT.runWithSolver m solverSetup
