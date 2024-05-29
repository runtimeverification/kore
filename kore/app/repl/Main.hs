{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- Added for outputFileName

module Main (main) where

import Control.Concurrent.MVar
import Control.Monad.Catch (
    SomeException,
    fromException,
    handle,
    throwM,
 )
import Control.Monad.Extra (whenJust)
import GlobalMain
import Kore.BugReport
import Kore.Exec (
    proveWithRepl,
 )
import Kore.IndexedModule.MetadataToolsBuilder qualified as MetadataTools (
    build,
 )
import Kore.Log (
    KoreLogOptions (..),
    SomeEntry (..),
    logEntry,
    runLoggerT,
    swappableLogger,
    withLoggerLegacy,
 )
import Kore.Log.ErrorException (
    errorException,
 )
import Kore.Log.KoreLogOptions (
    parseKoreLogOptions,
 )
import Kore.Log.WarnIfLowProductivity (
    warnIfLowProductivity,
 )
import Kore.Reachability.Claim qualified as Claim
import Kore.Repl.Data
import Kore.Rewrite.SMT.Lemma
import Kore.Rewrite.Timeout (
    EnableMovingAverage,
    StepTimeout,
 )
import Kore.Syntax.Module (
    ModuleName (..),
 )
import Kore.Unparser (
    unparseToString,
 )
import Options.Applicative (
    InfoMod,
    Parser,
    argument,
    flag,
    fullDesc,
    header,
    help,
    long,
    metavar,
    progDesc,
    short,
    showDefault,
    str,
    strOption,
    value,
 )
import Options.SMT (
    KoreSolverOptions (..),
    parseKoreSolverOptions,
 )
import Prelude.Kore
import SMT qualified
import System.Clock (
    Clock (Monotonic),
    TimeSpec,
    getTime,
 )
import System.Exit (
    exitFailure,
    exitWith,
 )
import System.FilePath (
    takeDirectory,
 )
import System.IO (
    hPutStrLn,
    stderr,
 )

-- | Represents a file name along with its module name passed.
data KoreModule = KoreModule
    { fileName :: !FilePath
    , moduleName :: !ModuleName
    }

-- | Options for the kore repl.
data KoreReplOptions = KoreReplOptions
    { definitionModule :: !KoreModule
    , proveOptions :: !KoreProveOptions
    , smtOptions :: !KoreSolverOptions
    , korePrintCommand :: !KorePrintCommand
    , replMode :: !ReplMode
    , stepTime :: StepTime
    , scriptModeOutput :: !ScriptModeOutput
    , replScript :: !ReplScript
    , outputFile :: !OutputFile
    , koreLogOptions :: !KoreLogOptions
    , bugReportOption :: !BugReportOption
    }

-- | Parse options after being given the value of startTime for KoreLogOptions
parseKoreReplOptions :: TimeSpec -> Parser KoreReplOptions
parseKoreReplOptions startTime =
    KoreReplOptions
        <$> parseMainModule
        <*> parseKoreProveOptions
        <*> parseKoreSolverOptions
        <*> parseKorePrintCommand
        <*> parseReplMode
        <*> parseShowStepTime
        <*> parseScriptModeOutput
        <*> parseReplScript
        <*> parseOutputFile
        <*> parseKoreLogOptions (ExeName "kore-repl") startTime
        <*> parseBugReportOption
  where
    parseMainModule :: Parser KoreModule
    parseMainModule =
        KoreModule
            <$> argument
                str
                ( metavar "MAIN_DEFINITION_FILE"
                    <> help "Kore definition file to verify and use for execution"
                )
            <*> GlobalMain.parseModuleName
                "MAIN_MODULE"
                "module"
                "Kore main module name."

    parseReplMode :: Parser ReplMode
    parseReplMode =
        flag
            Interactive
            RunScript
            ( long "run-mode"
                <> short 'r'
                <> help "Repl run script mode"
            )

    parseScriptModeOutput :: Parser ScriptModeOutput
    parseScriptModeOutput =
        flag
            DisableOutput
            EnableOutput
            ( long "save-run-output"
                <> help "Get output in run mode."
            )

    parseReplScript :: Parser ReplScript
    parseReplScript =
        ReplScript
            <$> optional
                ( strOption
                    ( metavar "REPL_SCRIPT"
                        <> long "repl-script"
                        <> help "Path to the repl script file"
                    )
                )

    parseOutputFile :: Parser OutputFile
    parseOutputFile =
        OutputFile
            <$> optional
                ( strOption
                    ( metavar "PATTERN_OUTPUT_FILE"
                        <> long "output"
                        <> help "Output file to contain final Kore pattern."
                    )
                )

    parseKorePrintCommand :: Parser KorePrintCommand
    parseKorePrintCommand =
        KorePrintCommand
            <$> ( strOption
                    ( metavar "EXEC_FILE"
                        <> long "kore-print-command"
                        <> help "Command to run the kore-print pretty printer."
                        <> value "kore-print"
                        <> showDefault
                    )
                )

    parseShowStepTime :: Parser StepTime
    parseShowStepTime =
        flag
            DisableStepTime
            EnableStepTime
            ( long "show-step-time"
                <> help "Print the time taken by steps."
            )

parserInfoModifiers :: InfoMod options
parserInfoModifiers =
    fullDesc
        <> progDesc "REPL debugger for proofs"
        <> header "kore-repl - a repl for Kore proofs"

exeName :: ExeName
exeName = ExeName "kore-repl"

-- | Environment variable name for extra arguments
envName :: String
envName = "KORE_REPL_OPTS"

main :: IO ()
main = do
    startTime <- getTime Monotonic
    options <-
        mainGlobal
            Main.exeName
            (Just envName)
            (parseKoreReplOptions startTime)
            parserInfoModifiers
    whenJust (localOptions options) mainWithOptions

mainWithOptions :: LocalOptions KoreReplOptions -> IO ()
mainWithOptions LocalOptions{execOptions} = do
    exitCode <-
        withBugReport Main.exeName bugReportOption $ \tempDirectory ->
            withLoggerLegacy tempDirectory koreLogOptions $ \actualLogAction -> do
                mvarLogAction <- newMVar actualLogAction
                let swapLogAction = swappableLogger mvarLogAction
                flip runLoggerT swapLogAction $
                    runExceptionHandlers $ do
                        definition <-
                            loadDefinitions [definitionFileName, specFile]
                        indexedModule <- loadModule mainModuleName definition
                        specDefIndexedModule <- loadModule specModule definition
                        let validatedDefinition =
                                addExtraAxioms indexedModule specDefIndexedModule

                        let smtConfig =
                                SMT.z3Config
                                    { SMT.timeOut = smtTimeOut
                                    , SMT.rLimit = smtRLimit
                                    , SMT.resetInterval = smtResetInterval
                                    , SMT.prelude = smtPrelude
                                    }

                        when
                            ( replMode == RunScript
                                && isNothing (unReplScript replScript)
                            )
                            $ lift
                            $ do
                                hPutStrLn
                                    stderr
                                    "You must supply the path to the repl script\
                                    \ in order to run the repl in run-script mode."
                                exitFailure

                        when
                            ( replMode == Interactive
                                && scriptModeOutput == EnableOutput
                            )
                            $ lift
                            $ do
                                hPutStrLn
                                    stderr
                                    "The --save-run-output flag is only available\
                                    \ when running the repl in run-script mode."
                                exitFailure

                        SMT.runSMT
                            smtConfig
                            ( declareSMTLemmas
                                (MetadataTools.build validatedDefinition)
                                (getSMTLemmas validatedDefinition)
                            )
                            $ proveWithRepl
                                replMinDepth
                                replStepTimeout
                                replEnableMovingAverage
                                stepTime
                                replStuckCheck
                                replAllowVacuous
                                validatedDefinition
                                specDefIndexedModule
                                Nothing
                                mvarLogAction
                                replScript
                                replMode
                                scriptModeOutput
                                outputFile
                                mainModuleName
                                koreLogOptions
                                (GlobalMain.kFileLocations definition)
                                kompiledDir
                                korePrintCommand

                        warnIfLowProductivity
                            (GlobalMain.kFileLocations definition)
                        pure ExitSuccess
    exitWith exitCode
  where
    KoreReplOptions
        { definitionModule
        , proveOptions
        , smtOptions
        , replScript
        , replMode
        , scriptModeOutput
        , outputFile
        , koreLogOptions
        , bugReportOption
        , korePrintCommand
        , stepTime
        } = execOptions
    runExceptionHandlers action =
        action
            & handle exitReplHandler
            & handle withConfigurationHandler
            & handle someExceptionHandler

    exitReplHandler :: ExitCode -> Main ExitCode
    exitReplHandler = pure

    withConfigurationHandler :: Claim.WithConfiguration -> Main ExitCode
    withConfigurationHandler
        (Claim.WithConfiguration lastConfiguration someException) =
            do
                liftIO $
                    hPutStrLn
                        stderr
                        ("// Last configuration:\n" <> unparseToString lastConfiguration)
                throwM someException

    someExceptionHandler :: SomeException -> Main ExitCode
    someExceptionHandler someException = do
        case fromException someException of
            Just entry@(SomeEntry _ _) -> logEntry entry
            Nothing -> errorException someException
        throwM someException

    mainModuleName :: ModuleName
    mainModuleName = moduleName definitionModule

    definitionFileName :: FilePath
    definitionFileName = fileName definitionModule

    specModule :: ModuleName
    specModule = specMainModule proveOptions

    specFile :: FilePath
    specFile = specFileName proveOptions

    kompiledDir :: KompiledDir
    kompiledDir = KompiledDir . takeDirectory $ fileName definitionModule

    smtTimeOut :: SMT.TimeOut
    smtTimeOut = timeOut smtOptions

    smtRLimit :: SMT.RLimit
    smtRLimit = rLimit smtOptions

    smtResetInterval :: SMT.ResetInterval
    smtResetInterval = resetInterval smtOptions

    smtPrelude :: SMT.Prelude
    smtPrelude = prelude smtOptions

    replStuckCheck :: Claim.StuckCheck
    replStuckCheck = stuckCheck proveOptions

    replMinDepth :: Maybe Claim.MinDepth
    replMinDepth = minDepth proveOptions

    replAllowVacuous :: Claim.AllowVacuous
    replAllowVacuous = allowVacuous proveOptions

    replStepTimeout :: Maybe StepTimeout
    replStepTimeout = GlobalMain.stepTimeout proveOptions

    replEnableMovingAverage :: EnableMovingAverage
    replEnableMovingAverage = GlobalMain.enableMovingAverage proveOptions
