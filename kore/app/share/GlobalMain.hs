{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2022
License     : BSD-3-Clause
-}
module GlobalMain (
    MainOptions (..),
    GlobalOptions (..),
    LocalOptions (..),
    KoreProveOptions (..),
    ExeName (..),
    Main,
    parseKoreProveOptions,
    mainGlobal,
    clockSomething,
    clockSomethingIO,
    mainPatternVerify,
    parseDefinition,
    parseModuleName,
    verifyDefinitionWithBase,
    mainParse,
    lookupMainModule,
    LoadedDefinition (..),
    LoadedModule,
    LoadedModuleSyntax,
    loadDefinitions,
    loadModule,
    mainParseSearchPattern,
    mainPatternParseAndVerify,
    addExtraAxioms,
    execute,
    SerializedDefinition (..),
    deserializeDefinition,
    makeSerializedDefinition,
) where

import Control.DeepSeq (
    deepseq,
 )
import Control.Exception (
    evaluate,
 )
import Control.Lens (
    (%~),
    (<>~),
 )
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Data.Binary qualified as Binary
import Data.Either.Extra (eitherToMaybe)
import Data.Generics.Product (
    field,
 )
import Data.HashMap.Strict (HashMap)
import Data.IORef (readIORef)
import Data.InternedText (InternedText, InternedTextCache, globalInternedTextCache)
import Data.List (
    intercalate,
    nub,
 )
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Text (
    Text,
    pack,
 )
import Data.Text.IO qualified as Text
import Data.Version (
    showVersion,
 )
import GHC.Generics qualified as GHC
import GHC.Stack (
    emptyCallStack,
 )
import Kore.Attribute.Definition (
    KFileLocations (..),
    parseKFileAttributes,
 )
import Kore.Attribute.SourceLocation (
    notDefault,
 )
import Kore.Attribute.Symbol (
    StepperAttributes,
 )
import Kore.Attribute.Symbol qualified as Attribute (
    Symbol,
 )
import Kore.Builtin qualified as Builtin
import Kore.Exec (
    SerializedModule (..),
    makeSerializedModule,
 )
import Kore.IndexedModule.IndexedModule (
    IndexedModule (indexedModuleAxioms),
    VerifiedModule,
    VerifiedModuleSyntax,
 )
import Kore.IndexedModule.MetadataTools (
    SmtMetadataTools,
 )
import Kore.IndexedModule.MetadataToolsBuilder qualified as MetadataTools (
    build,
 )
import Kore.Internal.Conditional (Conditional (..))
import Kore.Internal.Pattern (Pattern)
import Kore.Internal.Predicate (makePredicate)
import Kore.Internal.TermLike (TermLike, pattern And_)
import Kore.Log as Log
import Kore.Log.ErrorParse (
    errorParse,
 )
import Kore.Log.ErrorVerify (
    errorVerify,
 )
import Kore.Parser (
    ParsedPattern,
    parseKoreDefinition,
    parseKorePattern,
 )
import Kore.Parser.ParserUtils (
    readPositiveIntegral,
 )
import Kore.Reachability.Claim (AllowVacuous (..), MinDepth (..), StuckCheck (..))
import Kore.Rewrite.SMT.Lemma
import Kore.Rewrite.Strategy (
    GraphSearchOrder (..),
 )
import Kore.Rewrite.Timeout (
    EnableMovingAverage (..),
    StepTimeout (..),
 )
import Kore.Syntax hiding (Pattern)
import Kore.Syntax.Definition (
    ModuleName (..),
    ParsedDefinition,
    SentenceAxiom,
    definitionAttributes,
    getModuleNameForError,
 )
import Kore.Validate.DefinitionVerifier (
    sortModuleClaims,
    verifyAndIndexDefinitionWithBase,
 )
import Kore.Validate.PatternVerifier as PatternVerifier
import Kore.Verified qualified as Verified
import Kore.VersionInfo
import Options.Applicative (
    InfoMod,
    Parser,
    ParserHelp (..),
    defaultPrefs,
    execParserPure,
    flag,
    handleParseResult,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    overFailure,
    readerError,
    str,
    strOption,
    value,
    (<**>),
 )
import Options.Applicative qualified as Options
import Options.Applicative.Help.Chunk (
    Chunk (..),
    vsepChunks,
 )
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.SMT (
    KoreSolverOptions (..),
    Solver (..),
 )
import Paths_kore qualified as MetaData (
    version,
 )
import Prelude.Kore
import Pretty qualified as KorePretty
import SMT (
    SMT,
 )
import SMT qualified
import System.Clock (
    Clock (Monotonic),
    diffTimeSpec,
    getTime,
 )
import System.Environment qualified as Env

type Main = LoggerT IO

data KoreProveOptions = KoreProveOptions
    { specFileName :: !FilePath
    -- ^ Name of file containing the spec to be proven
    , specMainModule :: !ModuleName
    -- ^ The main module of the spec to be proven
    , graphSearch :: GraphSearchOrder
    -- ^ Search order of the execution graph
    , saveProofs :: !(Maybe FilePath)
    -- ^ The file in which to save the proven claims in case the prover
    -- fails.
    , stuckCheck :: !StuckCheck
    -- ^ Whether to apply the stuck state detection heuristic
    , minDepth :: !(Maybe MinDepth)
    -- ^ Forces the prover to run at least n steps
    , allowVacuous :: AllowVacuous
    -- ^ Enables discharging #Bottom paths as #Top at implication checking time.
    , stepTimeout :: !(Maybe StepTimeout)
    , enableMovingAverage :: !EnableMovingAverage
    }

parseModuleName :: String -> String -> String -> Parser ModuleName
parseModuleName metaName longName helpMsg =
    option
        readModuleName
        ( metavar metaName
            <> long longName
            <> help helpMsg
        )

readModuleName :: Options.ReadM ModuleName
readModuleName = do
    getModuleName <- str
    pure ModuleName{getModuleName}

parseKoreProveOptions :: Parser KoreProveOptions
parseKoreProveOptions =
    KoreProveOptions
        <$> strOption
            ( metavar "SPEC_FILE"
                <> long "prove"
                <> help
                    "Kore source file representing spec to be proven.\
                    \Needs --spec-module."
            )
        <*> parseModuleName
            "SPEC_MODULE"
            "spec-module"
            "The name of the main module in the spec to be proven."
        <*> parseGraphSearch
        <*> optional
            ( strOption
                ( long "save-proofs"
                    <> help
                        "The file in which to save the proven claims \
                        \in case the prover fails."
                )
            )
        <*> Options.flag
            EnabledStuckCheck
            DisabledStuckCheck
            ( long "disable-stuck-check"
                <> help "Disable the heuristic for detecting stuck states."
            )
        <*> optional
            ( option
                parseMinDepth
                ( metavar "MIN_DEPTH"
                    <> long "min-depth"
                    <> help "Force the prover to execute at least n steps."
                )
            )
        <*> Options.flag
            DisallowedVacuous
            AllowedVacuous
            ( long "allow-vacuous"
                <> help
                    "Enables discharging #Bottom paths as #Top at \
                    \implication checking time."
            )
        <*> ( fmap StepTimeout
                <$> Options.optional
                    ( option
                        Options.auto
                        ( metavar "INT"
                            <> long "set-step-timeout"
                            <> help "Set a timeout for one step in seconds."
                        )
                    )
            )
        <*> Options.flag
            DisableMovingAverage
            EnableMovingAverage
            ( long "moving-average"
                <> help "Enable timeout based on moving average."
            )
  where
    parseMinDepth =
        let minDepth = readPositiveIntegral MinDepth "min-depth"
         in minDepth <|> pure (MinDepth 1)
    parseGraphSearch =
        option
            readGraphSearch
            ( metavar "GRAPH_SEARCH"
                <> long "graph-search"
                <> value BreadthFirst
                <> help
                    "Search order of the execution graph. \
                    \Either breadth-first or depth-first. \
                    \Default is breadth-first."
            )
      where
        searchOrders =
            [ ("breadth-first", BreadthFirst)
            , ("depth-first", DepthFirst)
            ]
        readGraphSearch = do
            input <- str
            let found = lookup input searchOrders
            case found of
                Just searchOrder -> pure searchOrder
                Nothing ->
                    let unknown = "Unknown search order '" ++ input ++ "'. "
                        names = intercalate ", " (fst <$> searchOrders)
                        known = "Known search order are: " ++ names
                     in readerError (unknown ++ known)

{- | Record Type containing common command-line arguments for each executable in
the project
-}
data GlobalOptions = GlobalOptions
    { willVersion :: !Bool
    -- ^ Version flag [default=false]
    }

-- | Record type to store all state and options for the subMain operations
data MainOptions a = MainOptions
    { globalOptions :: !GlobalOptions
    , localOptions :: !(Maybe (LocalOptions a))
    }

newtype LocalOptions a = LocalOptions
    { execOptions :: a
    }

{- |
Global main function parses command line arguments, handles global flags
and returns the parsed options
-}
mainGlobal ::
    ExeName ->
    -- | environment variable name for extra arguments
    Maybe String ->
    -- | local options parser
    Parser options ->
    -- | option parser information
    InfoMod (MainOptions options) ->
    IO (MainOptions options)
mainGlobal exeName maybeEnv localOptionsParser modifiers = do
    options <- commandLineParse exeName maybeEnv localOptionsParser modifiers
    when (willVersion $ globalOptions options) mainVersion
    return options

-- | main function to print version information
mainVersion :: IO ()
mainVersion =
    mapM_
        putStrLn
        [ "Kore version " ++ packageVersion
        , "Git:"
        , "  revision:\t" ++ gitHash ++ if gitDirty then " (dirty)" else ""
        , "  branch:\t" ++ fromMaybe "<unknown>" gitBranch
        , "  last commit:\t" ++ gitCommitDate
        ]
  where
    packageVersion = showVersion MetaData.version
    VersionInfo{gitHash, gitDirty, gitBranch, gitCommitDate} = $versionInfo

--------------------
-- Option Parsers --

-- | Global Main argument parser for common options
globalCommandLineParser :: Parser GlobalOptions
globalCommandLineParser =
    GlobalOptions
        <$> flag
            False
            True
            ( long "version"
                <> help "Print version information"
            )

getArgs ::
    -- | environment variable name for extra arguments
    Maybe String ->
    IO ([String], [String])
getArgs maybeEnv = do
    args0 <- Env.getArgs
    args1 <-
        case maybeEnv of
            Nothing -> pure []
            Just env -> words . fromMaybe "" <$> Env.lookupEnv env
    pure (args0, args1)

-- | Run argument parser for local executable
commandLineParse ::
    ExeName ->
    -- | environment variable name for extra arguments
    Maybe String ->
    -- | local options parser
    Parser a ->
    -- | local parser info modifiers
    InfoMod (MainOptions a) ->
    IO (MainOptions a)
commandLineParse (ExeName exeName) maybeEnv parser infoMod = do
    (args, argsEnv) <- getArgs maybeEnv
    let allArgs = args <> argsEnv
        parseResult =
            execParserPure
                defaultPrefs
                (info parseMainOptions infoMod)
                allArgs
        changeHelpOverFailure
            | Just env <- maybeEnv = overFailure (changeHelp args env argsEnv)
            | otherwise = id
    handleParseResult $ changeHelpOverFailure parseResult
  where
    parseLocalOptions =
        LocalOptions
            <$> parser
    parseMainOptions =
        MainOptions
            <$> globalCommandLineParser
            <*> optional parseLocalOptions
            <**> helper

    changeHelp :: [String] -> String -> [String] -> ParserHelp -> ParserHelp
    changeHelp args env argsEnv parserHelp@ParserHelp{helpError} =
        parserHelp
            { helpError =
                vsepChunks [Chunk . Just $ commandWithOpts, helpError]
            }
      where
        commandWithOpts =
            Pretty.vsep
                [ Pretty.linebreak
                , Pretty.pretty (unwords (exeName : args))
                , Pretty.pretty env <> "=" <> Pretty.squotes (Pretty.pretty $ unwords argsEnv)
                ]

----------------------
-- Helper Functions --

-- | Time a pure computation and print results.
clockSomething :: String -> a -> Main a
clockSomething description something =
    clockSomethingIO description (liftIO $ evaluate something)

-- | Time an IO computation and print results.
clockSomethingIO :: String -> Main a -> Main a
clockSomethingIO description something = do
    start <- lift $ getTime Monotonic
    x <- something
    end <- lift $ getTime Monotonic
    logEntry $ logMessage end start
    return x
  where
    logMessage end start =
        mkMessage start end
    mkMessage start end =
        Log.LogMessage
            { message =
                pack $ description ++ " " ++ show (diffTimeSpec end start)
            , severity = Log.Info
            , callstack = emptyCallStack
            }

-- | Verify that a Kore pattern is well-formed and print timing information.
mainPatternVerify ::
    -- | Module containing definitions visible in the pattern
    VerifiedModuleSyntax Attribute.Symbol ->
    -- | Parsed pattern to check well-formedness
    ParsedPattern ->
    Main Verified.Pattern
mainPatternVerify verifiedModule patt = do
    verifyResult <-
        clockSomething
            "Verifying the pattern"
            (runPatternVerifier context $ verifyStandalonePattern Nothing patt)
    either errorVerify return verifyResult
  where
    context =
        PatternVerifier.verifiedModuleContext verifiedModule
            & PatternVerifier.withBuiltinVerifiers Builtin.koreVerifiers

lookupMainModule ::
    Monad monad =>
    ModuleName ->
    Map.Map ModuleName (VerifiedModule Attribute.Symbol) ->
    monad (VerifiedModule Attribute.Symbol)
lookupMainModule name modules =
    case Map.lookup name modules of
        Nothing ->
            error
                ( "The main module, '"
                    ++ getModuleNameForError name
                    ++ "', was not found. Check the --module flag."
                )
        Just m -> return m

{- | Verify the well-formedness of a Kore definition.

Also prints timing information; see 'mainParse'.
-}
verifyDefinitionWithBase ::
    -- | already verified definition
    ( Map.Map ModuleName (VerifiedModule Attribute.Symbol)
    , HashMap InternedText AstLocation
    ) ->
    -- | Parsed definition to check well-formedness
    ParsedDefinition ->
    Main
        ( Map.Map ModuleName (VerifiedModule Attribute.Symbol)
        , HashMap InternedText AstLocation
        )
verifyDefinitionWithBase
    alreadyVerified
    definition =
        do
            verifyResult <-
                clockSomething
                    "Verifying the definition"
                    ( verifyAndIndexDefinitionWithBase
                        alreadyVerified
                        Builtin.koreVerifiers
                        definition
                    )
            either errorVerify return verifyResult

{- | Parse a Kore definition from a filename.

Also prints timing information; see 'mainParse'.
-}
parseDefinition :: FilePath -> Main ParsedDefinition
parseDefinition = mainParse parseKoreDefinition

mainParse ::
    (FilePath -> Text -> Either String a) ->
    String ->
    Main a
mainParse parser fileName = do
    contents <-
        Text.readFile fileName
            & liftIO
            & clockSomethingIO "Reading the input file"
    parseResult <-
        clockSomething "Parsing the file" (parser fileName contents)
    case parseResult of
        Left err -> errorParse err
        Right definition -> return definition

-- | Run the worker in the context of the main module.
execute ::
    forall r.
    KoreSolverOptions ->
    -- | SMT Lemmas
    SmtMetadataTools StepperAttributes ->
    [SentenceAxiom (TermLike VariableName)] ->
    -- | Worker
    SMT r ->
    Main r
execute options metadataTools lemmas worker =
    clockSomethingIO "Executing" $
        case solver of
            Z3 -> withZ3
            None -> withoutSMT
  where
    withZ3 =
        SMT.runSMT
            config
            (declareSMTLemmas metadataTools lemmas)
            worker
    withoutSMT = SMT.runNoSMT worker
    KoreSolverOptions{timeOut, rLimit, resetInterval, prelude, solver} =
        options
    config =
        SMT.defaultConfig
            { SMT.timeOut = timeOut
            , SMT.rLimit = rLimit
            , SMT.resetInterval = resetInterval
            , SMT.prelude = prelude
            }

data SerializedDefinition = SerializedDefinition
    { serializedModule :: SerializedModule
    , lemmas :: [SentenceAxiom (TermLike VariableName)]
    , locations :: KFileLocations
    , internedTextCache :: InternedTextCache
    }
    deriving stock (GHC.Generic)
    deriving anyclass (NFData)
    deriving anyclass (Binary.Binary)

{- | Read a definition from disk, detect if it is a serialized compact region or not,
and either deserialize it, or else treat it as a text KORE definition and manually
construct the needed SerializedDefinition object from it.
-}
deserializeDefinition ::
    KoreSolverOptions ->
    FilePath ->
    ModuleName ->
    Main SerializedDefinition
deserializeDefinition
    solverOptions
    definitionFilePath
    mainModuleName =
        do
            maybeSerializedDefinition <-
                eitherToMaybe <$> liftIO (Binary.decodeFileOrFail definitionFilePath)
            case maybeSerializedDefinition of
                Just serializedDefinition ->
                    return serializedDefinition
                Nothing ->
                    makeSerializedDefinition
                        solverOptions
                        definitionFilePath
                        mainModuleName

makeSerializedDefinition ::
    KoreSolverOptions ->
    FilePath ->
    ModuleName ->
    Main SerializedDefinition
makeSerializedDefinition solverOptions definitionFileName mainModuleName = do
    definition <- loadDefinitions [definitionFileName]
    mainModule <- loadModule mainModuleName definition
    let metadataTools = MetadataTools.build mainModule
    let lemmas = getSMTLemmas mainModule
    serializedModule <-
        execute solverOptions metadataTools lemmas $
            makeSerializedModule mainModule
    let locations = kFileLocations definition
    internedTextCache <- lift $ readIORef globalInternedTextCache
    let serializedDefinition =
            SerializedDefinition
                { serializedModule
                , lemmas
                , locations
                , internedTextCache
                }
    serializedDefinition `deepseq` pure ()
    return serializedDefinition

type LoadedModule = VerifiedModule Attribute.Symbol
type LoadedModuleSyntax = VerifiedModuleSyntax Attribute.Symbol

data LoadedDefinition = LoadedDefinition
    { indexedModules :: Map ModuleName LoadedModule
    , definedNames :: HashMap InternedText AstLocation
    , kFileLocations :: KFileLocations
    }

loadDefinitions :: [FilePath] -> Main LoadedDefinition
loadDefinitions filePaths =
    do
        loadedDefinitions & fmap sortClaims
  where
    loadedDefinitions = do
        parsedDefinitions <- traverse parseDefinition filePaths
        let attributes = fmap definitionAttributes parsedDefinitions
        sources <- traverse parseKFileAttributes attributes
        let sources' = filter notDefault (nub sources)
        (indexedModules, definedNames) <-
            Monad.foldM verifyDefinitionWithBase mempty parsedDefinitions
        return $
            LoadedDefinition
                indexedModules
                definedNames
                (KFileLocations sources')

    sortClaims :: LoadedDefinition -> LoadedDefinition
    sortClaims def@LoadedDefinition{indexedModules} =
        let indexedModules' = indexedModules & Lens.traversed %~ sortModuleClaims
         in def{indexedModules = indexedModules'}

loadModule :: ModuleName -> LoadedDefinition -> Main LoadedModule
loadModule moduleName = lookupMainModule moduleName . indexedModules

mainParseSearchPattern ::
    SmtMetadataTools StepperAttributes ->
    VerifiedModuleSyntax StepperAttributes ->
    String ->
    Main (Pattern VariableName)
mainParseSearchPattern metadataTools indexedModule patternFileName = do
    purePattern <- mainPatternParseAndVerify metadataTools indexedModule patternFileName
    case purePattern of
        And_ _ term predicateTerm ->
            return
                Conditional
                    { term
                    , predicate =
                        either
                            (error . show . KorePretty.pretty)
                            id
                            (makePredicate predicateTerm)
                    , substitution = mempty
                    }
        _ -> error "Unexpected non-conjunctive pattern"

{- | IO action that parses a kore pattern from a filename, verifies it,
 converts it to a pure pattern, and prints timing information.
-}
mainPatternParseAndVerify ::
    SmtMetadataTools StepperAttributes ->
    VerifiedModuleSyntax StepperAttributes ->
    String ->
    Main (TermLike VariableName)
mainPatternParseAndVerify metadataTools indexedModule patternFileName =
    Builtin.internalize metadataTools
        <$> (mainPatternParse patternFileName >>= mainPatternVerify indexedModule)

{- | IO action that parses a kore pattern from a filename and prints timing
 information.
-}
mainPatternParse :: String -> Main ParsedPattern
mainPatternParse = mainParse parseKorePattern

{- | Extract axioms from a module and add them to the main definition module.
 This should be safe, as long as the axioms only depend on sorts/symbols
 defined in the main module.
-}
addExtraAxioms ::
    VerifiedModule StepperAttributes ->
    VerifiedModule StepperAttributes ->
    VerifiedModule StepperAttributes
addExtraAxioms definitionModule moduleWithExtraAxioms =
    definitionModule
        & field @"indexedModuleAxioms"
            <>~ indexedModuleAxioms moduleWithExtraAxioms
