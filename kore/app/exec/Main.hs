module Main (main) where

import Control.Lens qualified as Lens
import Control.Monad.Catch (
    handle,
    throwM,
 )
import Control.Monad.Extra as Monad
import Control.Monad.Validate
import Data.Binary qualified as Binary
import Data.Default (
    def,
 )
import Data.Generics.Product (
    field,
 )
import Data.IORef (writeIORef)
import Data.InternedText (globalInternedTextCache)
import Data.Limit (
    Limit (..),
    maybeLimit,
 )
import Data.List (
    intercalate,
 )
import Data.Proxy
import Data.Set.Internal qualified as Set
import Data.Text (
    unpack,
 )
import GHC.Generics qualified as GHC
import GlobalMain
import Kore.Attribute.Definition (
    KFileLocations (..),
 )
import Kore.Attribute.Symbol as Attribute
import Kore.BugReport
import Kore.Exec
import Kore.IndexedModule.IndexedModule (
    VerifiedModule,
    indexedModuleRawSentences,
 )
import Kore.IndexedModule.MetadataTools (
    SmtMetadataTools,
 )
import Kore.IndexedModule.MetadataToolsBuilder qualified as MetadataTools (
    build,
 )
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import Kore.Internal.MultiAnd qualified as MultiAnd
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.TermLike (
    TermLike,
    VariableName,
    mkSortVariable,
    mkTop,
 )
import Kore.Log (
    DebugOptionsValidationError (..),
    KoreLogOptions (..),
    parseKoreLogOptions,
    runKoreLog,
    unparseKoreLogOptions,
    validateDebugOptions,
 )
import Kore.Log.ErrorException (
    handleSomeException,
 )
import Kore.Log.InfoProofDepth
import Kore.Log.WarnIfLowProductivity (
    warnIfLowProductivity,
 )
import Kore.Log.WarnUnexploredBranches (
    warnUnexploredBranches,
 )
import Kore.Options (
    enableDisableFlag,
 )
import Kore.Parser.ParserUtils (
    readPositiveIntegral,
 )
import Kore.Reachability (
    MinDepth (..),
    ProveClaimsResult (..),
    SomeClaim,
    StuckClaim (..),
    getConfiguration,
    lensClaimPattern,
 )
import Kore.Reachability.Claim qualified as Claim
import Kore.Rewrite
import Kore.Rewrite.ClaimPattern (
    getClaimPatternSort,
 )
import Kore.Rewrite.RewritingVariable
import Kore.Rewrite.SMT.Lemma
import Kore.Rewrite.Search (
    SearchType (..),
 )
import Kore.Rewrite.Search qualified as Search
import Kore.Rewrite.Strategy (
    FinalNodeType (..),
    GraphSearchOrder (..),
 )
import Kore.Rewrite.Timeout (
    EnableMovingAverage (..),
    StepTimeout (..),
 )
import Kore.Syntax.Definition (
    Definition (Definition),
    Module (Module),
    ModuleName (..),
    Sentence (..),
 )
import Kore.Syntax.Definition qualified as Definition.DoNotUse
import Kore.Unparser (
    unparse,
 )
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
    progDesc,
    readerError,
    str,
    strOption,
    value,
 )
import Options.Applicative qualified as Options
import Options.Applicative.Help.Pretty qualified as OptPretty
import Options.SMT (
    KoreSolverOptions (..),
    ensureSmtPreludeExists,
    parseKoreSolverOptions,
    unparseKoreSolverOptions,
    writeKoreSolverFiles,
 )
import Prelude.Kore
import Pretty (
    Doc,
    hPutDoc,
    putDoc,
 )
import Stats
import System.Clock (
    Clock (Monotonic),
    TimeSpec,
    getTime,
 )
import System.Directory (
    copyFile,
    doesFileExist,
    emptyPermissions,
    setOwnerExecutable,
    setOwnerReadable,
    setOwnerSearchable,
    setOwnerWritable,
    setPermissions,
 )
import System.Exit (
    exitWith,
 )
import System.FilePath (
    takeDirectory,
    takeExtension,
    (</>),
 )
import System.IO (
    IOMode (WriteMode),
    withFile,
 )
import Type.Reflection (
    someTypeRep,
 )

{-
Main module to run kore-exec
TODO: add command line argument tab-completion
-}

data KoreSearchOptions = KoreSearchOptions
    { searchFileName :: !FilePath
    -- ^ Name of file containing a pattern to match during execution
    , bound :: !(Limit Natural)
    -- ^ The maximum bound on the number of search matches
    , searchType :: !SearchType
    -- ^ The type of search to perform
    }
    deriving stock (GHC.Generic)

parseKoreSearchOptions :: Parser KoreSearchOptions
parseKoreSearchOptions =
    KoreSearchOptions
        <$> strOption
            ( metavar "SEARCH_FILE"
                <> long "search"
                <> help
                    "Kore source file representing pattern to search for. \
                    \Needs --module."
            )
        <*> parseBound
        <*> parseSearchType
  where
    parseBound = Limit <$> bound <|> pure Unlimited
    bound =
        option
            auto
            ( metavar "BOUND"
                <> long "bound"
                <> help "Maximum number of solutions."
            )
    parseSearchType =
        parseSum
            "SEARCH_TYPE"
            "searchType"
            "Search type (selects potential solutions)"
            (map (\s -> (show s, s)) [ONE, FINAL, STAR, PLUS])

parseSum :: String -> String -> String -> [(String, value)] -> Parser value
parseSum metaName longName helpMsg options =
    option
        (snd <$> readSum longName options)
        ( metavar metaName
            <> long longName
            <> help (helpMsg <> ": " <> knownOptions)
        )
  where
    knownOptions = intercalate ", " (map fst options)

readSum :: String -> [(String, value)] -> Options.ReadM (String, value)
readSum longName options = do
    opt <- str
    case lookup opt options of
        Just val -> pure (opt, val)
        _ -> readerError (unknown opt ++ known)
  where
    knownOptions = intercalate ", " (map fst options)
    unknown opt = "Unknown " ++ longName ++ " '" ++ opt ++ "'. "
    known = "Known " ++ longName ++ "s are: " ++ knownOptions ++ "."

applyKoreSearchOptions ::
    Maybe KoreSearchOptions ->
    KoreExecOptions ->
    KoreExecOptions
applyKoreSearchOptions Nothing koreExecOpts = koreExecOpts
applyKoreSearchOptions koreSearchOptions@(Just koreSearchOpts) koreExecOpts =
    koreExecOpts
        { koreSearchOptions
        , depthLimit = min depthLimit searchTypeDepthLimit
        }
  where
    KoreSearchOptions{searchType} = koreSearchOpts
    KoreExecOptions{depthLimit} = koreExecOpts
    searchTypeDepthLimit =
        case searchType of
            ONE -> Limit 1
            _ -> Unlimited

-- | Main options record
data KoreExecOptions = KoreExecOptions
    { definitionFileName :: !FilePath
    -- ^ Name for a file containing a definition to verify and use for execution
    , patternFileName :: !(Maybe FilePath)
    -- ^ Name for file containing a pattern to verify and use for execution
    , outputFileName :: !(Maybe FilePath)
    -- ^ Name for file to contain the output pattern
    , mainModuleName :: !ModuleName
    -- ^ The name of the main module in the definition
    , breadthLimit :: !(Limit Natural)
    , depthLimit :: !(Limit Natural)
    , strategy :: !ExecutionMode
    , koreSolverOptions :: !KoreSolverOptions
    , koreLogOptions :: !KoreLogOptions
    , koreSearchOptions :: !(Maybe KoreSearchOptions)
    , koreProveOptions :: !(Maybe KoreProveOptions)
    , finalNodeType :: !FinalNodeType
    , rtsStatistics :: !(Maybe FilePath)
    , bugReportOption :: !BugReportOption
    , maxCounterexamples :: !Natural
    , serialize :: !Bool
    }
    deriving stock (GHC.Generic)

-- | Command Line Argument Parser
parseKoreExecOptions :: TimeSpec -> Parser KoreExecOptions
parseKoreExecOptions startTime =
    applyKoreSearchOptions
        <$> optional parseKoreSearchOptions
        <*> parseKoreExecOptions0
  where
    parseKoreExecOptions0 :: Parser KoreExecOptions
    parseKoreExecOptions0 =
        KoreExecOptions
            <$> argument
                str
                ( metavar "DEFINITION_FILE"
                    <> help "Kore definition file to verify and use for execution"
                )
            <*> optional
                ( strOption
                    ( metavar "PATTERN_FILE"
                        <> long "pattern"
                        <> help
                            "Verify and execute the Kore pattern found in PATTERN_FILE."
                    )
                )
            <*> optional
                ( strOption
                    ( metavar "PATTERN_OUTPUT_FILE"
                        <> long "output"
                        <> help "Output file to contain final Kore pattern."
                    )
                )
            <*> parseMainModuleName
            <*> parseBreadthLimit
            <*> parseDepthLimit
            <*> parseStrategy
            <*> parseKoreSolverOptions
            <*> parseKoreLogOptions (ExeName "kore-exec") startTime
            <*> pure Nothing
            <*> optional parseKoreProveOptions
            <*> Options.flag
                Leaf
                LeafOrBranching
                ( long "execute-to-branch"
                    <> help "Execute until the proof branches."
                )
            <*> optional parseRtsStatistics
            <*> parseBugReportOption
            <*> parseMaxCounterexamples
            <*> enableDisableFlag
                "serialize"
                True
                False
                False
                "serialization of initialized definition to disk. [default: disabled]"
    parseMaxCounterexamples = counterexamples <|> pure 1
      where
        counterexamples =
            option
                (readPositiveIntegral id "max-counterexamples")
                ( metavar "MAX_COUNTEREXAMPLES"
                    <> long "max-counterexamples"
                    <> help "Specify the maximum number of counterexamples."
                )
    parseBreadthLimit = Limit <$> breadth <|> pure Unlimited
    parseDepthLimit = Limit <$> depth <|> pure Unlimited
    parseStrategy =
        option
            parseExecutionMode
            ( metavar "STRATEGY"
                <> long "strategy"
                <> value All
                <> help "Select rewrites using STRATEGY."
            )

    breadth =
        option
            auto
            ( metavar "BREADTH"
                <> long "breadth"
                <> help "Allow up to BREADTH parallel execution branches."
            )
    depth =
        option
            auto
            ( metavar "DEPTH"
                <> long "depth"
                <> help "Execute up to DEPTH steps."
            )

    parseMainModuleName =
        GlobalMain.parseModuleName
            "MODULE"
            "module"
            "The name of the main module in the Kore definition."
    parseRtsStatistics =
        strOption (mconcat infos)
      where
        infos =
            [ metavar "FILENAME"
            , long "rts-statistics"
            , help "Write runtime statistics to FILENAME in JSON format."
            ]
    parseExecutionMode = do
        val <- str
        case val :: String of
            "all" -> return All
            "any" -> return Any
            _ ->
                readerError $
                    show $
                        OptPretty.hsep
                            [ "Unknown option"
                            , OptPretty.squotes (OptPretty.text val)
                                <> OptPretty.dot
                            , "Known options are 'all' and 'any'."
                            ]

-- | modifiers for the Command line parser description
parserInfoModifiers :: InfoMod options
parserInfoModifiers =
    fullDesc
        <> progDesc
            "Uses Kore definition in DEFINITION_FILE to execute pattern \
            \in PATTERN_FILE."
        <> header "kore-exec - an interpreter for Kore definitions"

unparseKoreSearchOptions :: KoreSearchOptions -> [String]
unparseKoreSearchOptions (KoreSearchOptions _ bound searchType) =
    [ "--search searchFile.kore"
    , maybeLimit "" (\limit -> unwords ["--bound", show limit]) bound
    , unwords ["--searchType", show searchType]
    ]

unparseKoreProveOptions :: KoreProveOptions -> [String]
unparseKoreProveOptions
    ( KoreProveOptions
            _
            (ModuleName moduleName)
            graphSearch
            saveProofs
            stuckCheck
            minDepth
            allowVacuous
            stepTimeout
            enableMA
        ) =
        [ "--prove spec.kore"
        , unwords ["--spec-module", unpack moduleName]
        , unwords
            [ "--graph-search"
            , if graphSearch == DepthFirst then "depth-first" else "breadth-first"
            ]
        , maybe "" ("--save-proofs " <>) saveProofs
        , case stuckCheck of
            Claim.DisabledStuckCheck -> "--disable-stuck-check"
            _ -> ""
        , maybe "" unparseMinDepth minDepth
        , case allowVacuous of
            Claim.AllowedVacuous -> "--allow-vacuous"
            _ -> ""
        , case stepTimeout of
            Just (StepTimeout st) -> "--set-step-timeout " <> show st
            _ -> ""
        , case enableMA of
            EnableMovingAverage -> "--moving-average"
            _ -> ""
        ]
      where
        unparseMinDepth md =
            unwords ["--min-depth", (show . getMinDepth) md]

koreExecSh :: KoreExecOptions -> String
koreExecSh
    koreExecOptions@( KoreExecOptions
                            _
                            patternFileName
                            outputFileName
                            mainModuleName
                            breadthLimit
                            depthLimit
                            strategy
                            koreSolverOptions
                            koreLogOptions
                            koreSearchOptions
                            koreProveOptions
                            finalNodeType
                            rtsStatistics
                            _
                            maxCounterexamples
                            _
                        ) =
        unlines $
            [ "#!/bin/sh"
            , "exec kore-exec \\"
            ]
                <> fmap (\line -> "    " <> line <> " \\") options
                <> ["    \"$@\""]
      where
        options =
            concat
                [ catMaybes
                    [ pure $ defaultDefinitionFilePath koreExecOptions
                    , patternFileName $> "--pattern pgm.kore"
                    , outputFileName $> "--output result.kore"
                    , pure $ unwords ["--module", unpack (getModuleName mainModuleName)]
                    , (\limit -> unwords ["--breadth", show limit])
                        <$> maybeLimit Nothing Just breadthLimit
                    , (\limit -> unwords ["--depth", show limit])
                        <$> maybeLimit Nothing Just depthLimit
                    , pure $ unwords ["--strategy", unparseExecutionMode strategy]
                    , rtsStatistics
                        $> unwords ["--rts-statistics", defaultRtsStatisticsFilePath]
                    , pure $ unwords ["--max-counterexamples", show maxCounterexamples]
                    ]
                , unparseKoreSolverOptions koreSolverOptions
                , unparseKoreLogOptions koreLogOptions
                , maybe mempty unparseKoreSearchOptions koreSearchOptions
                , maybe mempty unparseKoreProveOptions koreProveOptions
                , ["--execute-to-branch" | finalNodeType == LeafOrBranching]
                ]
        unparseExecutionMode All = "all"
        unparseExecutionMode Any = "any"

defaultDefinitionFilePath :: KoreExecOptions -> FilePath
defaultDefinitionFilePath KoreExecOptions{koreProveOptions}
    | isJust koreProveOptions = "vdefinition.kore"
    | otherwise = "definition.kore"

defaultRtsStatisticsFilePath :: FilePath
defaultRtsStatisticsFilePath = "rts-statistics.json"

writeKoreSearchFiles :: FilePath -> KoreSearchOptions -> IO ()
writeKoreSearchFiles reportFile KoreSearchOptions{searchFileName} =
    copyFile searchFileName $ reportFile <> "/searchFile.kore"

writeKoreProveFiles :: FilePath -> KoreProveOptions -> IO ()
writeKoreProveFiles reportFile koreProveOptions = do
    let KoreProveOptions{specFileName} = koreProveOptions
    copyFile specFileName (reportFile </> "spec.kore")
    let KoreProveOptions{saveProofs} = koreProveOptions
    for_ saveProofs $ \filePath ->
        Monad.whenM
            (doesFileExist filePath)
            (copyFile filePath (reportFile </> "save-proofs.kore"))

writeOptionsAndKoreFiles :: FilePath -> KoreExecOptions -> IO ()
writeOptionsAndKoreFiles
    reportDirectory
    opts@KoreExecOptions
        { definitionFileName
        , patternFileName
        , koreSolverOptions
        , koreSearchOptions
        , koreProveOptions
        } =
        do
            let shellScript = reportDirectory </> "kore-exec.sh"
            writeFile shellScript . koreExecSh $ opts
            let allPermissions =
                    setOwnerReadable True
                        . setOwnerWritable True
                        . setOwnerExecutable True
                        . setOwnerSearchable True
            setPermissions shellScript $ allPermissions emptyPermissions
            let definitionKore = case takeExtension definitionFileName of
                    ".bin" -> takeDirectory definitionFileName </> "definition.kore"
                    _ -> definitionFileName
            copyFile
                definitionKore
                (reportDirectory </> defaultDefinitionFilePath opts)
            for_ patternFileName $
                flip copyFile (reportDirectory </> "pgm.kore")
            writeKoreSolverFiles koreSolverOptions reportDirectory
            for_
                koreSearchOptions
                (writeKoreSearchFiles reportDirectory)
            for_
                koreProveOptions
                (writeKoreProveFiles reportDirectory)

exeName :: ExeName
exeName = ExeName "kore-exec"

-- | Environment variable name for extra arguments
envName :: String
envName = "KORE_EXEC_OPTS"

-- TODO(virgil): Maybe add a regression test for main.

-- | Loads a kore definition file and uses it to execute kore programs
main :: IO ()
main = do
    startTime <- getTime Monotonic
    options <-
        mainGlobal
            Main.exeName
            (Just envName)
            (parseKoreExecOptions startTime)
            parserInfoModifiers
    for_ (localOptions options) mainWithOptions

{- | Use the parsed 'KoreExecOptions' to set up output and logging, then
dispatch the requested command.
-}
mainWithOptions :: LocalOptions KoreExecOptions -> IO ()
mainWithOptions LocalOptions{execOptions} = do
    let KoreExecOptions{koreSolverOptions, bugReportOption, outputFileName} =
            execOptions
    ensureSmtPreludeExists koreSolverOptions
    exitCode <-
        withBugReport Main.exeName bugReportOption $ \tmpDir -> do
            let execOptions' =
                    execOptions
                        { outputFileName = Just (tmpDir </> "result.kore")
                        }
            writeOptionsAndKoreFiles tmpDir execOptions'
            e <-
                mainDispatch execOptions'
                    & handle handleWithConfiguration
                    & handle handleSomeException
                    & runKoreLog
                        tmpDir
                        (branchingDepth koreLogOptions)
            case outputFileName of
                Nothing -> readFile (tmpDir </> "result.kore") >>= putStr
                Just fileName -> copyFile (tmpDir </> "result.kore") fileName
            return e
    let KoreExecOptions{rtsStatistics} = execOptions
    for_ rtsStatistics $ \filePath ->
        writeStats filePath =<< getStats
    exitWith exitCode
  where
    KoreExecOptions{koreLogOptions} = execOptions

    -- Display the proof's depth if the flag '--execute-to-branch' was given
    branchingDepth :: KoreLogOptions -> KoreLogOptions
    branchingDepth logOpts
        | LeafOrBranching <-
            execOptions & Lens.view (field @"finalNodeType") =
            logOpts
                & Lens.over
                    (field @"logEntries")
                    (Set.insert (someTypeRep $ Proxy @InfoProofDepth))
    branchingDepth logOpts = logOpts

    handleWithConfiguration :: Claim.WithConfiguration -> Main ExitCode
    handleWithConfiguration
        (Claim.WithConfiguration lastConfiguration someException) =
            do
                liftIO $
                    renderResult
                        execOptions
                        ("// Last configuration:\n" <> unparse lastConfiguration)
                throwM someException

-- | Dispatch the requested command, for example 'koreProve' or 'koreRun'.
mainDispatch :: KoreExecOptions -> Main ExitCode
mainDispatch = warnProductivity . mainDispatchWorker
  where
    warnProductivity :: Main (KFileLocations, ExitCode) -> Main ExitCode
    warnProductivity action = do
        (kFileLocations, exitCode) <- action
        warnIfLowProductivity kFileLocations
        return exitCode
    mainDispatchWorker ::
        KoreExecOptions ->
        Main (KFileLocations, ExitCode)
    mainDispatchWorker execOptions
        | isJust koreProveOptions = koreProve execOptions
        | isJust koreSearchOptions = koreSearch execOptions
        | serialize = koreSerialize execOptions
        | otherwise = koreRun execOptions
      where
        KoreExecOptions
            { koreProveOptions
            , koreSearchOptions
            , serialize
            } = execOptions

koreSearch ::
    KoreExecOptions ->
    Main (KFileLocations, ExitCode)
koreSearch execOptions = do
    let KoreExecOptions
            { koreSearchOptions
            , definitionFileName
            , mainModuleName
            , koreSolverOptions
            , koreLogOptions
            , patternFileName
            , breadthLimit
            , depthLimit
            } = execOptions
    SerializedDefinition{serializedModule, lemmas, locations, internedTextCache} <-
        deserializeDefinition koreSolverOptions definitionFileName mainModuleName
    let SerializedModule
            { verifiedModule
            , metadataTools
            , equations
            , rewrites
            } = serializedModule
        undefinedLabels = runValidate $ validateDebugOptions equations (rewriteRules rewrites) koreLogOptions
    when (isLeft undefinedLabels) $
        throwM . DebugOptionsValidationError $
            fromLeft mempty undefinedLabels
    lift $ writeIORef globalInternedTextCache internedTextCache
    let KoreSearchOptions
            { searchFileName
            , bound
            , searchType
            } = fromMaybe (error "This never should happen") koreSearchOptions
    target <- mainParseSearchPattern metadataTools verifiedModule searchFileName
    initial <- loadPattern metadataTools verifiedModule patternFileName
    let config = Search.Config{bound, searchType}
    final <-
        execute koreSolverOptions metadataTools lemmas $
            search
                depthLimit
                breadthLimit
                serializedModule
                initial
                target
                config
    lift $ renderResult execOptions (unparse final)
    return (locations, ExitSuccess)

koreRun :: KoreExecOptions -> Main (KFileLocations, ExitCode)
koreRun execOptions = do
    let KoreExecOptions
            { definitionFileName
            , mainModuleName
            , koreSolverOptions
            , koreLogOptions
            , breadthLimit
            , depthLimit
            , strategy
            , patternFileName
            , finalNodeType
            } = execOptions
    SerializedDefinition{serializedModule, lemmas, locations, internedTextCache} <-
        deserializeDefinition
            koreSolverOptions
            definitionFileName
            mainModuleName
    let SerializedModule
            { verifiedModule
            , metadataTools
            , equations
            , rewrites
            } = serializedModule
        undefinedLabels = runValidate $ validateDebugOptions equations (rewriteRules rewrites) koreLogOptions
    when (isLeft undefinedLabels) $
        throwM . DebugOptionsValidationError $
            fromLeft mempty undefinedLabels
    lift $ writeIORef globalInternedTextCache internedTextCache
    initial <- loadPattern metadataTools verifiedModule patternFileName
    (exitCode, final) <-
        execute koreSolverOptions metadataTools lemmas $
            exec
                Nothing
                DisableMovingAverage
                depthLimit
                breadthLimit
                finalNodeType
                serializedModule
                strategy
                initial
    lift $ renderResult execOptions (unparse final)
    return (locations, exitCode)

-- kore-exec --serialize calls this function in order to construct the definition to serialize
-- and write it to the output file specified by the user. It is an error to not specify an output
-- file as binary data cannot be displayed on the terminal. We put this functionality in the
-- kore-exec binary because that's where most of the logic it needed in order to function already
-- lived.
koreSerialize ::
    KoreExecOptions ->
    Main (KFileLocations, ExitCode)
koreSerialize execOptions = do
    let KoreExecOptions
            { definitionFileName
            , mainModuleName
            , outputFileName
            , koreSolverOptions
            } = execOptions
    serializedDefinition@SerializedDefinition{locations} <-
        makeSerializedDefinition
            koreSolverOptions
            definitionFileName
            mainModuleName
    case outputFileName of
        Nothing -> return (locations, ExitFailure 1)
        Just outputFile -> liftIO $ do
            Binary.encodeFile outputFile serializedDefinition
            return (locations, ExitSuccess)

koreProve ::
    KoreExecOptions ->
    Main (KFileLocations, ExitCode)
koreProve execOptions = do
    let KoreExecOptions
            { definitionFileName
            , koreProveOptions
            , koreLogOptions
            , mainModuleName
            , maxCounterexamples
            , koreSolverOptions
            , breadthLimit
            , depthLimit
            , finalNodeType
            } = execOptions
        KoreProveOptions
            { specFileName
            , specMainModule
            , saveProofs
            , graphSearch
            , stuckCheck
            , minDepth
            , allowVacuous
            , stepTimeout
            , enableMovingAverage
            } = fromMaybe (error "This never should happen") koreProveOptions
    definition <- loadDefinitions [definitionFileName, specFileName]
    mainModule <- loadModule mainModuleName definition
    specModule <- loadModule specMainModule definition
    let mainModule' = addExtraAxioms mainModule specModule
    maybeAlreadyProvenModule <- loadProven definitionFileName saveProofs
    proveResult <- execute koreSolverOptions (MetadataTools.build mainModule) (getSMTLemmas mainModule) $ do
        prove
            koreLogOptions
            minDepth
            stepTimeout
            enableMovingAverage
            stuckCheck
            allowVacuous
            graphSearch
            breadthLimit
            depthLimit
            maxCounterexamples
            finalNodeType
            mainModule'
            specModule
            maybeAlreadyProvenModule

    let ProveClaimsResult{stuckClaims, provenClaims, unexplored} = proveResult
    let (exitCode, final) =
            case foldFirst stuckClaims of
                Nothing -> success -- stuckClaims is empty
                Just claim ->
                    stuckPatterns
                        & OrPattern.toTermLike (getClaimPatternSort $ claimPattern claim)
                        & failure
                  where
                    stuckPatterns =
                        OrPattern.fromPatterns (MultiAnd.map getStuckConfig stuckClaims)
                    getStuckConfig =
                        getRewritingPattern . getConfiguration . getStuckClaim
                    claimPattern = Lens.view lensClaimPattern . getStuckClaim

    lift $ for_ saveProofs $ saveProven specModule provenClaims
    lift $ renderResult execOptions (unparse final)
    when (unexplored /= 0) $
        warnUnexploredBranches unexplored
    return (kFileLocations definition, exitCode)
  where
    failure pat = (ExitFailure 1, pat)
    success :: (ExitCode, TermLike VariableName)
    success = (ExitSuccess, mkTop $ mkSortVariable "R")

    loadProven ::
        FilePath ->
        Maybe FilePath ->
        Main (Maybe (VerifiedModule StepperAttributes))
    loadProven _ Nothing = return Nothing
    loadProven definitionFileName (Just saveProofsFileName) = do
        fileExists <- lift $ doesFileExist saveProofsFileName
        if fileExists
            then do
                savedProofsDefinition <-
                    loadDefinitions [definitionFileName, saveProofsFileName]
                savedProofsModule <-
                    loadModule savedProofsModuleName savedProofsDefinition
                return (Just savedProofsModule)
            else return Nothing

    saveProven ::
        VerifiedModule StepperAttributes ->
        MultiAnd SomeClaim ->
        FilePath ->
        IO ()
    saveProven specModule (toList -> provenClaims) outputFile =
        withFile
            outputFile
            WriteMode
            (`hPutDoc` unparse provenDefinition)
      where
        specModuleDefinitions :: [Sentence (TermLike VariableName)]
        specModuleDefinitions =
            filter isNotAxiomOrClaim (indexedModuleRawSentences specModule)

        isNotAxiomOrClaim :: Sentence patternType -> Bool
        isNotAxiomOrClaim (SentenceAxiomSentence _) = False
        isNotAxiomOrClaim (SentenceClaimSentence _) = False
        isNotAxiomOrClaim (SentenceAliasSentence _) = True
        isNotAxiomOrClaim (SentenceSymbolSentence _) = True
        isNotAxiomOrClaim (SentenceImportSentence _) = True
        isNotAxiomOrClaim (SentenceSortSentence _) = True
        isNotAxiomOrClaim (SentenceHookSentence _) = True

        provenClaimSentences = map (from @SomeClaim @(Sentence _)) provenClaims
        provenModule =
            Module
                { moduleName = savedProofsModuleName
                , moduleSentences =
                    specModuleDefinitions <> provenClaimSentences
                , moduleAttributes = def
                }

        provenDefinition =
            Definition
                { definitionAttributes = def
                , definitionModules = [provenModule]
                }

loadPattern ::
    SmtMetadataTools StepperAttributes ->
    LoadedModuleSyntax ->
    Maybe FilePath ->
    Main (TermLike VariableName)
loadPattern metadataTools mainModule (Just fileName) =
    mainPatternParseAndVerify metadataTools mainModule fileName
loadPattern _ _ Nothing = error "Missing: --pattern PATTERN_FILE"

renderResult :: KoreExecOptions -> Doc ann -> IO ()
renderResult KoreExecOptions{outputFileName} doc =
    case outputFileName of
        Nothing -> putDoc doc
        Just outputFile -> withFile outputFile WriteMode (`hPutDoc` doc)

savedProofsModuleName :: ModuleName
savedProofsModuleName =
    ModuleName
        "haskell-backend-saved-claims-43943e50-f723-47cd-99fd-07104d664c6d"
