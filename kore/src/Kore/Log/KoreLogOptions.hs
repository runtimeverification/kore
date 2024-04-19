{-# LANGUAGE RecordWildCards #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.KoreLogOptions (
    KoreLogOptions (..),
    KoreLogType (..),
    KoreLogFormat (..),
    EntryTypes,
    ExeName (..),
    TimestampsSwitch (..),
    WarningSwitch (..),
    defaultKoreLogOptions,
    parseKoreLogOptions,
    HasSelected (..),
    DebugApplyEquationOptions (..),
    selectDebugApplyEquation,
    DebugAttemptEquationOptions (..),
    selectDebugAttemptEquation,
    DebugEquationOptions (..),
    selectDebugEquation,
    DebugAttemptRewriteOptions (..),
    selectDebugAttemptRewrite,
    DebugApplyRewriteOptions (..),
    selectDebugApplyRewrite,
    DebugRewriteOptions (..),
    selectDebugRewrite,
    unparseKoreLogOptions,
    defaultSeverity,
    DebugOptionsValidationError (..),
    validateDebugOptions,
    UndefinedLabels (..),
) where

import Control.Exception
import Control.Monad.Validate
import Data.Char qualified as Char
import Data.Default
import Data.HashSet (
    HashSet,
 )
import Data.HashSet qualified as HashSet
import Data.List (
    intercalate,
 )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Equation (Equation)
import Kore.Equation qualified as Equation
import Kore.Equation.DebugEquation (
    DebugApplyEquation (..),
    DebugAttemptEquation (..),
 )
import Kore.Log.DebugAppliedRewriteRules (DebugAppliedLabeledRewriteRule (..))
import Kore.Log.DebugAttemptedRewriteRules (DebugAttemptedRewriteRules (..))
import Kore.Log.DebugSolver (
    DebugSolverOptions (..),
    parseDebugSolverOptions,
 )
import Kore.Log.Registry (
    getEntryTypesAsText,
    getNoErrEntryTypesAsText,
    parseEntryType,
 )
import Kore.Log.SQLite (
    LogSQLiteOptions (..),
    parseLogSQLiteOptions,
 )
import Kore.Rewrite.Axiom.Identifier (AxiomIdentifier)
import Kore.Rewrite.RewritingVariable (RewritingVariableName)
import Kore.Rewrite.RulePattern (
    RewriteRule (..),
 )
import Kore.Syntax.Variable (VariableName)
import Log
import Options.Applicative (
    Parser,
    option,
 )
import Options.Applicative qualified as Options
import Options.Applicative.Help.Pretty qualified as OptPretty
import Prelude.Kore
import Pretty (Pretty (..))
import Pretty qualified
import System.Clock (
    TimeSpec,
 )
import Text.Megaparsec qualified as Parser
import Text.Megaparsec.Char qualified as Parser
import Type.Reflection (
    SomeTypeRep (..),
 )

-- | Command line options for logging.
data KoreLogOptions = KoreLogOptions
    { logType :: !KoreLogType
    -- ^ desired output method, see 'KoreLogType'
    , logFormat :: !KoreLogFormat
    -- ^ the format to display the error log in
    , logLevel :: !Severity
    -- ^ minimal log level, passed via "--log-level"
    , timestampsSwitch :: !TimestampsSwitch
    -- ^ enable or disable timestamps
    , logEntries :: !EntryTypes
    -- ^ extra entries to show, ignoring 'logLevel'
    , debugSolverOptions :: !DebugSolverOptions
    , exeName :: !ExeName
    , startTime :: !TimeSpec
    , logSQLiteOptions :: !LogSQLiteOptions
    , warningSwitch :: !WarningSwitch
    , turnedIntoErrors :: !EntryTypes
    , debugApplyEquationOptions :: !DebugApplyEquationOptions
    , debugAttemptEquationOptions :: !DebugAttemptEquationOptions
    , debugEquationOptions :: !DebugEquationOptions
    , debugAttemptRewriteOptions :: !DebugAttemptRewriteOptions
    , debugApplyRewriteOptions :: !DebugApplyRewriteOptions
    , debugRewriteOptions :: !DebugRewriteOptions
    , rewriteTraceFileName :: !(Maybe FilePath)
    }
    deriving stock (Eq, Show, GHC.Generic)

defaultSeverity :: Severity
defaultSeverity = Warning

defaultKoreLogOptions :: ExeName -> TimeSpec -> KoreLogOptions
defaultKoreLogOptions exeName startTime =
    KoreLogOptions
        { logType = def @KoreLogType
        , logFormat = def @KoreLogFormat
        , logLevel = defaultSeverity
        , timestampsSwitch = def @TimestampsSwitch
        , logEntries = mempty
        , debugSolverOptions = def @DebugSolverOptions
        , exeName
        , startTime
        , logSQLiteOptions = def @LogSQLiteOptions
        , warningSwitch = def @WarningSwitch
        , turnedIntoErrors = mempty
        , debugApplyEquationOptions = def @DebugApplyEquationOptions
        , debugAttemptEquationOptions = def @DebugAttemptEquationOptions
        , debugEquationOptions = def @DebugEquationOptions
        , debugAttemptRewriteOptions = def @DebugAttemptRewriteOptions
        , debugApplyRewriteOptions = def @DebugApplyRewriteOptions
        , debugRewriteOptions = def @DebugRewriteOptions
        , rewriteTraceFileName = Nothing
        }

{- | 'KoreLogType' is passed via command line arguments and decides if and how
 the logger will operate.
-}
data KoreLogType
    = -- | Log to stderr
      LogStdErr
    | -- | Log to specified file when '--log <filename>' is passed.
      LogFileText FilePath
    | LogSomeAction (forall m. MonadIO m => LogAction m Text)

instance Eq KoreLogType where
    LogStdErr == LogStdErr = True
    LogFileText t1 == LogFileText t2 = t1 == t2
    _ == _ = False

instance Show KoreLogType where
    show = \case
        LogStdErr -> "LogStdErr"
        LogFileText fp -> "LogFileText " <> show fp
        LogSomeAction _ -> "LogSomeAction _"

data KoreLogFormat
    = Standard
    | OneLine
    | Json
    deriving stock (Eq, Show)

instance Default KoreLogType where
    def = LogStdErr

instance Default KoreLogFormat where
    def = Standard

parseKoreLogType :: Parser KoreLogType
parseKoreLogType =
    LogFileText <$> Options.strOption info
  where
    info =
        mempty
            <> Options.long "log"
            <> Options.help "Name of the log file"

parseKoreLogFormat :: Parser KoreLogFormat
parseKoreLogFormat = option formatReader info
  where
    formatReader = Options.maybeReader $ \case
        "standard" -> Just Standard
        "oneline" -> Just OneLine
        "json" -> Just Json
        _ -> Nothing
    info =
        mempty
            <> Options.long "log-format"
            <> Options.help "Log format: standard, oneline, json"
            <> Options.value def

type EntryTypes = Set SomeTypeRep

-- | Enable or disable timestamps
data TimestampsSwitch = TimestampsEnable | TimestampsDisable
    deriving stock (Eq, Show)

instance Default TimestampsSwitch where
    def = TimestampsEnable

parseTimestampsSwitch :: Parser TimestampsSwitch
parseTimestampsSwitch =
    parseTimestampsEnable <|> parseTimestampsDisable
  where
    parseTimestampsEnable =
        let info =
                mempty
                    <> Options.long "enable-log-timestamps"
                    <> Options.help "Enable log timestamps"
         in Options.flag' TimestampsEnable info
    parseTimestampsDisable =
        let info =
                mempty
                    <> Options.long "disable-log-timestamps"
                    <> Options.help "Disable log timestamps"
         in Options.flag' TimestampsDisable info

-- | Parse 'KoreLogOptions'.
parseKoreLogOptions :: ExeName -> TimeSpec -> Parser KoreLogOptions
parseKoreLogOptions exeName startTime =
    KoreLogOptions
        <$> (parseKoreLogType <|> pure def)
        <*> (parseKoreLogFormat <|> pure def)
        <*> (parseSeverity <|> pure Warning)
        <*> (parseTimestampsSwitch <|> pure TimestampsEnable)
        <*> (mconcat <$> many parseEntryTypes)
        <*> parseDebugSolverOptions
        <*> pure exeName
        <*> pure startTime
        <*> parseLogSQLiteOptions
        <*> parseWarningSwitch
        <*> (mconcat <$> many parseErrorEntries)
        <*> parseDebugApplyEquationOptions
        <*> parseDebugAttemptEquationOptions
        <*> parseDebugEquationOptions
        <*> parseDebugAttemptRewriteOptions
        <*> parseDebugApplyRewriteOptions
        <*> parseDebugRewriteOptions
        <*> parseTraceRewrites

parseEntryTypes :: Parser EntryTypes
parseEntryTypes =
    option parseCommaSeparatedEntries info
  where
    info =
        mempty
            <> Options.long "log-entries"
            <> Options.helpDoc (Just allEntryTypes)

    allEntryTypes :: OptPretty.Doc
    allEntryTypes =
        OptPretty.vsep
            [ "Log entries: comma-separated list logs entries to enable"
            , "Available entry types:"
            , (OptPretty.indent 4 . OptPretty.vsep)
                (Pretty.pretty <$> getEntryTypesAsText)
            ]

parseCommaSeparatedEntries :: Options.ReadM EntryTypes
parseCommaSeparatedEntries =
    Options.maybeReader $ Parser.parseMaybe parseEntryTypes' . Text.pack
  where
    parseEntryTypes' :: Parser.Parsec String Text EntryTypes
    parseEntryTypes' = Set.fromList <$> Parser.sepEndBy parseSomeTypeRep comma

    comma = void (Parser.char ',')

    parseSomeTypeRep :: Parser.Parsec String Text SomeTypeRep
    parseSomeTypeRep =
        Parser.takeWhile1P (Just "SomeTypeRep") (flip notElem [',', ' '])
            >>= parseEntryType

parseSeverity :: Parser Severity
parseSeverity =
    option (Options.maybeReader readSeverity) info
  where
    info =
        mempty
            <> Options.long "log-level"
            <> Options.help "Log level: debug, info, warning, error, or critical"

readSeverity :: String -> Maybe Severity
readSeverity =
    \case
        "debug" -> pure Debug
        "info" -> pure Info
        "warning" -> pure Warning
        "error" -> pure Error
        _ -> Nothing

parseWarningSwitch :: Parser WarningSwitch
parseWarningSwitch =
    Options.flag
        AsWarning
        AsError
        ( Options.long "warnings-to-errors"
            <> Options.help "Turn warnings into errors"
        )

parseErrorEntries :: Parser EntryTypes
parseErrorEntries =
    option parseCommaSeparatedEntries info
  where
    info =
        mempty
            <> Options.long "error-entries"
            <> Options.helpDoc (Just nonErrorEntryTypes)

    nonErrorEntryTypes :: OptPretty.Doc
    nonErrorEntryTypes =
        OptPretty.vsep
            [ "Turn arbitrary log entries into errors"
            , "Available entry types:"
            , (OptPretty.indent 4 . OptPretty.vsep)
                (Pretty.pretty <$> getNoErrEntryTypesAsText)
                {- The user can still give error entries as arguments, but it's
                    useless, so we don't list them here
                -}
            ]

-- | Caller of the logging function
newtype ExeName = ExeName {getExeName :: String}
    deriving stock (Eq, Show)

instance Pretty.Pretty ExeName where
    pretty = Pretty.pretty . getExeName

data WarningSwitch = AsWarning | AsError
    deriving stock (Eq, Show)

instance Default WarningSwitch where
    def = AsWarning

class HasSelected a where
    selected :: a -> HashSet Text

newtype DebugApplyEquationOptions = DebugApplyEquationOptions (HashSet Text)
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid)

instance Default DebugApplyEquationOptions where
    def = DebugApplyEquationOptions HashSet.empty

instance HasSelected DebugApplyEquationOptions where
    selected (DebugApplyEquationOptions s) = s

parseDebugApplyEquationOptions :: Parser DebugApplyEquationOptions
parseDebugApplyEquationOptions =
    mconcat <$> many parse
  where
    parse =
        fmap (DebugApplyEquationOptions . HashSet.singleton) $
            Options.strOption $
                mconcat
                    [ Options.metavar "EQUATION_IDENTIFIER"
                    , Options.long "debug-apply-equation"
                    , Options.help
                        "Log every succesfully applied equation that matches EQUATION_IDENTIFIER, \
                        \which may be the name of a symbol or a rule. \
                        \The name of a symbol may be a Kore identifier or a K label, \
                        \which will match any equation where the named symbol \
                        \occurs on the left-hand side. \
                        \The name of a rule is given with the K module name \
                        \as a dot-separated prefix: 'MODULE-NAME.rule-name'. \
                        \This option may be specified multiple times to log multiple equations."
                    ]

selectDebugApplyEquation ::
    DebugApplyEquationOptions ->
    SomeEntry ->
    Bool
selectDebugApplyEquation options entry
    | Just (DebugApplyEquation equation _) <- fromEntry entry =
        any (flip HashSet.member $ selected options) (Equation.identifiers equation)
    | otherwise = False

newtype DebugAttemptEquationOptions = DebugAttemptEquationOptions (HashSet Text)
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid)

instance Default DebugAttemptEquationOptions where
    def = DebugAttemptEquationOptions HashSet.empty

instance HasSelected DebugAttemptEquationOptions where
    selected (DebugAttemptEquationOptions s) = s

parseDebugAttemptEquationOptions :: Parser DebugAttemptEquationOptions
parseDebugAttemptEquationOptions =
    mconcat <$> many parse
  where
    parse =
        fmap (DebugAttemptEquationOptions . HashSet.singleton) $
            Options.strOption $
                mconcat
                    [ Options.metavar "EQUATION_IDENTIFIER"
                    , Options.long "debug-attempt-equation"
                    , Options.help
                        "Log every attempt to apply an equation \
                        \that matches EQUATION_IDENTIFIER, \
                        \which may be the name of a symbol or a rule. \
                        \It will not be mentioned whether the application succeeds or not. \
                        \The name of a symbol may be a Kore identifier or a K label, \
                        \which will match any equation where the named symbol \
                        \occurs on the left-hand side. \
                        \The name of a rule is given with the K module name \
                        \as a dot-separated prefix: 'MODULE-NAME.rule-name'. \
                        \This option may be specified multiple times to log multiple equations."
                    ]

selectDebugAttemptEquation ::
    DebugAttemptEquationOptions ->
    SomeEntry ->
    Bool
selectDebugAttemptEquation options entry
    | Just equation <- getEquation =
        any (flip HashSet.member $ selected options) (Equation.identifiers equation)
    | otherwise = False
  where
    getEquation = do
        debugAttemptEquation <- fromEntry entry
        case debugAttemptEquation of
            DebugAttemptEquation equation _ -> pure equation
            DebugAttemptEquationResult equation _ -> pure equation

newtype DebugEquationOptions = DebugEquationOptions (HashSet Text)
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid)

instance Default DebugEquationOptions where
    def = DebugEquationOptions HashSet.empty

instance HasSelected DebugEquationOptions where
    selected (DebugEquationOptions s) = s

parseDebugEquationOptions :: Parser DebugEquationOptions
parseDebugEquationOptions =
    mconcat <$> many parse
  where
    parse =
        fmap (DebugEquationOptions . HashSet.singleton) $
            Options.strOption $
                mconcat
                    [ Options.metavar "EQUATION_IDENTIFIER"
                    , Options.long "debug-equation"
                    , Options.help
                        "Log every attempt to apply or successful application of \
                        \an equation that matches EQUATION_IDENTIFIER, \
                        \which may be the name of a symbol or a rule. \
                        \The name of a symbol may be a Kore identifier or a K label, \
                        \which will match any equation where the named symbol \
                        \occurs on the left-hand side. \
                        \The name of a rule is given with the K module name \
                        \as a dot-separated prefix: 'MODULE-NAME.rule-name'. \
                        \This option may be specified multiple times to log multiple equations."
                    ]

selectDebugEquation ::
    DebugEquationOptions ->
    SomeEntry ->
    Bool
selectDebugEquation options =
    (fmap or . sequence)
        [ selectDebugApplyEquation $ DebugApplyEquationOptions (selected options)
        , selectDebugAttemptEquation $ DebugAttemptEquationOptions (selected options)
        ]

newtype DebugAttemptRewriteOptions = DebugAttemptRewriteOptions (HashSet Text)
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid)

instance Default DebugAttemptRewriteOptions where
    def = DebugAttemptRewriteOptions HashSet.empty

instance HasSelected DebugAttemptRewriteOptions where
    selected (DebugAttemptRewriteOptions s) = s

parseDebugAttemptRewriteOptions :: Parser DebugAttemptRewriteOptions
parseDebugAttemptRewriteOptions =
    mconcat <$> many parse
  where
    parse =
        fmap (DebugAttemptRewriteOptions . HashSet.singleton) $
            Options.strOption $
                mconcat
                    [ Options.metavar "REWRITE_RULE_IDENTIFIER"
                    , Options.long "debug-attempt-rewrite"
                    , Options.help
                        "Log every attempt to apply an rewrite rule \
                        \that matches REWRITE_RULE_IDENTIFIER, \
                        \which may be the name of a symbol or a rule. \
                        \It will not be mentioned whether the application succeeds or not. \
                        \The name of a symbol may be a Kore identifier or a K label, \
                        \which will match any equation where the named symbol \
                        \occurs on the left-hand side. \
                        \The name of a rule is given with the K module name \
                        \as a dot-separated prefix: 'MODULE-NAME.rule-name'. \
                        \This option may be specified multiple times to log multiple equations."
                    ]

selectDebugAttemptRewrite ::
    DebugAttemptRewriteOptions ->
    SomeEntry ->
    Bool
selectDebugAttemptRewrite options entry
    | Just label <- getLabel = HashSet.member label (selected options)
    | otherwise = False
  where
    getLabel = do
        DebugAttemptedRewriteRules _ ruleLabel _ <- fromEntry entry
        ruleLabel

newtype DebugApplyRewriteOptions = DebugApplyRewriteOptions (HashSet Text)
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid)

instance Default DebugApplyRewriteOptions where
    def = DebugApplyRewriteOptions HashSet.empty

instance HasSelected DebugApplyRewriteOptions where
    selected (DebugApplyRewriteOptions s) = s

parseDebugApplyRewriteOptions :: Parser DebugApplyRewriteOptions
parseDebugApplyRewriteOptions =
    mconcat <$> many parse
  where
    parse =
        fmap (DebugApplyRewriteOptions . HashSet.singleton) $
            Options.strOption $
                mconcat
                    [ Options.metavar "REWRITE_RULE_IDENTIFIER"
                    , Options.long "debug-apply-rewrite"
                    , Options.help
                        "Log every succesfully applied rewrite rule that matches REWRITE_RULE_IDENTIFIER, \
                        \which may be the name of a symbol or a rule. \
                        \The name of a symbol may be a Kore identifier or a K label, \
                        \which will match any equation where the named symbol \
                        \occurs on the left-hand side. \
                        \The name of a rule is given with the K module name \
                        \as a dot-separated prefix: 'MODULE-NAME.rule-name'. \
                        \This option may be specified multiple times to log multiple equations."
                    ]

selectDebugApplyRewrite ::
    DebugApplyRewriteOptions ->
    SomeEntry ->
    Bool
selectDebugApplyRewrite options entry
    | Just label <- getLabel = HashSet.member label (selected options)
    | otherwise = False
  where
    getLabel = do
        DebugAppliedLabeledRewriteRule _ mLabel _ <- fromEntry entry
        mLabel

newtype DebugRewriteOptions = DebugRewriteOptions (HashSet Text)
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid)

instance Default DebugRewriteOptions where
    def = DebugRewriteOptions HashSet.empty

instance HasSelected DebugRewriteOptions where
    selected (DebugRewriteOptions s) = s

parseDebugRewriteOptions :: Parser DebugRewriteOptions
parseDebugRewriteOptions =
    mconcat <$> many parse
  where
    parse =
        fmap (DebugRewriteOptions . HashSet.singleton) $
            Options.strOption $
                mconcat
                    [ Options.metavar "REWRITE_RULE_IDENTIFIER"
                    , Options.long "debug-rewrite"
                    , Options.help
                        "Log every attempt to apply or successful application of \
                        \an rewrite rule that matches REWRITE_RULE_IDENTIFIER, \
                        \which may be the name of a symbol or a rule. \
                        \The name of a symbol may be a Kore identifier or a K label, \
                        \which will match any equation where the named symbol \
                        \occurs on the left-hand side. \
                        \The name of a rule is given with the K module name \
                        \as a dot-separated prefix: 'MODULE-NAME.rule-name'. \
                        \This option may be specified multiple times to log multiple equations."
                    ]

selectDebugRewrite ::
    DebugRewriteOptions ->
    SomeEntry ->
    Bool
selectDebugRewrite options =
    (fmap or . sequence)
        [ selectDebugAttemptRewrite $ DebugAttemptRewriteOptions (selected options)
        , selectDebugApplyRewrite $ DebugApplyRewriteOptions (selected options)
        ]

parseTraceRewrites :: Parser (Maybe FilePath)
parseTraceRewrites =
    Options.optional
        ( Options.strOption
            ( Options.metavar "FILENAME"
                <> Options.long "trace-rewrites"
                <> Options.help "Output rewrite trace to a YAML file"
            )
        )

unparseKoreLogOptions :: KoreLogOptions -> [String]
unparseKoreLogOptions
    ( KoreLogOptions
            logType
            logFormat
            logLevel
            timestampsSwitch
            logEntries
            debugSolverOptions
            _
            _
            logSQLiteOptions
            warningSwitch
            toError
            debugApplyEquationOptions
            debugAttemptEquationOptions
            debugEquationOptions
            debugAttemptRewriteOptions
            debugApplyRewriteOptions
            debugRewriteOptions
            _
        ) =
        concat
            [ koreLogTypeFlag logType
            , koreLogFormatFlag logFormat
            , ["--log-level", fmap Char.toLower (show logLevel)]
            , timestampsSwitchFlag timestampsSwitch
            , logEntriesFlag logEntries
            , debugSolverOptionsFlag debugSolverOptions
            , logSQLiteOptionsFlag logSQLiteOptions
            , ["--warnings-to-errors" | AsError <- [warningSwitch]]
            , logEntriesFlag toError
            , debugApplyEquationOptionsFlag debugApplyEquationOptions
            , debugAttemptEquationOptionsFlag debugAttemptEquationOptions
            , debugEquationOptionsFlag debugEquationOptions
            , debugAttemptRewriteOptionsFlag debugAttemptRewriteOptions
            , debugApplyRewriteOptionsFlag debugApplyRewriteOptions
            , debugRewriteOptionsFlag debugRewriteOptions
            ]
      where
        koreLogTypeFlag LogStdErr = []
        koreLogTypeFlag (LogFileText file) = ["--log", file]
        koreLogTypeFlag (LogSomeAction _) = []

        koreLogFormatFlag Standard = []
        koreLogFormatFlag OneLine = ["--log-format=oneline"]
        koreLogFormatFlag Json = ["--log-format=json"]

        timestampsSwitchFlag TimestampsEnable = ["--enable-log-timestamps"]
        timestampsSwitchFlag TimestampsDisable = ["--disable-log-timestamps"]

        logEntriesFlag entries
            | Set.null entries = []
            | otherwise =
                [ "--log-entries"
                , intercalate "," (fmap show (toList entries))
                ]

        debugSolverOptionsFlag (DebugSolverOptions Nothing) = []
        debugSolverOptionsFlag (DebugSolverOptions (Just file)) =
            ["--solver-transcript", file]

        logSQLiteOptionsFlag (LogSQLiteOptions Nothing) = []
        logSQLiteOptionsFlag (LogSQLiteOptions (Just file)) =
            ["--sqlog", file]

        debugApplyEquationOptionsFlag (DebugApplyEquationOptions set) =
            concatMap (("--debug-apply-equation" :) . (: []) . Text.unpack) (toList set)

        debugAttemptEquationOptionsFlag (DebugAttemptEquationOptions set) =
            concatMap (("--debug-attempt-equation" :) . (: []) . Text.unpack) (toList set)

        debugEquationOptionsFlag (DebugEquationOptions set) =
            concatMap (("--debug-equation" :) . (: []) . Text.unpack) (toList set)

        debugAttemptRewriteOptionsFlag (DebugAttemptRewriteOptions set) =
            concatMap (("--debug-attempt-rewrite" :) . (: []) . Text.unpack) (toList set)

        debugApplyRewriteOptionsFlag (DebugApplyRewriteOptions set) =
            concatMap (("--debug-apply-rewrite" :) . (: []) . Text.unpack) (toList set)

        debugRewriteOptionsFlag (DebugRewriteOptions set) =
            concatMap (("--debug-rewrite" :) . (: []) . Text.unpack) (toList set)

newtype UndefinedLabels = UndefinedLabels {unUndefinedLabels :: Map Text [Text]}
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid)

newtype DebugOptionsValidationError = DebugOptionsValidationError UndefinedLabels
    deriving newtype (Eq)

instance Exception DebugOptionsValidationError

instance Pretty DebugOptionsValidationError => Show DebugOptionsValidationError where
    show = show . pretty

instance Pretty DebugOptionsValidationError where
    pretty (DebugOptionsValidationError labels) =
        Pretty.vsep $
            ["Rule labels for the following debug options are not defined:"]
                <> Map.foldMapWithKey
                    (\k v -> [Pretty.hsep [pretty (k <> ":"), pretty . Text.intercalate ", " $ v]])
                    (unUndefinedLabels labels)

validateDebugOptions ::
    Map
        AxiomIdentifier
        [Equation VariableName] ->
    [RewriteRule RewritingVariableName] ->
    KoreLogOptions ->
    Validate UndefinedLabels ()
validateDebugOptions equations rewrites KoreLogOptions{..} = do
    let eqDefinedLabels =
            HashSet.fromList $
                mapMaybe
                    (Attribute.unLabel . Attribute.label . Equation.attributes)
                    (concat (Map.elems equations))
        rwDefinedLabels = HashSet.fromList $ mapMaybe (Attribute.unLabel . from . getRewriteRule) rewrites
        equationLabels =
            [ ("--debug-equation", selected debugEquationOptions)
            , ("--debug-attempt-equation", selected debugAttemptEquationOptions)
            , ("--debug-apply-equation", selected debugApplyEquationOptions)
            ]
        rewriteRuleLabels =
            [ ("--debug-rewrite", selected debugRewriteOptions)
            , ("--debug-attempt-rewrite", selected debugAttemptRewriteOptions)
            , ("--debug-apply-rewrite", selected debugApplyRewriteOptions)
            ]
    for_ equationLabels $ validateOption eqDefinedLabels
    for_ rewriteRuleLabels $ validateOption rwDefinedLabels
  where
    validateOption definedLabels (opt, value) = do
        let undefinedLabels = HashSet.difference value definedLabels
        unless (HashSet.null undefinedLabels) $
            dispute (UndefinedLabels $ Map.singleton opt (HashSet.toList undefinedLabels))
