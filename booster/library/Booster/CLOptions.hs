{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Booster.CLOptions (
    CLOptions (..),
    EquationOptions (..),
    LogFormat (..),
    TimestampFormat (..),
    clOptionsParser,
    adjustLogLevels,
    levelToContext,
    versionInfoParser,
) where

import Control.Monad.Logger (LogLevel (..))
import Data.ByteString.Char8 qualified as BS (pack)
import Data.Char (isAscii, isPrint)
import Data.List (intercalate, partition)
import Data.List.Extra (splitOn, trim)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeASCII)
import Options.Applicative
import Text.Casing (fromHumps, fromKebab, toKebab, toPascal)
import Text.Read (readMaybe)

import Booster.GlobalState (EquationOptions (..))
import Booster.Log.Context (ContextFilter, ctxt, readContextFilter)
import Booster.Pattern.Pretty
import Booster.SMT.Interface (SMTOptions (..), defaultSMTOptions)
import Booster.SMT.LowLevelCodec qualified as SMT (parseSExpr)
import Booster.Trace (CustomUserEventType)
import Booster.Util (Bound (..), encodeLabel)
import Booster.VersionInfo (VersionInfo (..), versionInfo)

data CLOptions = CLOptions
    { definitionFile :: FilePath
    , mainModuleName :: Text
    , llvmLibraryFile :: Maybe FilePath
    , port :: Int
    , logLevels :: [LogLevel]
    , logTimeStamps :: Bool
    , timeStampsFormat :: TimestampFormat
    , logFormat :: LogFormat
    , logContexts :: [ContextFilter]
    , logFile :: Maybe FilePath
    , smtOptions :: Maybe SMTOptions
    , equationOptions :: EquationOptions
    , indexCells :: [Text]
    , prettyPrintOptions :: [ModifierT]
    , -- developer options below
      eventlogEnabledUserEvents :: [CustomUserEventType]
    }
    deriving (Show)

data LogFormat
    = Standard
    | OneLine
    | Json
    deriving (Eq)

instance Show LogFormat where
    show = \case
        OneLine -> "oneline"
        Standard -> "standard"
        Json -> "json"

data TimestampFormat
    = Pretty
    | Nanoseconds
    deriving (Eq, Enum)

instance Show TimestampFormat where
    show = \case
        Pretty -> "pretty"
        Nanoseconds -> "nanoseconds"

clOptionsParser :: Parser CLOptions
clOptionsParser =
    CLOptions
        <$> strArgument
            ( metavar "DEFINITION_FILE"
                <> help "Kore definition file to verify and use for execution"
            )
        <*> strOption
            ( metavar "MODULE"
                <> long "module"
                <> help "The name of the main module in the Kore definition."
            )
        <*> optional
            ( strOption
                ( metavar "LLVM_BACKEND_LIBRARY"
                    <> long "llvm-backend-library"
                    <> help "Path to the .so/.dylib shared LLVM backend library"
                )
            )
        <*> option
            auto
            ( metavar "SERVER_PORT"
                <> long "server-port"
                <> value 31337
                <> help "Port for the RPC server to bind to"
                <> showDefault
            )
        <*> many
            ( option
                (eitherReader readLogLevel)
                ( metavar "LEVEL"
                    <> long "log-level"
                    <> short 'l'
                    <> help
                        ( "Log level: debug, info (default), warn, error, \
                          \or a custom level: "
                            <> intercalate ", " (map fst allowedLogLevels)
                        )
                )
            )
        <*> switch (long "log-timestamps" <> help "Add timestamps to logs")
        <*> option
            (eitherReader readTimeStampFormat)
            ( metavar "TIMESTAMPFORMAT"
                <> value Pretty
                <> long "timestamp-format"
                <> help
                    ( "Format to output log timestamps in. Available formats: "
                        <> intercalate ", " (map show $ enumFrom (toEnum @TimestampFormat 0))
                    )
                <> showDefault
            )
        <*> option
            (eitherReader readLogFormat)
            ( metavar "LOGFORMAT"
                <> value OneLine
                <> long "log-format"
                <> help "Format to output logs in"
                <> showDefault
            )
        <*> many
            ( option
                (eitherReader readContextFilter)
                ( metavar "CONTEXT"
                    <> long "log-context"
                    <> short 'c'
                    <> help
                        "Log context"
                )
            )
        <*> optional
            ( strOption
                ( metavar "LOG_FILE"
                    <> long "log-file"
                    <> help
                        "Log file to output the logs into"
                )
            )
        <*> parseSMTOptions
        <*> parseEquationOptions
        <*> option
            (eitherReader $ mapM (readCellName . trim) . splitOn ",")
            ( metavar "CELL-NAME[,CELL-NAME]"
                <> long "index-cells"
                <> help "Names of configuration cells to index rewrite rules with (default: 'k')"
                <> value []
            )
        <*> option
            (eitherReader $ mapM (readModifierT . trim) . splitOn ",")
            ( metavar "PRETTY_PRINT"
                <> value [Decoded, Truncated]
                <> long "pretty-print"
                <> help "Prety print options for kore terms: decode, infix, truncated"
                <> showDefault
            )
        -- developer options below
        <*> many
            ( option
                (eitherReader readEventLogTracing)
                ( metavar "TRACE"
                    <> long "trace"
                    <> short 't'
                    <> help
                        ( "Eventlog tracing options: "
                            <> intercalate
                                ", "
                                [toKebab $ fromHumps $ show t | t <- [minBound .. maxBound] :: [CustomUserEventType]]
                        )
                )
            )
  where
    readLogLevel :: String -> Either String LogLevel
    readLogLevel = \case
        "debug" -> Right LevelDebug
        "info" -> Right LevelInfo
        "warn" -> Right LevelWarn
        "error" -> Right LevelError
        other
            | other `elem` map fst allowedLogLevels -> Right (LevelOther $ pack other)
            | otherwise -> Left $ other <> ": Unsupported log level"

    readEventLogTracing :: String -> Either String CustomUserEventType
    readEventLogTracing =
        (\s -> maybe (Left $ s <> " not supported in eventlog tracing") Right $ readMaybe s)
            . toPascal
            . fromKebab

    readLogFormat :: String -> Either String LogFormat
    readLogFormat = \case
        "oneline" -> Right OneLine
        "standard" -> Right Standard
        "json" -> Right Json
        other -> Left $ other <> ": Unsupported log format"

    readModifierT :: String -> Either String ModifierT
    readModifierT = \case
        "truncated" -> Right Truncated
        "infix" -> Right Infix
        "decoded" -> Right Decoded
        other -> Left $ other <> ": Unsupported prettry printer option"

    readTimeStampFormat :: String -> Either String TimestampFormat
    readTimeStampFormat = \case
        "pretty" -> Right Pretty
        "nanoseconds" -> Right Nanoseconds
        other -> Left $ other <> ": Unsupported timestamp format"

    readCellName :: String -> Either String Text
    readCellName input
        | null input =
            Left "Empty cell name"
        | all isAscii input
        , all isPrint input =
            Right $ "Lbl'-LT-'" <> enquote input <> "'-GT-'"
        | otherwise =
            Left $ "Illegal non-ascii characters in `" <> input <> "'"

    enquote = decodeASCII . encodeLabel . BS.pack

-- custom log levels that can be selected
allowedLogLevels :: [(String, String)]
allowedLogLevels =
    [ ("Aborts", "Log information related to aborting execution")
    , ("Rewrite", "Log all rewriting in booster")
    , ("RewriteKore", "Log all rewriting in kore-rpc fall-backs")
    , ("RewriteSuccess", "Log successful rewrites (booster and kore-rpc)")
    , ("Simplify", "Log all simplification/evaluation in booster")
    , ("SimplifyKore", "Log all simplification in kore-rpc")
    , ("SimplifySuccess", "Log successful simplifications (booster and kore-rpc)")
    , ("Depth", "Log the current depth of the state")
    , ("SMT", "Log the SMT-solver interactions")
    , ("ErrorDetails", "Log error conditions with extensive details")
    ,
        ( "EquationWarnings"
        , "Log warnings indicating soft-violations of conditions, i.e. exceeding the equation recursion/iteration limit "
        )
    , ("TimeProfile", "Logs for timing analysis")
    , ("Timing", "Formerly --print-stats")
    ]

levelToContext :: Map Text [ContextFilter]
levelToContext =
    Map.fromList
        [
            ( "Simplify"
            ,
                [ [ctxt| request*,booster|kore>function*|simplification*|hook*,success|failure|abort|detail |]
                , [ctxt| request*,booster|kore>function*|simplification*,match,failure|abort |]
                ]
            )
        ,
            ( "SimplifySuccess"
            ,
                [ [ctxt| request*,booster|kore>function*|simplification*|hook*,success|detail |]
                ]
            )
        ,
            ( "Rewrite"
            ,
                [ [ctxt| request*,booster|kore>rewrite*,success|failure|abort|detail |]
                , [ctxt| request*,booster|kore>rewrite*,match|definedness|constraint,failure|abort |]
                ]
            )
        ,
            ( "RewriteSuccess"
            ,
                [ [ctxt| request*,booster|kore>rewrite*,success|detail |]
                ]
            )
        ,
            ( "SMT"
            ,
                [ [ctxt| request*,booster|kore>smt |]
                ]
            )
        ,
            ( "Aborts"
            ,
                [ [ctxt| request*,booster>rewrite*,detail. |]
                , [ctxt| request*,booster>rewrite*,match|definedness|constraint,abort. |]
                , [ctxt| proxy. |]
                , [ctxt| proxy,abort. |]
                , [ctxt| request*,booster>failure,abort |]
                ]
            )
        ,
            ( "EquationWarnings"
            ,
                [ [ctxt| request*,booster>(simplification *|function *)>warning |]
                ]
            )
        ,
            ( "TimeProfile"
            ,
                [ [ctxt| request*,booster|kore>rewrite*,success|failure|abort|detail |]
                , [ctxt| request*,booster|kore>rewrite*,match|definedness|condition,failure|abort |]
                , [ctxt| request*,booster|kore>function*|simplification*,success|failure|abort|detail |]
                , [ctxt| request*,booster|kore>function*|simplification*,match,failure|abort |]
                ]
            )
        ,
            ( "Timing"
            ,
                [ [ctxt| *>timing |]
                ]
            )
        ]

-- Partition provided log levels into standard and custom ones, and
-- select the lowest standard level. Default to 'LevelInfo' if no
-- standard log level was given.
adjustLogLevels :: [LogLevel] -> (LogLevel, [LogLevel])
adjustLogLevels ls = (standardLevel, customLevels)
  where
    (stds, customLevels) = partition (<= LevelError) ls
    standardLevel = if null stds then LevelInfo else minimum stds

-- SMTOptions aligned with Options.SMT from kore-rpc, with
-- fully-compatible option names in the parser
parseSMTOptions :: Parser (Maybe SMTOptions)
parseSMTOptions =
    flag
        (Just defaultSMTOptions)
        Nothing
        ( long "no-smt"
            <> help "Disable SMT solver sub-process"
        )
        <|> fmap
            Just
            ( SMTOptions
                <$> optional
                    ( strOption
                        ( metavar "PATH"
                            <> long "solver-transcript"
                            <> help "Destination file for SMT transcript (should not exist prior)"
                        )
                    )
                <*> option
                    nonnegativeInt
                    ( metavar "TIMEOUT"
                        <> long "smt-timeout"
                        <> help "Timeout for SMT requests, in milliseconds (0 for Nothing)."
                        <> value smtDefaults.timeout
                        <> showDefault
                    )
                <*> optional
                    ( option
                        nonnegativeInt
                        ( metavar "COUNT"
                            <> long "smt-retry-limit"
                            <> help "Optional Retry-limit for SMT requests - with scaling timeout."
                            <> value (fromMaybe 0 smtDefaults.retryLimit)
                            <> showDefault
                        )
                    )
                <*> optional
                    ( option
                        readTactic
                        ( metavar "TACTIC"
                            <> long "smt-tactic"
                            <> help
                                "Optional Z3 tactic to use when checking satisfiability. \
                                \Example: '(check-sat-using smt)' (i.e., plain 'check-sat')"
                        )
                    )
            )
  where
    smtDefaults = defaultSMTOptions

    nonnegativeInt :: ReadM Int
    nonnegativeInt =
        auto >>= \case
            i
                | i < 0 -> readerError "must be a non-negative integer."
                | otherwise -> pure i

    readTactic =
        either (readerError . ("Invalid s-expression. " <>)) pure . SMT.parseSExpr . BS.pack =<< str

parseEquationOptions :: Parser EquationOptions
parseEquationOptions =
    (\x y -> EquationOptions (Bound x) (Bound y))
        <$> option
            nonnegativeInt
            ( metavar "ITERATION_LIMIT"
                <> long "equation-max-iterations"
                <> help "Number of iterations the equational rules will be attempted for"
                <> value defaultMaxIterations
                <> showDefault
            )
        <*> option
            nonnegativeInt
            ( metavar "RECURSION_LIMIT"
                <> long "equation-max-recursion"
                <> help "Depth of recursion for equational rules evaluation"
                <> value defaultMaxRecursion
                <> showDefault
            )
  where
    defaultMaxIterations = 100
    defaultMaxRecursion = 5

    nonnegativeInt :: ReadM Int
    nonnegativeInt =
        auto >>= \case
            i
                | i < 0 -> readerError "must be a non-negative integer."
                | otherwise -> pure i

versionInfoParser :: Parser (a -> a)
versionInfoParser =
    infoOption
        versionInfoStr
        ( short 'v'
            <> long "version"
            <> help "Print version info."
        )

versionInfoStr :: String
versionInfoStr =
    unlines
        [ "hs-backend-booster version:"
        , "  revision:\t" <> gitHash <> if gitDirty then " (dirty)" else ""
        , "  branch:\t" <> fromMaybe "<unknown>" gitBranch
        , "  last commit:\t" <> gitCommitDate
        ]
  where
    VersionInfo{gitHash, gitDirty, gitBranch, gitCommitDate} = $versionInfo
