{-# LANGUAGE TemplateHaskell #-}

module Booster.CLOptions (
    CLOptions (..),
    clOptionsParser,
    adjustLogLevels,
    versionInfoParser,
) where

import Booster.Trace (CustomUserEventType)
import Booster.VersionInfo (VersionInfo (..), versionInfo)
import Control.Monad.Logger (LogLevel (..))
import Data.List (intercalate, partition)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Options.Applicative
import Text.Casing (fromHumps, fromKebab, toKebab, toPascal)
import Text.Read (readMaybe)

import Booster.SMT.Interface (SMTOptions (..))

data CLOptions = CLOptions
    { definitionFile :: FilePath
    , mainModuleName :: Text
    , llvmLibraryFile :: Maybe FilePath
    , port :: Int
    , logLevels :: [LogLevel]
    , smtOptions :: Maybe SMTOptions
    , -- developer options below
      eventlogEnabledUserEvents :: [CustomUserEventType]
    , hijackEventlogFile :: Maybe FilePath
    }
    deriving (Show)

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
        <*> parseSMTOptions
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
        <*> optional
            ( strOption
                ( metavar "HIJACK_EVENTLOG_FILE"
                    <> long "hijack-eventlog-file"
                    <> help
                        "Hijack LlvmCall tracing events and write them to a file to overcome GHC RTS' restriction on event size (2^16 bytes). Avoid at all costs."
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

-- custom log levels that can be selected
allowedLogLevels :: [(String, String)]
allowedLogLevels =
    [ ("Rewrite", "Log all rewriting in booster")
    , ("RewriteKore", "Log all rewriting in kore-rpc fall-backs")
    , ("RewriteSuccess", "Log successful rewrites (booster and kore-rpc)")
    , ("Simplify", "Log all simplification/evaluation in booster")
    , ("SimplifyKore", "Log all simplification in kore-rpc")
    , ("SimplifySuccess", "Log successful simplifications (booster and kore-rpc)")
    , ("Depth", "Log the current depth of the state")
    , ("SMT", "Log the SMT-solver interactions")
    ]

-- Partition provided log levels into standard and custom ones, and
-- select the lowest standard level. Default to 'LevelInfo' if no
-- standard log level was given.
adjustLogLevels :: [LogLevel] -> (LogLevel, [LogLevel])
adjustLogLevels ls = (standardLevel, customLevels)
  where
    (stds, customLevels) = partition (<= LevelError) ls
    standardLevel = if null stds then LevelInfo else minimum stds

-- FIXME SMTOptions should later replace Options.SMT from kore-rpc,
-- with fully-compatible option names
parseSMTOptions :: Parser (Maybe SMTOptions)
parseSMTOptions =
    flag
        (Just $ SMTOptions Nothing)
        Nothing
        ( long "no-smt"
            <> help "Disable SMT solver sub-process"
        )
        <|> ( Just . SMTOptions
                <$> optional
                    ( strOption
                        ( metavar "SMT_TRANSCRIPT_FILE"
                            <> long "smt-transcript"
                            <> help "Destination file for SMT transcript (should not exist prior)"
                        )
                    )
            )

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
