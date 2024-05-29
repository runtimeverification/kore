{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Options.SMT (
    KoreSolverOptions (..),
    Solver (..),
    parseKoreSolverOptions,
    unparseKoreSolverOptions,
    defaultSmtPreludeFilePath,
    writeKoreSolverFiles,
    ensureSmtPreludeExists,
) where

import Control.Monad.Extra as Monad
import Data.Char qualified as Char
import Data.Limit (
    Limit (..),
    maybeLimit,
 )
import Data.List (
    intercalate,
 )
import Data.Text qualified as Text
import Kore.Parser.ParserUtils (
    readPositiveIntegral,
 )
import Options.Applicative (
    Parser,
    help,
    long,
    metavar,
    option,
    readerError,
    str,
    strOption,
    value,
 )
import Options.Applicative qualified as Options
import Prelude.Kore
import SMT hiding (
    Solver,
    not,
 )
import SMT.AST qualified as SMT (readSExpr)
import System.Directory (
    copyFile,
    doesFileExist,
 )
import System.FilePath (
    (</>),
 )

-- | Command line options for the SMT solver.
data KoreSolverOptions = KoreSolverOptions
    { timeOut :: !TimeOut
    , retryLimit :: !RetryLimit
    , rLimit :: !RLimit
    , resetInterval :: !ResetInterval
    , prelude :: !Prelude
    , solver :: !Solver
    , tactic :: !(Maybe SExpr)
    }

parseKoreSolverOptions :: Parser KoreSolverOptions
parseKoreSolverOptions =
    KoreSolverOptions
        <$> parseTimeOut
        <*> parseRetryLimit
        <*> parseRLimit
        <*> parseResetInterval
        <*> parsePrelude
        <*> parseSolver
        <*> parseTactic
  where
    parseTimeOut =
        option
            readTimeOut
            ( metavar "SMT_TIMEOUT"
                <> long "smt-timeout"
                <> help "Timeout for calls to the SMT solver, in milliseconds"
                <> value defaultTimeOut
            )

    parseRetryLimit =
        option
            readRetryLimit
            ( metavar "SMT_RETRY_LIMIT"
                <> long "smt-retry-limit"
                <> help "Limit how many times an SMT query can be retried (with scaling timeouts)"
                <> value defaultRetryLimit
            )

    parseRLimit =
        option
            readRLimit
            ( metavar "SMT_RLIMIT"
                <> long "smt-rlimit"
                <> help "Resource limit for calls to the SMT solver"
                <> value defaultRLimit
            )

    parseResetInterval =
        option
            readResetInterval
            ( metavar "SMT_RESET_INTERVAL"
                <> long "smt-reset-interval"
                <> help "Reset the solver after this number of queries"
                <> value defaultResetInterval
            )

    parsePrelude =
        Prelude
            <$> optional
                ( strOption
                    ( metavar "SMT_PRELUDE"
                        <> long "smt-prelude"
                        <> help "Path to the SMT prelude file"
                    )
                )

    parseTactic =
        option
            readTactic
            ( metavar "SMT_TACTIC"
                <> long "smt-tactic"
                <> help "Z3 tactic to use when checking satisfiability. Example: (check-sat-using smt)"
                <> value defaultTactic
            )

    SMT.Config
        { timeOut = defaultTimeOut
        , retryLimit = defaultRetryLimit
        , rLimit = defaultRLimit
        , resetInterval = defaultResetInterval
        , tactic = defaultTactic
        } =
            SMT.z3Config

    readTimeOut = readPositiveIntegral (SMT.TimeOut . Limit) "smt-timeout"
    readRetryLimit = readPositiveIntegral (SMT.RetryLimit . Limit) "smt-retry-limit"
    readRLimit = readPositiveIntegral (SMT.RLimit . Limit) "smt-rlimit"
    readResetInterval =
        readPositiveIntegral SMT.ResetInterval "smt-reset-interval"
    readTactic = do
        tacticStr <- str
        case SMT.readSExpr @Maybe (Text.pack tacticStr) of
            Nothing -> readerError . unwords $ ["smt-tactic", "must be a valid s-expression."]
            Just sexpr -> pure . Just $ sexpr

unparseKoreSolverOptions :: KoreSolverOptions -> [String]
unparseKoreSolverOptions
    KoreSolverOptions
        { timeOut = TimeOut unwrappedTimeOut
        , retryLimit = RetryLimit unwrappedRetryLimit
        , rLimit = RLimit unwrappedRLimit
        , resetInterval
        , prelude = Prelude unwrappedPrelude
        , solver
        , tactic
        } =
        catMaybes
            [ (\limit -> unwords ["--smt-timeout", show limit])
                <$> maybeLimit Nothing Just unwrappedTimeOut
            , (\limit -> unwords ["--smt-retry-limit", show limit])
                <$> maybeLimit Nothing Just unwrappedRetryLimit
            , (\limit -> unwords ["--smt-rlimit", show limit])
                <$> maybeLimit Nothing Just unwrappedRLimit
            , pure $
                unwords
                    [ "--smt-reset-interval"
                    , show . getResetInterval $ resetInterval
                    ]
            , unwrappedPrelude
                $> unwords ["--smt-prelude", defaultSmtPreludeFilePath]
            , pure $ unwords ["--smt", unparseSolver solver]
            , (\tacticSExpr -> unwords ["--smt-tactic", showSExpr tacticSExpr]) <$> tactic
            ]

-- | Available SMT solvers.
data Solver = Z3 | CVC5 | None
    deriving stock (Eq, Ord, Show)
    deriving stock (Enum, Bounded)

parseSolver :: Parser Solver
parseSolver =
    option (snd <$> readSum longName options) $
        metavar "SOLVER"
            <> long longName
            <> help ("SMT solver for checking constraints: " <> knownOptions)
            <> value Z3
  where
    longName = "smt"
    knownOptions = intercalate ", " (map fst options)
    options = [(map Char.toLower $ show s, s) | s <- [minBound .. maxBound]]

unparseSolver :: Solver -> String
unparseSolver Z3 = "z3"
unparseSolver CVC5 = "cvc5"
unparseSolver None = "none"

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

defaultSmtPreludeFilePath :: FilePath
defaultSmtPreludeFilePath = "prelude.smt2"

writeKoreSolverFiles :: KoreSolverOptions -> FilePath -> IO ()
writeKoreSolverFiles
    KoreSolverOptions{prelude = Prelude unwrappedPrelude}
    reportFile =
        for_ unwrappedPrelude $
            flip copyFile (reportFile </> defaultSmtPreludeFilePath)

-- | Ensure that the SMT prelude file exists, if specified.
ensureSmtPreludeExists :: KoreSolverOptions -> IO ()
ensureSmtPreludeExists
    KoreSolverOptions
        { prelude = SMT.Prelude unwrappedPrelude
        } =
        traverse_
            ( \filePath ->
                Monad.whenM
                    (not <$> doesFileExist filePath)
                    (error $ "SMT prelude file does not exist: " <> filePath)
            )
            unwrappedPrelude
