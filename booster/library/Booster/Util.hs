{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}

module Booster.Util (
    decodeLabel,
    decodeLabel',
    encodeLabel,
    Flag (..),
    Bound (..),
    constructorName,
    handleOutput,
    withFastLogger,
    newTimeCache,
    pattern PrettyTimestamps,
    pattern NoPrettyTimestamps,
) where

import Control.AutoUpdate (defaultUpdateSettings, mkAutoUpdate, updateAction, updateFreq)
import Control.DeepSeq (NFData (..))
import Control.Exception (bracket, catch, throwIO)
import Control.Monad.Logger.CallStack qualified as Log
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.Data
import Data.Either (fromRight)
import Data.Hashable (Hashable)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Maybe (fromMaybe)
import Data.Time.Clock.System (SystemTime (..), getSystemTime, systemToUTCTime)
import Data.Time.Format
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import System.Log.FastLogger (
    FastLogger,
    LogStr,
    LogType,
    LogType' (..),
    defaultBufSize,
    newFastLogger,
    newTimedFastLogger,
    toLogStr,
 )
import System.Log.FastLogger.Types (FormattedTime)

newtype Flag (name :: k) = Flag Bool
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving anyclass (NFData, Hashable)

newtype Bound (name :: k) = Bound Int
    deriving stock (Eq, Ord, Show, Generic, Data, Lift)
    deriving newtype (Num)
    deriving anyclass (NFData, Hashable)

constructorName :: Data a => a -> String
constructorName x = showConstr (toConstr x)

-- | Un-escapes special characters in symbol names
decodeLabel :: ByteString -> Either String ByteString
decodeLabel str
    | BS.null str = Right str
    | "'" `BS.isPrefixOf` str =
        let (encoded, rest) = BS.span (/= '\'') (BS.tail str)
         in (<>) <$> decode encoded <*> decodeLabel (BS.drop 1 rest)
    | otherwise =
        let (notEncoded, rest) = BS.span (/= '\'') str
         in (notEncoded <>) <$> decodeLabel rest
  where
    decode :: ByteString -> Either String ByteString
    decode s
        | BS.null s = Right s
        | BS.length code < 4 = Left $ "Bad character code  " <> show code
        | otherwise =
            maybe
                (Left $ "Unknown character code  " <> show code)
                (\c -> (c <>) <$> decode rest)
                (Map.lookup code decodeMap)
      where
        (code, rest) = BS.splitAt 4 s

decodeMap :: Map.Map ByteString ByteString
decodeMap =
    Map.fromList
        [ ("Spce", " ")
        , ("Bang", "!")
        , ("Quot", "\"")
        , ("Hash", "#")
        , ("Dolr", "$")
        , ("Perc", "%")
        , ("And-", "&")
        , ("Apos", "'")
        , ("LPar", "(")
        , ("RPar", ")")
        , ("Star", "*")
        , ("Plus", "+")
        , ("Comm", ",")
        , ("Hyph", "-")
        , ("Stop", ".")
        , ("Slsh", "/")
        , ("Coln", ":")
        , ("SCln", ";")
        , ("-LT-", "<")
        , ("Eqls", "=")
        , ("-GT-", ">")
        , ("Ques", "?")
        , ("-AT-", "@")
        , ("LSqB", "[")
        , ("RSqB", "]")
        , ("Bash", "\\")
        , ("Xor-", "^")
        , ("Unds", "_")
        , ("BQuo", "`")
        , ("LBra", "{")
        , ("Pipe", "|")
        , ("RBra", "}")
        , ("Tild", "~")
        ]

decodeLabel' :: ByteString -> ByteString
decodeLabel' orig =
    fromRight orig (decodeLabel orig)

encodeLabel :: ByteString -> ByteString
encodeLabel = BS.concatMap encodeChar
  where
    encodeMap = Map.fromList [(BS.head c, code) | (code, c) <- Map.assocs decodeMap]
    encodeChar c = fromMaybe (BS.singleton c) $ Map.lookup c encodeMap

-------------------------------------------------------------------
-- logging helpers, some are adapted from monad-logger-aeson
handleOutput ::
    FastLogger ->
    Log.Loc ->
    Log.LogSource ->
    Log.LogLevel ->
    Log.LogStr ->
    IO ()
handleOutput stderrLogger _loc src level msg =
    stderrLogger $ prettySrc <> prettyLevel <> " " <> msg <> "\n"
  where
    prettySrc = if Text.null src then mempty else "[" <> toLogStr src <> "]"
    prettyLevel = case level of
        Log.LevelOther t -> "[" <> toLogStr t <> "]"
        Log.LevelInfo -> mempty
        _ -> "[" <> (toLogStr $ BS.pack $ map toLower $ drop 5 $ show level) <> "]"

newFastLoggerMaybeWithTime :: Maybe (IO FormattedTime) -> LogType -> IO (LogStr -> IO (), IO ())
newFastLoggerMaybeWithTime = \case
    Nothing -> newFastLogger
    Just formattedTime -> \typ -> do
        (logger, cleanup) <- newTimedFastLogger formattedTime typ
        pure (\msg -> logger (\time -> toLogStr time <> " " <> msg), cleanup)

withFastLogger ::
    Maybe (IO FormattedTime) ->
    Maybe FilePath ->
    (FastLogger -> Maybe FastLogger -> IO a) ->
    IO a
withFastLogger mFormattedTime Nothing log' =
    let typStderr = LogStderr defaultBufSize
     in bracket (newFastLoggerMaybeWithTime mFormattedTime typStderr) snd $ \(logger, _) -> log' logger Nothing
withFastLogger mFormattedTime (Just fp) log' =
    let typStderr = LogStderr defaultBufSize
        typFile = LogFileNoRotate fp defaultBufSize
     in bracket (newFastLoggerMaybeWithTime mFormattedTime typStderr) snd $ \(loggerStderr, _) -> do
            removeFileIfExists fp
            bracket (newFastLogger typFile) snd $ \(loggerFile, _) ->
                log' loggerStderr (Just loggerFile)
  where
    removeFileIfExists :: FilePath -> IO ()
    removeFileIfExists fileName = removeFile fileName `catch` handleExists
      where
        handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e

{- |  Make 'IO' action which get cached formatted local time.
Use this to avoid the cost of frequently time formatting by caching an
auto updating formatted time, this cache update every 100 microseconds.

Borrowed almost verbatim from the fast-logger package: https://hackage.haskell.org/package/fast-logger-3.2.3/docs/src/System.Log.FastLogger.Date.html#newTimeCache, but the timestamp resolution and the actions to get and format the time are tweaked.
-}
newTimeCache :: Flag "PrettyTimestamp" -> IO (IO FormattedTime)
newTimeCache prettyTimestamp =
    mkAutoUpdate
        defaultUpdateSettings{updateFreq = 100}
            { updateAction = formatSystemTime prettyTimestamp <$> getSystemTime
            }

pattern PrettyTimestamps, NoPrettyTimestamps :: Flag "PrettyTimestamp"
pattern PrettyTimestamps = Flag True
pattern NoPrettyTimestamps = Flag False

-- | Format time either as a human-readable date and time or as nanoseconds
formatSystemTime :: Flag "PrettyTimestamp" -> SystemTime -> ByteString
formatSystemTime prettyTimestamp =
    let formatString = BS.unpack "%Y-%m-%dT%H:%M:%S.%6Q"
        formatter =
            if coerce prettyTimestamp
                then formatTime defaultTimeLocale formatString . systemToUTCTime
                else show . toNanoSeconds
     in BS.pack . formatter
  where
    toNanoSeconds :: SystemTime -> Integer
    toNanoSeconds MkSystemTime{systemSeconds, systemNanoseconds} =
        fromIntegral @_ @Integer systemSeconds * (10 :: Integer) ^ (9 :: Integer)
            + fromIntegral @_ @Integer systemNanoseconds
