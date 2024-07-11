{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

A very simple RPC client to use for testing HS backend RPC servers

* can send file content verbatim
* can construct 'KorePattern' data

* Results can be compared to expected output files, displaying a diff
  if the response is not the expected one.
-}
module RpcClient (
    main,
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Check qualified as Tar
import Codec.Compression.BZip qualified as BZ2
import Codec.Compression.GZip qualified as GZip
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson qualified as Json
import Data.Aeson.Encode.Pretty qualified as Json
import Data.Aeson.Key qualified as JsonKey
import Data.Aeson.KeyMap qualified as JsonKeyMap
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Char (isDigit, toLower, toUpper)
import Data.Int (Int64)
import Data.List.Extra
import Data.Maybe (isNothing, mapMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Vector as Array (fromList)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy
import Options.Applicative
import Prettyprinter (pretty)
import System.Clock
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Extra
import System.Time.Extra (Seconds, sleep)
import Text.Casing (fromKebab, toPascal)
import Text.Read (readMaybe)

import Booster.JsonRpc (rpcJsonConfig)
import Booster.JsonRpc.Utils (
    DiffResult (DifferentType),
    KoreRpcJson (RpcRequest),
    decodeKoreRpc,
    diffJson,
    isIdentical,
    methodOfRpcCall,
    renderResult,
    rpcTypeOf,
 )
import Booster.Prettyprinter (renderDefault)
import Booster.Syntax.Json qualified as Syntax
import Kore.JsonRpc.Types qualified

main :: IO ()
main = do
    Options{common, runOptions} <- execParser parseOptions
    if common.dryRun
        then handleRunOptions common Nothing runOptions
        else
            retryTCPClient
                2
                10
                common.host
                (show common.port)
                ( \s ->
                    cancelIfInterrupted s $
                        handleRunOptions common (Just s) runOptions
                )
                `catch` (withLogLevel common.logLevel . noServerError common)

handleRunOptions :: CommonOptions -> Maybe Socket -> [RunOptions] -> IO ()
handleRunOptions common@CommonOptions{dryRun} s = \case
    [] -> case s of
        Just sock -> shutdown sock ShutdownReceive
        Nothing -> pure ()
    (RunTarball tarFile keepGoing runOnly noDetails : xs) -> do
        runTarball common s tarFile keepGoing runOnly (not noDetails)
        handleRunOptions common s xs
    (RunSingle mode optionFile options processingOptions : xs) -> do
        let ProcessingOptions{postProcessing, prettify, time} = processingOptions
        request <- withLogLevel common.logLevel $ do
            logInfo_ "Preparing request data"
            request <- prepareRequestData mode optionFile options
            when dryRun $ do
                logInfo_ "Dry-run mode, just showing request instead of sending"
                let write :: BS.ByteString -> LoggingT IO ()
                    write
                        | Just (Expect True file) <- postProcessing = \contents -> do
                            logInfo_ ("Writing request to file " <> file)
                            liftIO $ BS.writeFile file contents
                        | otherwise = liftIO . BS.putStrLn
                    reformat = Json.encodePretty' rpcJsonConfig . Json.decode @Json.Value
                write $ if not prettify then request else reformat request
            -- liftIO exitSuccess
            pure request

        withLogLevel common.logLevel $
            void $
                makeRequest
                    time
                    (getModeFile mode)
                    s
                    request
                    (postProcess prettify postProcessing)
                    []
        handleRunOptions common s xs

makeRequest ::
    MonadLoggerIO m =>
    Bool ->
    String ->
    Maybe Socket ->
    BS.ByteString ->
    (BS.ByteString -> m a) ->
    [Kore.JsonRpc.Types.APIMethod] ->
    m (Maybe a)
makeRequest _ _ Nothing _ _ _ = pure Nothing
makeRequest time name (Just s) request handleResponse runOnly = do
    start <- liftIO $ getTime Monotonic
    logInfo_ $ "Sending request " <> name <> "..."
    logDebug_ $ "Request JSON: " <> BS.unpack request
    case decodeKoreRpc request of
        RpcRequest r -> do
            if null runOnly || methodOfRpcCall r `elem` runOnly
                then do
                    liftIO $ sendAll s request
                    response <- liftIO readResponse
                    end <- liftIO $ getTime Monotonic
                    logInfo_ "Response received."
                    logDebug_ $ "Response JSON: " <> BS.unpack response
                    let timeStr = timeSpecs start end
                    logInfo_ $ "Round trip time for request '" <> name <> "' was " <> timeStr
                    when time $ do
                        logInfo_ $ "Saving timing for " <> name
                        liftIO $ writeFile (name <> ".time") timeStr
                    Just <$> handleResponse response
                else do
                    logInfo_ $ "Skipping " <> reqToStr r <> " request"
                    pure Nothing
        _ -> do
            logWarn_ "Expected an RPC request. Skipping..."
            pure Nothing
  where
    reqToStr :: Kore.JsonRpc.Types.API 'Kore.JsonRpc.Types.Req -> String
    reqToStr = renderDefault . pretty

    bufSize :: Int64
    bufSize = 1024
    readResponse :: IO BS.ByteString
    readResponse = do
        part <- recv s bufSize
        when (BS.length part == 0) $ do
            putStrLn "[Error] Empty response from server. Did the server crash?"
            exitWith (ExitFailure 3)
        if '\n' `BS.elem` part
            then pure part
            else do
                more <- readResponse
                pure $ part <> more

cancelIfInterrupted :: Socket -> IO () -> IO ()
cancelIfInterrupted skt operation =
    catchJust isInterrupt operation $ const sendCancel
  where
    isInterrupt :: AsyncException -> Maybe ()
    isInterrupt UserInterrupt = Just ()
    isInterrupt _other = Nothing

    sendCancel = do
        sendAll skt (Json.encode cancel)
        shutdown skt ShutdownReceive
        hPutStrLn stderr "Interrupted"
        exitWith (ExitFailure 130)

    cancel = object ["jsonrpc" ~> "2.0", "id" ~> "1", "method" ~> "cancel"]

retryTCPClient :: Seconds -> Int -> String -> String -> (Socket -> IO a) -> IO a
retryTCPClient delay retries host port operation
    | retries < 0 || delay <= 0 =
        error $ "retryTCPClient: negative parameters " <> show (delay, retries)
    | retries == 0 = runTCPClient host port operation
    | otherwise =
        catchJust isSocketError (runTCPClient host port operation) tryAgain
  where
    tryAgain msg = do
        hPutStrLn stderr $
            "[Warning] Could not connect: " <> msg <> ". Retrying " <> show retries <> " times"
        sleep delay
        retryTCPClient delay (retries - 1) host port operation

    isSocketError :: IOError -> Maybe String
    isSocketError IOError{ioe_type = NoSuchThing, ioe_filename = Nothing, ioe_description} =
        Just ioe_description
    isSocketError _other = Nothing

----------------------------------------
-- Logging

-- Logging functions without location
logError_, logWarn_, logInfo_, logDebug_ :: MonadLoggerIO m => String -> m ()
logError_ = logWithoutLoc "" LevelError
logWarn_ = logWithoutLoc "" LevelWarn
logInfo_ = logWithoutLoc "" LevelInfo
logDebug_ = logWithoutLoc "" LevelDebug

withLogLevel :: LogLevel -> LoggingT IO a -> IO a
withLogLevel lvl = runStderrLoggingT . filterLogger (const (>= lvl))

----------------------------------------
-- program options

data Options = Options
    { common :: CommonOptions
    , runOptions :: [RunOptions]
    }
    deriving stock (Show)

data CommonOptions = CommonOptions
    { host :: String
    , port :: Int
    , logLevel :: LogLevel
    , dryRun :: Bool
    }
    deriving stock (Show)

data RunOptions
    = -- | run a single request
      RunSingle
        Mode -- what kind of request
        (Maybe FilePath) -- json file with options
        [(String, String)] -- verbatim options (name, value) to add to json
        ProcessingOptions
    | -- | run all requests contained in a tarball (using some conventions)
      RunTarball
        FilePath -- tar file
        Bool -- do not stop on first diff if set to true
        [Kore.JsonRpc.Types.APIMethod] -- only run specified types of requests. Run all if empty.
        Bool -- omit detailed comparison with expected output
    deriving stock (Show)

data ProcessingOptions = ProcessingOptions
    { postProcessing :: Maybe PostProcessing
    , prettify :: Bool
    , time :: Bool
    }
    deriving stock (Show)

{- | Defines details for a single requests. Either assemble a request
 from the state in a file and given options in a file or on the
 command line, or raw data (entire input in a file) without additional
 options.
-}
data Mode
    = Exec FilePath
    | Simpl FilePath
    | AddModule FilePath
    | GetModel FilePath
    | Check FilePath FilePath
    | SendRaw FilePath
    deriving stock (Show)

getModeFile :: Mode -> FilePath
getModeFile = \case
    Exec f -> f
    Simpl f -> f
    AddModule f -> f
    GetModel f -> f
    Check f1 _ -> f1
    SendRaw f -> f

{- | Optional output post-processing:
  * 'Expect' checks formatted output against a given golden file.
  * If `regenerate` is set to true, will create/overrie the expected file with received output
-}
data PostProcessing = Expect
    { regenerate :: Bool
    , expectFile :: FilePath
    }
    deriving stock (Show)

parseCommonOptions :: Parser CommonOptions
parseCommonOptions =
    CommonOptions
        <$> strOption
            ( long "host"
                <> short 'h'
                <> metavar "HOST"
                <> value "127.0.0.1"
                <> help "server host to connect to"
                <> showDefault
            )
        <*> option
            auto
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value 31337
                <> help "server port to connect to"
                <> showDefault
            )
        <*> option
            readLogLevel
            ( long "log-level"
                <> short 'l'
                <> metavar "LOG_LEVEL"
                <> value LevelInfo
                <> help "Log level, one of [Error, Warn, Info, Debug]"
                <> showDefault
            )
        <*> switch (long "dry-run" <> help "Do not send anything, just output the request")
  where
    readLogLevel :: ReadM LogLevel
    readLogLevel =
        eitherReader $ \s -> case map toLower s of
            "debug" -> Right LevelDebug
            "info" -> Right LevelInfo
            "warn" -> Right LevelWarn
            "error" -> Right LevelError
            _other -> Left $ s <> ": Unsupported log level"

parseOptions :: ParserInfo Options
parseOptions =
    info
        (parseOptions' <**> helper)
        ( fullDesc
            <> progDesc "Simple RPC test client"
        )
  where
    parseOptions' =
        Options
            <$> parseCommonOptions
            <*> some parseMode

parseProcessingOptions :: Parser ProcessingOptions
parseProcessingOptions =
    ProcessingOptions
        <$> optional parsePostProcessing
        <*> prettifyOpt
        <*> timeOpt
  where
    flagOpt name desc = switch $ long name <> help desc
    prettifyOpt = flagOpt "prettify" "format JSON before printing"
    timeOpt = flagOpt "time" "record the timing information between sending a request and receiving a response"

parsePostProcessing :: Parser PostProcessing
parsePostProcessing =
    ( Expect
        <$> ( flag False True $
                long "regenerate"
                    <> help "regenerate the expected file"
            )
        <*> strOption
            ( long "expect"
                <> metavar "EXPECTATIONFILE"
                <> help "compare JSON output against file contents"
            )
    )
        <|> ( Expect True
                <$> ( strOption $
                        long "output"
                            <> short 'o'
                            <> metavar "OUTPUTFILE"
                            <> help "write JSON output to a file"
                    )
            )

parseMode :: Parser RunOptions
parseMode =
    subparser
        ( command
            "send"
            ( info
                ( RunSingle
                    <$> (SendRaw <$> strArgument (metavar "FILENAME"))
                    <*> pure Nothing -- no param file
                    <*> pure [] -- no params
                    <*> parseProcessingOptions
                    <**> helper
                )
                (progDesc "send the raw file contents directly")
            )
            <> command
                "execute"
                ( info
                    ( RunSingle
                        <$> (Exec <$> strArgument (metavar "FILENAME"))
                        <*> paramFileOpt
                        <*> many paramOpt
                        <*> parseProcessingOptions
                        <**> helper
                    )
                    (progDesc "execute (rewrite) the state in the file")
                )
            <> command
                "simplify"
                ( info
                    ( RunSingle
                        <$> (Simpl <$> strArgument (metavar "FILENAME"))
                        <*> paramFileOpt
                        <*> many paramOpt
                        <*> parseProcessingOptions
                        <**> helper
                    )
                    (progDesc "simplify the state or condition in the file")
                )
            <> command
                "add-module"
                ( info
                    ( RunSingle
                        <$> (AddModule <$> strArgument (metavar "FILENAME"))
                        <*> paramFileOpt
                        <*> many paramOpt
                        <*> parseProcessingOptions
                        <**> helper
                    )
                    (progDesc "add the module in the given kore file")
                )
            <> command
                "get-model"
                ( info
                    ( RunSingle
                        <$> (GetModel <$> strArgument (metavar "FILENAME"))
                        <*> paramFileOpt
                        <*> many paramOpt
                        <*> parseProcessingOptions
                        <**> helper
                    )
                    (progDesc "check satisfiability/provide model for the state in the file")
                )
            <> command
                "run-tarball"
                ( info
                    ( RunTarball
                        <$> strArgument (metavar "FILENAME")
                        <*> switch (long "keep-going" <> help "do not stop on unexpected output")
                        <*> readAPIMethods
                            ( long "run-only"
                                <> help "Only run the specified request(s), e.g. --run-only \"add-module implies\""
                            )
                        <*> switch
                            ( long "omit-details"
                                <> help "only compare response types, not contents"
                            )
                        <**> helper
                    )
                    (progDesc "Run all requests and compare responses from a bug report tarball")
                )
        )
  where
    paramFileOpt =
        optional $
            strOption $
                long "param-file"
                    <> metavar "PARAMFILE"
                    <> help "file with parameters (json object), optional"
    readAPIMethods desc = concat <$> many single
      where
        single = option (maybeReader $ \s -> mapM readAPIMethod $ words s) desc

        readAPIMethod :: String -> Maybe Kore.JsonRpc.Types.APIMethod
        readAPIMethod = readMaybe . (<> "M") . toPascal . fromKebab

    paramOpt =
        option readPair $
            short 'O'
                <> metavar "NAME=VALUE"
                <> help "parameters to use (name=value)"
    readPair =
        maybeReader $ \s -> case split (== '=') s of [k, v] -> Just (k, v); _ -> Nothing

----------------------------------------
-- Running all requests contained in the subdirectories of the tarball

runTarball ::
    CommonOptions ->
    Maybe Socket ->
    FilePath ->
    Bool ->
    [Kore.JsonRpc.Types.APIMethod] ->
    Bool ->
    IO ()
runTarball _ Nothing _ _ _ _ = pure ()
runTarball common (Just sock) tarFile keepGoing runOnly compareDetails = do
    -- unpack tar files, determining type from extension(s)
    let unpackTar
            | ".tar" == takeExtension tarFile = Tar.read
            | ".tgz" == takeExtension tarFile = Tar.read . GZip.decompress
            | ".tar.gz" `isSuffixOf` takeExtensions tarFile = Tar.read . GZip.decompress
            | ".tar.bz2" `isSuffixOf` takeExtensions tarFile = Tar.read . BZ2.decompress
            | otherwise = Tar.read

    containedFiles <- unpackTar <$> BS.readFile tarFile
    let checked = Tar.checkSecurity containedFiles
    -- probe server connection before doing anything, display
    -- instructions unless server was found.
    runAllRequests checked sock
  where
    runAllRequests ::
        Tar.Entries (Either Tar.FormatError Tar.FileNameError) -> Socket -> IO ()
    runAllRequests checked skt = cancelIfInterrupted skt $ do
        withTempDir $ \tmp -> withLogLevel common.logLevel $ do
            -- unpack relevant tar files (rpc_* directories only)
            logInfo_ $ unwords ["unpacking json files from tarball", tarFile, "into", tmp]
            jsonFiles <-
                liftIO $ Tar.foldEntries (unpackIfRpc tmp) (pure []) throwAnyError checked
            logInfo_ $ "RPC data:" <> show jsonFiles

            -- we should not rely on the requests being returned in a sorted order and
            -- should therefore sort them explicitly
            let requests = sort $ mapMaybe (stripSuffix "_request.json") jsonFiles
                successMsg = if compareDetails then "matches expected" else "has expected type"
            results <-
                forM requests $ \r -> do
                    mbError <- runRequest skt tmp jsonFiles r
                    case mbError of
                        Just err -> do
                            logError_ $ "Request " <> r <> " failed: " <> BS.unpack err
                            unless keepGoing $
                                liftIO $
                                    shutdown skt ShutdownReceive >> exitWith (ExitFailure 2)
                        Nothing ->
                            logInfo_ $ unwords ["Response to", r, successMsg]
                    pure mbError
            liftIO $ shutdown skt ShutdownReceive
            liftIO $ exitWith (if all isNothing results then ExitSuccess else ExitFailure 2)

    -- complain on any errors in the tarball
    throwAnyError :: Either Tar.FormatError Tar.FileNameError -> IO a
    throwAnyError = either throwIO throwIO

    -- unpack all */*.json files into dir and return their names
    unpackIfRpc :: FilePath -> Tar.Entry -> IO [FilePath] -> IO [FilePath]
    unpackIfRpc tmpDir entry acc = do
        case splitFileName (Tar.entryPath entry) of
            -- unpack all directories "<something>" containing "*.json" files
            (dir, "") -- directory
                | Tar.Directory <- Tar.entryContent entry -> do
                    createDirectoryIfMissing True dir -- create rpc dir so we can unpack files there
                    acc -- no additional file to return
                | otherwise ->
                    acc -- skip other directories and top-level files
            (dir, file)
                | ".json" `isSuffixOf` file
                , not ("." `isPrefixOf` file)
                , Tar.NormalFile bs _size <- Tar.entryContent entry -> do
                    -- unpack json files into tmp directory
                    let newPath = dir </> file
                    -- current tarballs do not have dir entries, create dir here
                    createDirectoryIfMissing True $ tmpDir </> dir
                    BS.writeFile (tmpDir </> newPath) bs
                    (newPath :) <$> acc
                | otherwise ->
                    -- skip anything else
                    acc

    -- Runs one request, checking that a response is available for
    -- comparison. Returns Nothing if successful (identical
    -- response), or rendered diff or error message if failing
    runRequest ::
        MonadLoggerIO m => Socket -> FilePath -> [FilePath] -> String -> m (Maybe BS.ByteString)
    runRequest skt tmpDir jsonFiles basename
        | not . (`elem` jsonFiles) $ basename <> "_response.json" =
            pure . Just . BS.pack $ "Response file " <> basename <> "_response.json is missing."
        | not . (`elem` jsonFiles) $ basename <> "_request.json" =
            pure . Just . BS.pack $ "Request file " <> basename <> "_request.json is missing."
        | otherwise = do
            request <- liftIO . BS.readFile $ tmpDir </> basename <> "_request.json"
            expected <- liftIO . BS.readFile $ tmpDir </> basename <> "_response.json"

            let showResult =
                    renderResult "expected response" "actual response"
            makeRequest False basename (Just skt) request pure runOnly >>= \case
                Nothing -> pure Nothing -- should not be reachable
                Just actual
                    | compareDetails -> do
                        let diff = diffJson expected actual
                        if isIdentical diff
                            then pure Nothing
                            else pure . Just $ showResult diff
                    | otherwise -> do
                        let expectedType = rpcTypeOf (decodeKoreRpc expected)
                            actualType = rpcTypeOf (decodeKoreRpc actual)
                        if expectedType == actualType
                            then pure Nothing
                            else pure . Just $ showResult (DifferentType expectedType actualType)

noServerError :: MonadLoggerIO m => CommonOptions -> IOException -> m ()
noServerError common e@IOError{ioe_type = NoSuchThing} = do
    -- show instructions how to run the server
    logError_ $ "Could not connect to RPC server on port " <> show common.port
    logError_ $ show e
    logError_ $
        unlines
            [ ""
            , "To run the required RPC server, you need to"
            , "1) extract `definition.kore` and `server_instance.json` from the tarball;"
            , "2) look up the module name `<MODULE>` in `server_instance.json`;"
            , "3) and then run the server using"
            , "   $ kore-rpc definition.kore --module <MODULE> --server-port " <> show common.port
            , ""
            , "If you want to use `kore-rpc-booster, you should also compile an LLVM backend library"
            , "by 1) extracting the `llvm_definition/` directory from the tarball;"
            , "   2) running the llvm backend compiler on the unpacked files:"
            , "      $ llvm-kompile llvm_definition/definition.kore llvm_definition/dt c -- -o interpreter`"
            , "This will generate `interpreter.[so|dylib]` and you can run"
            , "  `kore-rpc-booster definition.kore --main-module <MODULE> --llvm-backend-library interpreter.so`"
            ]
    liftIO $ exitWith (ExitFailure 1)
noServerError _ otherError = do
    logError_ $ show otherError
    liftIO $ exitWith (ExitFailure 1)

----------------------------------------
prepareRequestData ::
    MonadLoggerIO m => Mode -> Maybe FilePath -> [(String, String)] -> m BS.ByteString
prepareRequestData (SendRaw file) mbFile opts = do
    unless (isNothing mbFile) $
        logWarn_ "Raw mode, ignoring given option file"
    unless (null opts) $
        logWarn_ "Raw mode, ignoring given request options"
    liftIO $ BS.readFile file
prepareRequestData (Exec file) mbOptFile opts =
    liftIO $ prepareOneTermRequest "execute" file mbOptFile opts
prepareRequestData (Simpl file) mbOptFile opts =
    liftIO $ prepareOneTermRequest "simplify" file mbOptFile opts
prepareRequestData (AddModule file) mbOptFile opts = do
    moduleText <- liftIO $ Text.readFile file
    paramsFromFile <-
        liftIO $
            maybe
                (pure JsonKeyMap.empty)
                ( BS.readFile
                    >=> either error (pure . getObject) . Json.eitherDecode @Json.Value
                )
                mbOptFile
    let params = paramsFromFile <> object opts
    pure . Json.encode $
        object
            [ "jsonrpc" ~> "2.0"
            , "id" ~> "1"
            , "method" ~> "add-module"
            ]
            +: "params"
            ~> Json.Object (params +: "module" ~> Json.String moduleText)
prepareRequestData (GetModel file) mbOptFile opts =
    liftIO $ prepareOneTermRequest "get-model" file mbOptFile opts
prepareRequestData (Check _file1 _file2) _mbOptFile _opts = do
    error "not implemented yet"

prepareOneTermRequest ::
    String -> FilePath -> Maybe FilePath -> [(String, String)] -> IO BS.ByteString
prepareOneTermRequest method file mbOptFile opts = do
    term :: Json.Value <-
        Json.toJSON
            <$> ( BS.readFile file -- decode given term to test whether it is valid
                    >>= either error pure . Json.eitherDecode @Syntax.KoreJson
                )
    paramsFromFile <-
        maybe
            (pure JsonKeyMap.empty)
            ( BS.readFile
                >=> either error (pure . getObject) . Json.eitherDecode @Json.Value
            )
            mbOptFile
    let params = paramsFromFile <> object opts
    let requestData =
            object
                [ "jsonrpc" ~> "2.0"
                , "id" ~> "1"
                , "method" ~> method
                ]
                +: "params"
                ~> Json.Object (params +: "state" ~> term)
    pure $ Json.encode requestData

getObject :: Json.Value -> Json.Object
getObject (Json.Object o) = o
getObject other = error $ "Expected object, found " <> show other

object :: [(String, String)] -> Json.Object
object = JsonKeyMap.fromList . map mkKeyPair
  where
    mkKeyPair = bimap JsonKey.fromString valueFrom

    -- second-guessing the value type from the contents
    -- we need single-word strings, lists of strings, and numbers
    valueFrom :: String -> Json.Value
    valueFrom [] = Json.Null
    valueFrom s@('[' : rest)
        | last rest == ']' =
            Json.Array $ valuesFrom (init rest)
        | otherwise =
            error $ "garbled list " <> s
    valueFrom s
        | all isDigit s =
            Json.Number (fromInteger $ read s)
        | map toLower s `elem` ["false", "true"] =
            Json.Bool (read $ title $ map toLower s)
    valueFrom s =
        Json.String $ Text.pack s

    title [] = []
    title (x : xs) = toUpper x : xs

    -- comma-separated list of values
    valuesFrom :: String -> Json.Array
    valuesFrom = Array.fromList . map (valueFrom . trim) . split (== ',')

infixl 5 ~>
(~>) :: k -> v -> (k, v)
(~>) = (,)

infixl 4 +:
(+:) :: Json.Object -> (String, Json.Value) -> Json.Object
o +: (k, v) = JsonKeyMap.insert (JsonKey.fromString k) v o

postProcess ::
    MonadLoggerIO m => Bool -> Maybe PostProcessing -> BS.ByteString -> m ()
postProcess prettify postProcessing output =
    case postProcessing of
        Nothing ->
            liftIO $ BS.putStrLn $ if prettify then prettyOutput else output
        Just Expect{expectFile, regenerate} -> do
            liftIO (doesFileExist expectFile) >>= \case
                False ->
                    if regenerate
                        then do
                            logInfo_ $ "Writing file " <> expectFile <> " for the first time."
                            liftIO $ BS.writeFile expectFile prettyOutput
                        else do
                            logError_ $
                                "The expected file "
                                    <> expectFile
                                    <> " does not exist. Use `--regenerate` if you wish to create it."
                            liftIO . exitWith $ ExitFailure 1
                True -> do
                    expected <- liftIO $ BS.readFile expectFile
                    let diff = diffJson expected prettyOutput
                    unless (isIdentical diff) $ do
                        liftIO $ BS.writeFile "response" prettyOutput
                        liftIO $ BS.putStrLn $ renderResult "expected response" "actual response" diff

                        if regenerate
                            then do
                                logInfo_ $ "Re-generating expected file " <> expectFile
                                liftIO $ renameFile "response" expectFile
                            else do
                                liftIO $ removeFile "response"
                                logError_ $ "Response differs from expected " <> expectFile
                                liftIO . exitWith $ ExitFailure 1
                    logInfo_ $ "Output matches " <> expectFile
  where
    prettyOutput =
        Json.encodePretty' rpcJsonConfig $
            either error (id @Json.Value) $
                Json.eitherDecode output

timeSpecs :: TimeSpec -> TimeSpec -> String
timeSpecs = fmt0
  where
    fmt diff
        | Just i <- scale (10 ^ (9 :: Int)) = show i <> " s"
        | Just i <- scale (10 ^ (6 :: Int)) = show i <> " ms"
        | Just i <- scale (10 ^ (3 :: Int)) = show i <> " us"
        | otherwise = show diff <> " ns"
      where
        scale :: Integer -> Maybe Double
        scale i =
            if diff >= i
                then Just (fromIntegral diff / fromIntegral i)
                else Nothing

    fmt0 (TimeSpec s1 n1) (TimeSpec s2 n2) = fmt diff
      where
        diff :: Integer
        diff = a2 - a1
        a1 = (fromIntegral s1 * 10 ^ (9 :: Int)) + fromIntegral n1
        a2 = (fromIntegral s2 * 10 ^ (9 :: Int)) + fromIntegral n2
