{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.BugReport (
    BugReport (..),
    BugReportOption (..),
    parseBugReportOption,
    withBugReport,

    -- * Re-exports
    ExitCode (..),
) where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (
    AsyncException (UserInterrupt),
    fromException,
 )
import Control.Monad.Catch (
    ExitCase (..),
    displayException,
    generalBracket,
    handleAll,
 )
import Control.Monad.Extra qualified as Monad
import Data.Binary (Binary)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Debug
import GHC.Generics qualified as GHC
import GHC.IO.Exception (
    IOErrorType (..),
    IOException (ioe_type),
 )
import Generics.SOP qualified as SOP
import Kore.Log.KoreLogOptions (
    DebugOptionsValidationError (..),
    ExeName (..),
 )
import Options.Applicative
import Prelude.Kore
import System.Directory (
    copyFile,
    doesFileExist,
    listDirectory,
    removeFile,
    removePathForcibly,
 )
import System.Exit (
    ExitCode (..),
 )
import System.FilePath (
    (<.>),
    (</>),
 )
import System.IO (
    hPutStrLn,
    stderr,
 )
import System.IO.Temp (
    createTempDirectory,
    getCanonicalTemporaryDirectory,
 )

newtype BugReport = BugReport {toReport :: FilePath}
    deriving stock (Eq, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

data BugReportOption
    = -- | Always creates a bug report
      BugReportEnable BugReport
    | -- | Never creates a bug report
      BugReportDisable
    | -- | Creates a bug report only after a crash
      BugReportOnError
    deriving stock (Eq, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

parseBugReportOption :: Parser BugReportOption
parseBugReportOption =
    parseBugReportEnable <|> parseBugReportDisable
  where
    parseBugReportEnable =
        BugReportEnable . BugReport
            <$> strOption
                ( metavar "REPORT_FILE"
                    <> long "bug-report"
                    <> help "Generate reproducible example of bug at REPORT_FILE.tar.gz"
                )
    parseBugReportDisable =
        flag
            BugReportOnError
            BugReportDisable
            ( long "no-bug-report"
                <> help "Disables the creation of a bug report."
            )

-- | Create a @.tar.gz@ archive containing the bug report.
writeBugReportArchive ::
    -- | Directory to archive
    FilePath ->
    -- | Name of the archive file, without extension.
    FilePath ->
    IO ()
writeBugReportArchive base tar = do
    let sessionCommands = ".sessionCommands"
    Monad.whenM
        (doesFileExist sessionCommands)
        $ do
            copyFile sessionCommands (base </> tail sessionCommands)
            removeFile sessionCommands
    contents <- listDirectory base
    let filename = tar <.> "tar" <.> "gz"
    ByteString.Lazy.writeFile filename . GZip.compress . Tar.write
        =<< Tar.pack base contents
    (hPutStrLn stderr . unwords) ["Created bug report:", filename]

{- | Run the inner action with a temporary directory holding the bug report.

The bug report will be saved as an archive if that was requested by the user, or
if there is an error in the inner action other than
'UserInterrupt' or 'ExitSuccess'.
-}
withBugReport ::
    ExeName ->
    BugReportOption ->
    (FilePath -> IO ExitCode) ->
    IO ExitCode
withBugReport exeName bugReportOption act =
    do
        (exitCode, _) <-
            generalBracket
                acquireTempDirectory
                releaseTempDirectory
                act
        pure exitCode
        & handleAll handler
  where
    handler _ = pure (ExitFailure 1)
    acquireTempDirectory = do
        tmp <- getCanonicalTemporaryDirectory
        createTempDirectory tmp (getExeName exeName)
    releaseTempDirectory tmpDir exitCase = do
        case exitCase of
            ExitCaseSuccess _ -> optionalWriteBugReport tmpDir
            ExitCaseException someException
                | Just ExitSuccess == fromException someException ->
                    {- User exits the repl after the proof was finished -}
                    optionalWriteBugReport tmpDir
                | Just UserInterrupt == fromException someException ->
                    optionalWriteBugReport tmpDir
                | Just (DebugOptionsValidationError _) <- fromException someException ->
                    optionalWriteBugReport tmpDir
                | Just (ioe :: IOException) <- fromException someException
                , NoSuchThing <- ioe_type ioe -> do
                    hPutStrLn stderr $ displayException someException
                    optionalWriteBugReport tmpDir
                | otherwise -> do
                    let message = displayException someException
                    writeFile (tmpDir </> "error" <.> "log") message
                    alwaysWriteBugReport tmpDir
            ExitCaseAbort -> alwaysWriteBugReport tmpDir
        removePathForcibly tmpDir
    alwaysWriteBugReport tmpDir =
        case bugReportOption of
            BugReportEnable bugReport ->
                writeBugReportArchive tmpDir (toReport bugReport)
            BugReportOnError ->
                writeBugReportArchive tmpDir (getExeName exeName)
            BugReportDisable -> pure ()
    optionalWriteBugReport tmpDir =
        case bugReportOption of
            BugReportEnable bugReport ->
                writeBugReportArchive tmpDir (toReport bugReport)
            _ -> pure ()
