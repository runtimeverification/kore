{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugSolver (
    DebugSolverSend (..),
    DebugSolverRecv (..),
    logDebugSolverSendWith,
    logDebugSolverRecvWith,
    DebugSolverOptions (..),
    emptyDebugSolverOptions,
    parseDebugSolverOptions,
    solverTranscriptLogger,
) where

import Data.Default
import Data.Text (
    Text,
 )
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text.Lazy.Builder
import Log (
    Entry (..),
    LogAction (..),
    Severity (Debug),
    SimpleContext (CtxSMT),
    SomeEntry (..),
    logWith,
    single,
 )
import Options.Applicative (
    Parser,
    help,
    long,
    strOption,
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import SMT.AST (
    SExpr (..),
 )
import SMT.AST qualified as SMT

newtype DebugSolverSend = DebugSolverSend
    { getSendSExpr :: SExpr
    }
    deriving stock (Show)

newtype DebugSolverRecv = DebugSolverRecv
    { getRecvSExpr :: Text
    }
    deriving stock (Show)

instance Pretty DebugSolverSend where
    pretty DebugSolverSend{getSendSExpr} =
        pretty . SMT.buildText $ getSendSExpr

instance Pretty DebugSolverRecv where
    pretty DebugSolverRecv{getRecvSExpr} =
        pretty getRecvSExpr

instance Entry DebugSolverSend where
    entrySeverity _ = Debug
    oneLineDoc _ = "DebugSolverSend"
    helpDoc _ = "log commands sent to SMT solver"
    oneLineContextDoc _ = single CtxSMT

instance Entry DebugSolverRecv where
    entrySeverity _ = Debug
    oneLineDoc _ = "DebugSolverRecv"
    helpDoc _ = "log responses received from SMT solver"
    oneLineContextDoc _ = single CtxSMT

logDebugSolverSendWith ::
    LogAction m SomeEntry ->
    SExpr ->
    m ()
logDebugSolverSendWith logger sExpr =
    logWith logger $ DebugSolverSend sExpr

logDebugSolverRecvWith ::
    LogAction m SomeEntry ->
    Text ->
    m ()
logDebugSolverRecvWith logger smtText =
    logWith logger $ DebugSolverRecv smtText

solverTranscriptLogger ::
    Applicative m =>
    LogAction m Text ->
    LogAction m SomeEntry
solverTranscriptLogger textLogger =
    LogAction action
  where
    action entry
        | Just sendEntry <- matchDebugSolverSend entry =
            unLogAction textLogger (sentText sendEntry)
        | Just recvEntry <- matchDebugSolverRecv entry =
            unLogAction textLogger (receivedText recvEntry)
        | otherwise =
            pure ()
      where
        sentText :: DebugSolverSend -> Text
        sentText (DebugSolverSend sexpr) =
            let builder = SMT.buildSExpr sexpr
             in (Text.Lazy.toStrict . Text.Lazy.Builder.toLazyText) builder
        receivedText :: DebugSolverRecv -> Text
        receivedText (DebugSolverRecv text) = "; " <> text

matchDebugSolverSend :: SomeEntry -> Maybe DebugSolverSend
matchDebugSolverSend = fromEntry

matchDebugSolverRecv :: SomeEntry -> Maybe DebugSolverRecv
matchDebugSolverRecv = fromEntry

{- | Options (from the command-line) specifying where to create a solver
transcript.

See also: 'parseDebugSolverOptions'
-}
newtype DebugSolverOptions = DebugSolverOptions
    { logFile :: Maybe FilePath
    }
    deriving stock (Eq, Show)

instance Default DebugSolverOptions where
    def = DebugSolverOptions Nothing

parseDebugSolverOptions :: Parser DebugSolverOptions
parseDebugSolverOptions =
    (DebugSolverOptions . Just <$> parseLogFile)
        <|> pure (def @DebugSolverOptions)
  where
    parseLogFile =
        let info =
                long "solver-transcript"
                    <> help "Name of the file for the SMT solver transcript."
         in strOption info

emptyDebugSolverOptions :: DebugSolverOptions
emptyDebugSolverOptions = DebugSolverOptions{logFile = Nothing}
