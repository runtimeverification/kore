{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Log.WarnRestartSolver (
    WarnRestartSolver,
    warnRestartSolver,
) where

import Control.Monad.Catch qualified as Exception
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified
import SMT.SimpleSMT (SolverException)

newtype WarnRestartSolver = WarnRestartSolver
    {solverException :: SolverException}
    deriving stock (Show)

instance Pretty WarnRestartSolver where
    pretty WarnRestartSolver{solverException} =
        Pretty.vsep
            [ "The SMT solver crashed with the following exception:"
            , Pretty.indent 4 (pretty $ Exception.displayException solverException)
            , "Restarting and reinitialising the solver\
              \, attempting to continue execution."
            ]

instance Entry WarnRestartSolver where
    entrySeverity _ = Warning
    oneLineDoc _ = "WarnRestartSolver"
    oneLineContextDoc _ = single CtxWarn
    helpDoc _ =
        "warning raised to notify the user that the solver has\
        \ crashed and the backend will attempt to restart it,\
        \ reinitialise it and continue execution"

warnRestartSolver ::
    MonadLog log =>
    SolverException ->
    log ()
warnRestartSolver solverException =
    logEntry WarnRestartSolver{solverException}
