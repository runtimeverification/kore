{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugRetrySolverQuery (
    DebugRetrySolverQuery,
    debugRetrySolverQuery,
) where

import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.Variable (
    InternalVariable,
    VariableName,
    toVariableName,
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

newtype DebugRetrySolverQuery = DebugRetrySolverQuery
    {predicates :: NonEmpty (Predicate VariableName)}
    deriving stock (Show)

instance Pretty DebugRetrySolverQuery where
    pretty DebugRetrySolverQuery{predicates} =
        Pretty.vsep $
            [ "The SMT solver initially failed to solve the following query:"
            , Pretty.indent 2 "Decide predicate:"
            , Pretty.indent 4 (pretty predicate)
            , Pretty.indent 2 "with side conditions:"
            ]
                <> fmap (Pretty.indent 4 . pretty) sideConditions
                <> [ "The SMT solver was reset and the query\
                     \ was tried one more time."
                   ]
      where
        predicate :| sideConditions = predicates

instance Entry DebugRetrySolverQuery where
    entrySeverity _ = Debug
    oneLineDoc _ = "DebugRetrySolverQuery"
    oneLineContextDoc _ = single CtxSMT
    helpDoc _ =
        "warning raised when the solver failed to decide\
        \ the satisfiability of a formula, indicating that\
        \ the solver was reset and the formula retried"

debugRetrySolverQuery ::
    InternalVariable variable =>
    MonadLog log =>
    NonEmpty (Predicate variable) ->
    log ()
debugRetrySolverQuery predicates' =
    logEntry DebugRetrySolverQuery{predicates}
  where
    predicates =
        Predicate.mapVariables (pure toVariableName) <$> predicates'
