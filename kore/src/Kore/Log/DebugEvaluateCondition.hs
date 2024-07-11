{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugEvaluateCondition (
    DebugEvaluateCondition (..),
    whileDebugEvaluateCondition,
    debugEvaluateConditionResult,
) where

import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.TermLike
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified
import SMT.SimpleSMT (
    Result (..),
 )
import SQL qualified

data DebugEvaluateCondition
    = DebugEvaluateCondition (NonEmpty (Predicate VariableName))
    | DebugEvaluateConditionResult Result
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Pretty DebugEvaluateCondition where
    pretty (DebugEvaluateCondition predicates) =
        Pretty.vsep
            ( [ "evaluating predicate:"
              , Pretty.indent 4 (pretty predicate)
              ]
                ++ do
                    sideCondition <- sideConditions
                    [ "with side condition:"
                        , Pretty.indent 4 (pretty sideCondition)
                        ]
            )
      where
        predicate :| sideConditions = predicates
    pretty (DebugEvaluateConditionResult result) =
        case result of
            Unsat -> "solver returned unsatisfiable"
            Sat -> "solver returned satisfiable"
            Unknown condition -> "solver returned unknown: " <> pretty condition

instance Entry DebugEvaluateCondition where
    entrySeverity _ = Debug
    contextDoc _ = Just "while evaluating predicate"
    oneLineDoc (DebugEvaluateCondition _) = "DebugEvaluateCondition _"
    oneLineDoc result = pretty (show result)
    oneLineContextDoc DebugEvaluateCondition{} =
        map CLNullary [CtxConstraint, CtxSMT]
    oneLineContextDoc DebugEvaluateConditionResult{} =
        map CLNullary [CtxConstraint, CtxSMT]
    helpDoc _ = "log every predicate evaluated by the SMT solver"

instance SQL.Table DebugEvaluateCondition

whileDebugEvaluateCondition ::
    MonadLog log =>
    InternalVariable variable =>
    NonEmpty (Predicate variable) ->
    log a ->
    log a
whileDebugEvaluateCondition =
    logWhile
        . DebugEvaluateCondition
        . fmap (Predicate.mapVariables (pure toVariableName))

debugEvaluateConditionResult :: MonadLog log => Result -> log ()
debugEvaluateConditionResult = logEntry . DebugEvaluateConditionResult
