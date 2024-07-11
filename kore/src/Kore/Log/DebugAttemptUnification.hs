{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugAttemptUnification (
    DebugAttemptUnification (..),
    debugAttemptUnification,
) where

import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Internal.TermLike (
    InternalVariable,
    TermLike,
    VariableName,
    toVariableName,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Unparser (
    unparse,
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

data DebugAttemptUnification = DebugAttemptUnification {term1, term2 :: TermLike VariableName}
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Entry DebugAttemptUnification where
    entrySeverity _ = Debug
    contextDoc _ = Just "while attempting unification"
    oneLineDoc _ = "DebugAttemptUnification"
    oneLineContextDoc _ = single CtxUnify
    helpDoc _ = "log unification attempts"

instance Pretty DebugAttemptUnification where
    pretty DebugAttemptUnification{term1, term2} =
        Pretty.vsep
            [ "Attempting to unify"
            , Pretty.indent 4 $ unparse term1
            , Pretty.indent 2 "with"
            , Pretty.indent 4 $ unparse term2
            ]

debugAttemptUnification ::
    MonadLog log =>
    InternalVariable variable =>
    TermLike variable ->
    TermLike variable ->
    log a ->
    log a
debugAttemptUnification term1' term2' =
    logWhile DebugAttemptUnification{term1, term2}
  where
    mapVariables = TermLike.mapVariables (pure toVariableName)
    term1 = mapVariables term1'
    term2 = mapVariables term2'
