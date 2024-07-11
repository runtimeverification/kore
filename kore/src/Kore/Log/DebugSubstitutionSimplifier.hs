{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugSubstitutionSimplifier (
    DebugSubstitutionSimplifier (..),
    whileDebugSubstitutionSimplifier,
    debugSubstitutionSimplifierResult,
) where

import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import SQL qualified

data DebugSubstitutionSimplifier
    = WhileSimplifySubstitution
    | SubstitutionSimplifierResult
    deriving stock (Show)
    deriving stock (GHC.Generic)

instance SOP.Generic DebugSubstitutionSimplifier

instance SOP.HasDatatypeInfo DebugSubstitutionSimplifier

instance Pretty DebugSubstitutionSimplifier where
    pretty WhileSimplifySubstitution = "Simplifying substitution"
    pretty SubstitutionSimplifierResult = "Non-\\bottom result"

instance Entry DebugSubstitutionSimplifier where
    entrySeverity _ = Debug
    contextDoc _ = Just "while simplifying substitution"
    oneLineDoc = pretty . show
    oneLineContextDoc _ = single CtxSubstitution
    helpDoc _ = "log non-\\bottom results when normalizing unification solutions"

instance SQL.Table DebugSubstitutionSimplifier

whileDebugSubstitutionSimplifier ::
    MonadLog log =>
    log a ->
    log a
whileDebugSubstitutionSimplifier =
    logWhile WhileSimplifySubstitution

debugSubstitutionSimplifierResult ::
    MonadLog log =>
    log ()
debugSubstitutionSimplifierResult = logEntry SubstitutionSimplifierResult
