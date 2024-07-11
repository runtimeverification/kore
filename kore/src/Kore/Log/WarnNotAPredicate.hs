{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Log.WarnNotAPredicate (
    WarnNotAPredicate (..),
    warnNotAPredicate,
    -- re-export
    Severity (..),
) where

import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.Pattern (Pattern)
import Kore.Internal.Predicate (Predicate)
import Kore.Rewrite.RewritingVariable
import Log hiding (severity)
import Prelude.Kore
import Pretty (Pretty)
import Pretty qualified

data WarnNotAPredicate = WarnNotAPredicate
    { severity :: Severity
    , original :: Predicate RewritingVariableName
    , notAPredicate :: Pattern RewritingVariableName
    }
    deriving stock (Show)

instance Pretty WarnNotAPredicate where
    pretty WarnNotAPredicate{original, notAPredicate} =
        Pretty.vsep
            [ "Predicate simplification produced a non-predicate:"
            , "Original predicate:"
            , (Pretty.indent 4) (Pretty.pretty original)
            , "Output contained"
            , (Pretty.indent 4) (Pretty.pretty notAPredicate)
            ]

instance Entry WarnNotAPredicate where
    entrySeverity = severity
    oneLineDoc =
        Pretty.pretty
            . unwords
            . lines
            . Pretty.renderString
            . Pretty.layoutOneLine
            . Pretty.pretty
    oneLineContextDoc WarnNotAPredicate{severity} = [severityToContext severity]
    helpDoc _ =
        "warn when a predicate simplification produces a non-predicate"

warnNotAPredicate ::
    MonadLog log =>
    Severity ->
    Predicate RewritingVariableName ->
    Pattern RewritingVariableName ->
    log ()
warnNotAPredicate severity original notAPredicate
    | Conditional.isPredicate notAPredicate = pure ()
    | otherwise =
        logEntry WarnNotAPredicate{severity, original, notAPredicate}
