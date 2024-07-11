{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Log.WarnBottom (
    WarnClaimRHSIsBottom (..),
    warnClaimRHSIsBottom,
    WarnConfigIsBottom (..),
    warnConfigIsBottom,
) where

import Kore.Attribute.SourceLocation
import Kore.Internal.Pattern (Pattern)
import Kore.Rewrite.ClaimPattern
import Kore.Rewrite.RewritingVariable (RewritingVariableName)
import Kore.Unparser (unparse)
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

newtype WarnClaimRHSIsBottom = WarnClaimRHSIsBottom {claim :: ClaimPattern}
    deriving stock (Show)

instance Pretty WarnClaimRHSIsBottom where
    pretty WarnClaimRHSIsBottom{claim} =
        Pretty.hsep
            [ "The right-hand side of the claim is bottom:"
            , prettySourceLocation claim
            ]

instance Entry WarnClaimRHSIsBottom where
    entrySeverity _ = Warning
    helpDoc _ = "warn when the right-hand side of a claim is bottom"
    oneLineDoc WarnClaimRHSIsBottom{claim} = prettySourceLocation claim
    oneLineContextDoc _ = single CtxWarn

prettySourceLocation :: ClaimPattern -> Pretty.Doc ann
prettySourceLocation = Pretty.pretty @SourceLocation . from

warnClaimRHSIsBottom ::
    MonadLog log =>
    ClaimPattern ->
    log ()
warnClaimRHSIsBottom = logEntry . WarnClaimRHSIsBottom

newtype WarnConfigIsBottom = WarnConfigIsBottom
    { config :: Pattern RewritingVariableName
    }
    deriving stock (Show)

instance Pretty WarnConfigIsBottom where
    pretty WarnConfigIsBottom{config} =
        Pretty.vsep
            [ "The following configuration has been simplified to bottom:"
            , Pretty.indent 4 $ unparse config
            ]

instance Entry WarnConfigIsBottom where
    entrySeverity _ = Warning
    helpDoc _ = "warn when the configuration is bottom"
    oneLineDoc _ = "A configuration has been simplified to bottom."
    oneLineContextDoc _ = single CtxWarn

warnConfigIsBottom ::
    MonadLog log =>
    Pattern RewritingVariableName ->
    log ()
warnConfigIsBottom = logEntry . WarnConfigIsBottom
