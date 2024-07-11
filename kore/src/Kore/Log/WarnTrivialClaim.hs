{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.WarnTrivialClaim (
    WarnTrivialClaim (..),
    warnProvenClaimZeroDepth,
    warnTrivialClaimRemoved,
) where

import Kore.Attribute.SourceLocation
import Kore.Log.InfoProofDepth
import Kore.Reachability.SomeClaim
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

data WarnTrivialClaim
    = -- | Warning when a claim is proved without rewriting.
      WarnProvenClaimZeroDepth SomeClaim
    | -- | Warning when a claim is proved during initialization.
      WarnTrivialClaimRemoved SomeClaim
    deriving stock (Show)

instance Pretty WarnTrivialClaim where
    pretty (WarnProvenClaimZeroDepth rule) =
        Pretty.hsep
            [ "Claim proven without rewriting at:"
            , Pretty.pretty (from rule :: SourceLocation)
            ]
    pretty (WarnTrivialClaimRemoved rule) =
        Pretty.vsep
            [ Pretty.hsep
                [ "Claim proven during initialization:"
                , Pretty.pretty (from rule :: SourceLocation)
                ]
            , "The left-hand side of the claim may be undefined."
            ]

instance Entry WarnTrivialClaim where
    entrySeverity _ = Warning
    oneLineDoc (WarnProvenClaimZeroDepth claim) =
        Pretty.hsep
            [ "WarnProvenClaimZeroDepth"
            , Pretty.colon
            , Pretty.pretty @SourceLocation $ from claim
            ]
    oneLineDoc (WarnTrivialClaimRemoved claim) =
        Pretty.hsep
            [ "WarnTrivialClaimRemoved:"
            , Pretty.colon
            , Pretty.pretty @SourceLocation $ from claim
            ]
    oneLineContextDoc _ = single CtxWarn
    helpDoc _ = "warn when a claim is proven without taking any steps"

warnProvenClaimZeroDepth ::
    MonadLog log =>
    ProofDepth ->
    SomeClaim ->
    log ()
warnProvenClaimZeroDepth (ProofDepth depth) rule =
    when (depth == 0) $ logEntry (WarnProvenClaimZeroDepth rule)

warnTrivialClaimRemoved ::
    MonadLog log =>
    SomeClaim ->
    log ()
warnTrivialClaimRemoved rule =
    logEntry (WarnTrivialClaimRemoved rule)
