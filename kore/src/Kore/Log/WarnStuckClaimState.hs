{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Log.WarnStuckClaimState (
    WarnStuckClaimState (..),
    warnStuckClaimStateTermsUnifiable,
    warnStuckClaimStateTermsNotUnifiable,
    warnStuckClaimStateBottomLHS,
) where

import Kore.Attribute.SourceLocation
import Kore.Reachability.SomeClaim
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

{- | @WarnStuckClaimState@ is emitted when a proof gets stuck.

The warning message distinguishes for the user the ways that a proof can be stuck.
-}
data WarnStuckClaimState
    = -- | The terms of the left- and right-hand sides do not unify,
      -- and the left-hand side cannot be rewritten any further.
      TermsUnifiableStuck SomeClaim
    | -- | The left- and right-hand side terms are unifiable, but the left-hand side
      -- condition does not imply the right-hand side condition.
      TermsNotUnifiableStuck SomeClaim
    | -- | The left-hand side of the claim has been simplified to bottom.
      BottomLHS SomeClaim
    deriving stock (Show)

instance Pretty WarnStuckClaimState where
    pretty (TermsUnifiableStuck claim) =
        Pretty.hsep
            [ "The configuration's term unifies with the destination's term,\
              \ but the implication check between the conditions has failed. Location:"
            , Pretty.pretty (from claim :: SourceLocation)
            ]
    pretty (TermsNotUnifiableStuck claim) =
        Pretty.hsep
            [ "The configuration's term doesn't unify with the destination's term\
              \ and the configuration cannot be rewritten further. Location:"
            , Pretty.pretty (from claim :: SourceLocation)
            ]
    pretty (BottomLHS claim) =
        Pretty.hsep
            [ "The left-hand side of the claim has been simplified to bottom. Location:"
            , Pretty.pretty (from claim :: SourceLocation)
            ]

instance Entry WarnStuckClaimState where
    entrySeverity _ = Warning
    oneLineDoc (TermsUnifiableStuck claim) =
        Pretty.hsep
            [ "TermsUnifiableStuck"
            , Pretty.colon
            , Pretty.pretty @SourceLocation $ from claim
            ]
    oneLineDoc (TermsNotUnifiableStuck claim) =
        Pretty.hsep
            [ "TermsNotUnifiableStuck:"
            , Pretty.colon
            , Pretty.pretty @SourceLocation $ from claim
            ]
    oneLineDoc (BottomLHS claim) =
        Pretty.hsep
            [ "BottomLHS:"
            , Pretty.colon
            , Pretty.pretty @SourceLocation $ from claim
            ]
    oneLineContextDoc _ = single CtxWarn

    helpDoc _ = "distinguish the ways a proof can become stuck"

warnStuckClaimStateTermsUnifiable :: MonadLog log => SomeClaim -> log ()
warnStuckClaimStateTermsUnifiable = logEntry . TermsUnifiableStuck

warnStuckClaimStateTermsNotUnifiable :: MonadLog log => SomeClaim -> log ()
warnStuckClaimStateTermsNotUnifiable = logEntry . TermsNotUnifiableStuck

warnStuckClaimStateBottomLHS :: MonadLog log => SomeClaim -> log ()
warnStuckClaimStateBottomLHS = logEntry . BottomLHS
