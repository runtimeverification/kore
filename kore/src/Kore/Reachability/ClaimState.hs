{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Reachability.ClaimState (
    extractRemaining,
    extractUnproven,
    extractStuck,
    retractRewritable,
    isRewritable,
    isRemaining,
    ClaimState (..),
    claimState,
    ClaimStateTransformer (..),
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Debug
import Kore.Unparser (
    Unparse (..),
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

-- | The state of the reachability proof strategy for @claim@.
data ClaimState claim
    = -- | The claim is being proven.
      Claimed !claim
    | -- | The claim is being rewritten, but no rule has applied.
      Remaining !claim
    | -- | The claim was rewritten.
      Rewritten !claim
    | -- | The proof of this claim cannot be completed because the implication
      -- does not hold.
      Stuck !claim
    | -- | The claim was proven.
      Proven
    deriving stock (Eq, Ord, Show)
    deriving stock (Foldable, Functor, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse claim => Pretty (ClaimState claim) where
    pretty (Claimed claim) =
        Pretty.vsep
            [ "claimed:"
            , Pretty.indent 4 $ unparse claim
            ]
    pretty (Remaining claim) =
        Pretty.vsep
            [ "remaining:"
            , Pretty.indent 4 $ unparse claim
            ]
    pretty (Rewritten claim) =
        Pretty.vsep
            [ "rewritten:"
            , Pretty.indent 4 $ unparse claim
            ]
    pretty (Stuck claim) =
        Pretty.vsep
            [ "stuck:"
            , Pretty.indent 4 $ unparse claim
            ]
    pretty Proven = "proven"

{- | Extract the unproven claims of a 'ClaimState'.

Returns 'Nothing' if there is no remaining unproven claim.
-}
extractUnproven :: ClaimState a -> Maybe a
extractUnproven (Claimed t) = Just t
extractUnproven (Rewritten t) = Just t
extractUnproven (Remaining t) = Just t
extractUnproven (Stuck t) = Just t
extractUnproven Proven = Nothing

extractRemaining :: ClaimState a -> Maybe a
extractRemaining (Remaining t) = Just t
extractRemaining _ = Nothing

extractStuck :: ClaimState a -> Maybe a
extractStuck (Stuck a) = Just a
extractStuck _ = Nothing

retractRewritable :: ClaimState a -> Maybe a
retractRewritable (Claimed a) = Just a
retractRewritable (Remaining a) = Just a
retractRewritable _ = Nothing

isRewritable :: ClaimState a -> Bool
isRewritable = isJust . retractRewritable

isRemaining :: ClaimState a -> Bool
isRemaining = isJust . extractRemaining

data ClaimStateTransformer a val = ClaimStateTransformer
    { claimedTransformer :: a -> val
    , remainingTransformer :: a -> val
    , rewrittenTransformer :: a -> val
    , stuckTransformer :: a -> val
    , provenValue :: val
    }

-- | Catamorphism for 'ClaimState'
claimState ::
    ClaimStateTransformer a val ->
    ClaimState a ->
    val
claimState
    ClaimStateTransformer
        { claimedTransformer
        , remainingTransformer
        , rewrittenTransformer
        , stuckTransformer
        , provenValue
        } =
        \case
            Claimed claim -> claimedTransformer claim
            Remaining claim -> remainingTransformer claim
            Rewritten claim -> rewrittenTransformer claim
            Stuck claim -> stuckTransformer claim
            Proven -> provenValue
