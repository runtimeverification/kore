{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Simplify.InjSimplifier (
    InjSimplifier (..),
    UnifyInj (..),
    InjPair (..),
    mkInjSimplifier,
    normalize,
) where

import Data.Binary (Binary)
import Data.Functor.Foldable qualified as Recursive
import Data.Set qualified as Set
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Synthetic (
    synthesize,
 )
import Kore.Debug
import Kore.IndexedModule.SortGraph (
    SortGraph,
    isSubsortOf,
    subsortsOf,
 )
import Kore.Internal.Inj
import Kore.Internal.TermLike
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Pair
import Prelude.Kore

{- | Two 'Inj' are 'Distinct' if they are known not to unify.

Otherwise, 'unifyInj' may return 'Unknown'.
-}
data Distinct = Distinct | Unknown
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

data InjPair variable = InjPair {inj1, inj2 :: Inj (TermLike variable)}

data UnifyInj a
    = -- | The children of the injections can be unified directly because the
      -- injections have the same inner and outer sorts.
      UnifyInjDirect a
    | -- | The right injection's inner sort is a subsort of the left injection's,
      -- so unification can proceed by splitting the right injection.
      UnifyInjSplit a
    | -- | The injections are known to be distinct because there is no subsort
      -- relation between their inner sorts.
      UnifyInjDistinct a
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

data InjSimplifier = InjSimplifier
    { isOrderedInj :: forall child. Inj child -> Bool
    -- ^ Is 'injFrom' a proper subsort of 'injTo'?
    , evaluateInj ::
        forall variable.
        HasCallStack =>
        InternalVariable variable =>
        Inj (TermLike variable) ->
        TermLike variable
    -- ^ Apply the triangle axiom to combine an 'Inj' with its 'Inj' child:
    --
    --        @
    --            inj{middle, outer}(inj{inner, middle}(_)) = inj{inner, outer}(_)
    --        @
    , matchInjs ::
        forall variable.
        HasCallStack =>
        InternalVariable variable =>
        Inj (TermLike variable) ->
        Inj (TermLike variable) ->
        Maybe (UnifyInj (InjPair variable))
    , unifyInjs ::
        forall variable.
        InternalVariable variable =>
        UnifyInj (InjPair variable) ->
        Maybe (Inj (Pair (TermLike variable)))
    -- ^ Push down the conjunction of 'Inj':
    --
    --        @
    --            inj{lo, hi}(a) ∧ inj{lower, hi}(b)
    --            ===
    --            inj{lo, hi}(a ∧ inj{lower, lo}(b))
    --                where lower < lo
    --        @
    --
    --        Returns 'Distinct' if the sort injections cannot match, or 'Unknown' if
    --        further simplification could produce matching injections.
    , evaluateCeilInj ::
        forall variable.
        HasCallStack =>
        InternalVariable variable =>
        Ceil Sort (Inj (TermLike variable)) ->
        Ceil Sort (TermLike variable)
    -- ^ Evaluate the 'Ceil' of 'Inj':
    --
    --        @
    --            \ceil{outer, middle}(inj{inner, middle}(x))
    --            ===
    --            \ceil{outer, inner}(x)
    --                where inner < middle
    --        @
    , injectTermTo ::
        forall variable.
        HasCallStack =>
        InternalVariable variable =>
        Inj () ->
        TermLike variable ->
        Sort ->
        TermLike variable
    -- ^ Injects a term to a given sort. Applies 'evaluateInj' to keep
    --        the injection simplified.
    }

-- | Ignore 'UnorderedInj' errors in 'evaluateInj' and 'evaluateCeilInj' below.
ignoreUnorderedInj :: Bool
ignoreUnorderedInj =
    -- TODO (thomas.tuegel): Fix the frontend and change to 'False'.
    True

mkInjSimplifier :: SortGraph -> InjSimplifier
mkInjSimplifier sortGraph =
    InjSimplifier
        { evaluateInj
        , matchInjs
        , unifyInjs
        , isOrderedInj
        , evaluateCeilInj
        , injectTermTo
        }
  where
    isSubsortOf' = isSubsortOf sortGraph

    isOrderedInj :: forall child. Inj child -> Bool
    isOrderedInj Inj{injFrom, injTo}
        | SortVariableSort _ <- injFrom
        , SortVariableSort _ <- injTo =
            -- Assume variable sorts are properly ordered.
            True
        | otherwise = injFrom `isSubsortOf'` injTo

    evaluateInj ::
        forall variable.
        HasCallStack =>
        InternalVariable variable =>
        Inj (TermLike variable) ->
        TermLike variable
    evaluateInj inj
        | injFrom inj == injTo inj = injChild inj
        | not ignoreUnorderedInj, not (isOrderedInj inj) = unorderedInj inj
        | otherwise =
            synthesize . InjF $ case injChild inj of
                Inj_ inj'
                    | not ignoreUnorderedInj
                    , not (isOrderedInj inj') ->
                        unorderedInj inj
                    | otherwise ->
                        assert sameConstructor
                            . assert innerSortsAgree
                            $ Inj
                                { injConstructor = injConstructor inj
                                , injTo = injTo inj
                                , injFrom = injFrom inj'
                                , injChild = injChild inj'
                                , injAttributes = injAttributes inj
                                }
                  where
                    sameConstructor = injConstructor inj == injConstructor inj'
                    innerSortsAgree = injFrom inj == injTo inj'
                _ -> inj

    matchInjs ::
        forall variable.
        Inj (TermLike variable) ->
        Inj (TermLike variable) ->
        Maybe (UnifyInj (InjPair variable))
    matchInjs inj1 inj2
        | injTo1 /= injTo2 = distinct
        | injFrom1 == injFrom2 = direct
        | injFrom2 `isSubsortOf'` injFrom1 = splitRight
        | injFrom1 `isSubsortOf'` injFrom2 = splitLeft
        -- If the child patterns are simplifiable, then they could eventually be
        -- simplified to produce matching sort injections, but if they are
        -- non-simplifiable, then they will never match.
        | hasConstructorLikeTop (injChild inj1) = distinct
        | hasConstructorLikeTop (injChild inj2) = distinct
        -- Even if the child patterns are simplifiable, if they do not have any
        -- common subsorts, then they will never simplify to produce matching sort
        -- injections.
        | Set.disjoint subsorts1 subsorts2 = distinct
        | otherwise = Nothing
      where
        Inj{injFrom = injFrom1, injTo = injTo1} = inj1
        Inj{injFrom = injFrom2, injTo = injTo2} = inj2
        subsorts1 = subsortsOf sortGraph injFrom1
        subsorts2 = subsortsOf sortGraph injFrom2
        distinct = Just (UnifyInjDistinct InjPair{inj1, inj2})
        direct = Just (UnifyInjDirect InjPair{inj1, inj2})
        splitRight = Just (UnifyInjSplit InjPair{inj1, inj2})
        splitLeft = Just (UnifyInjSplit InjPair{inj1 = inj2, inj2 = inj1})

    evaluateCeilInj ::
        forall variable.
        Ceil Sort (Inj (TermLike variable)) ->
        Ceil Sort (TermLike variable)
    evaluateCeilInj Ceil{ceilResultSort, ceilChild = inj}
        | not ignoreUnorderedInj, not (isOrderedInj inj) = unorderedInj inj
        | otherwise =
            Ceil
                { ceilOperandSort = injFrom inj
                , ceilResultSort
                , ceilChild = injChild inj
                }

    unifyInjs ::
        forall variable.
        InternalVariable variable =>
        UnifyInj (InjPair variable) ->
        Maybe (Inj (Pair (TermLike variable)))
    unifyInjs unify =
        case unify of
            (UnifyInjDirect injPair) ->
                assert (injTo1 == injTo2) $ do
                    let child1 = injChild inj1
                        child2 = injChild inj2
                    Just (Pair child1 child2 <$ inj1)
              where
                InjPair{inj1, inj2} = injPair
                Inj{injTo = injTo1} = inj1
                Inj{injTo = injTo2} = inj2
            (UnifyInjSplit injPair) ->
                assert (injTo1 == injTo2) $ do
                    let child1' = injChild inj1
                        child2' = evaluateInj inj2{injTo = injFrom1}
                    Just (Pair child1' child2' <$ inj1)
              where
                InjPair{inj1, inj2} = injPair
                Inj{injFrom = injFrom1, injTo = injTo1} = inj1
                Inj{injTo = injTo2} = inj2
            UnifyInjDistinct _ -> Nothing

    injectTermTo injProto injChild injTo =
        evaluateInj injProto{injFrom, injTo, injChild}
      where
        injFrom = termLikeSort injChild

normalize ::
    InjSimplifier ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
normalize InjSimplifier{evaluateInj} =
    Recursive.fold worker
  where
    worker (_ :< termLikeF) =
        case termLikeF of
            InjF inj -> evaluateInj inj
            _ -> synthesize termLikeF
