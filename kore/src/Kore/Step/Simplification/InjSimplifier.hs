{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

-}
module Kore.Step.Simplification.InjSimplifier
    ( InjSimplifier (..)
    , mkInjSimplifier
    , normalize
    ) where

import qualified Control.Exception as Exception
import qualified Data.Functor.Foldable as Recursive
import qualified GHC.Stack as GHC

import Kore.Attribute.Synthetic
    ( synthesize
    )
import Kore.IndexedModule.SortGraph
    ( SortGraph
    , isSubsortOf
    )
import Kore.Internal.Inj
import Kore.Internal.TermLike
import Pair

data InjSimplifier =
    InjSimplifier
        { isOrderedInj :: forall child. Inj child -> Bool
        -- ^ Is 'injFrom' a proper subsort of 'injTo'?

        , evaluateInj
            :: forall variable
            .  GHC.HasCallStack
            => InternalVariable variable
            => Inj (TermLike variable)
            -> TermLike variable
        {- ^ Apply the triangle axiom to combine an 'Inj' with its 'Inj' child:

        @
            inj{middle, outer}(inj{inner, middle}(_)) = inj{inner, outer}(_)
        @
         -}

        , unifyInj
            :: forall variable
            .  InternalVariable variable
            => Inj (TermLike variable)
            -> Inj (TermLike variable)
            -> Maybe (Inj (Pair (TermLike variable)))
        {- ^ Push down the conjunction of 'Inj':

        @
            inj{lo, hi}(a) ∧ inj{lower, hi}(b)
            ===
            inj{lo, hi}(a ∧ inj{lower, lo}(b))
                where lower < lo
        @
         -}

        , evaluateCeilInj
            :: forall variable
            .  GHC.HasCallStack
            => InternalVariable variable
            => Ceil Sort (Inj (TermLike variable))
            -> Ceil Sort (TermLike variable)
        {- ^ Evaluate the 'Ceil' of 'Inj':

        @
            \ceil{outer, middle}(inj{inner, middle}(x))
            ===
            \ceil{outer, inner}(x)
                where inner < middle
        @
         -}
        }

-- | Ignore 'UnorderedInj' errors in 'evaluateInj' and 'evaluateCeilInj' below.
ignoreUnorderedInj :: Bool
ignoreUnorderedInj =
    -- TODO (thomas.tuegel): Fix the frontend and change to 'False'.
    True

mkInjSimplifier :: SortGraph -> InjSimplifier
mkInjSimplifier sortGraph =
    InjSimplifier { evaluateInj, unifyInj, isOrderedInj, evaluateCeilInj }
  where
    isSubsortOf' = isSubsortOf sortGraph

    isOrderedInj :: forall child. Inj child -> Bool
    isOrderedInj Inj { injFrom, injTo }
      | SortVariableSort _ <- injFrom
      , SortVariableSort _ <- injTo
      =
        -- Assume variable sorts are properly ordered.
        True

      | otherwise = injFrom `isSubsortOf'` injTo

    evaluateInj
        :: forall variable
        .  GHC.HasCallStack
        => InternalVariable variable
        => Inj (TermLike variable)
        -> TermLike variable
    evaluateInj inj
      | not ignoreUnorderedInj, not (isOrderedInj inj) = unorderedInj inj
      | otherwise =
        case injChild inj of
            Inj_ inj'
              | not ignoreUnorderedInj, not (isOrderedInj inj') ->
                unorderedInj inj
              | otherwise ->
                Exception.assert sameConstructor
                . Exception.assert innerSortsAgree
                . synthesize . InjF
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

            _ -> synthesize $ InjF inj

    evaluateCeilInj
        :: forall variable
        .  Ceil Sort (Inj (TermLike variable))
        -> Ceil Sort (TermLike variable)
    evaluateCeilInj Ceil { ceilResultSort, ceilChild = inj }
      | not ignoreUnorderedInj, not (isOrderedInj inj) = unorderedInj inj
      | otherwise =
        Ceil
            { ceilOperandSort = injFrom inj
            , ceilResultSort
            , ceilChild = injChild inj
            }

    unifyInj
        :: forall variable
        .  InternalVariable variable
        => Inj (TermLike variable)
        -> Inj (TermLike variable)
        -> Maybe (Inj (Pair (TermLike variable)))
    unifyInj inj1 inj2
      | injTo1 /= injTo2 = Nothing

      | injFrom1 == injFrom2 =
        Exception.assert (injTo1 == injTo2) $ do
            let child1 = injChild inj1
                child2 = injChild inj2
            pure (Pair child1 child2 <$ inj1)

      | injFrom2 `isSubsortOf'` injFrom1 =
        Exception.assert (injTo1 == injTo2) $ do
            let child1' = injChild inj1
                child2' = evaluateInj inj2 { injTo = injFrom1 }
            pure (Pair child1' child2' <$ inj1)

      | injFrom1 `isSubsortOf'` injFrom2 =
        Exception.assert (injTo1 == injTo2) $ do
            let child1' = evaluateInj inj1 { injTo = injFrom2 }
                child2' = injChild inj2
            pure (Pair child1' child2' <$ inj2)

      | otherwise = Nothing
      where
        Inj { injFrom = injFrom1, injTo = injTo1 } = inj1
        Inj { injFrom = injFrom2, injTo = injTo2 } = inj2

normalize
    :: InternalVariable variable
    => InjSimplifier
    -> TermLike variable
    -> TermLike variable
normalize InjSimplifier { evaluateInj } =
    Recursive.fold worker
  where
    worker (_ :< termLikeF) =
        case termLikeF of
            InjF inj -> evaluateInj inj
            _ -> synthesize termLikeF
