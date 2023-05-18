{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Kore.Internal.MultiAnd
Description : Data structures and functions for manipulating
              And with any number of children.
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Internal.MultiAnd (
    MultiAnd,
    top,
    make,
    fromTermLike,
    singleton,
    map,
    traverse,
    distributeAnd,
    traverseOr,
    traverseOrAnd,
    size,
) where

import Data.Binary (Binary)
import Data.Functor.Foldable qualified as Recursive
import Data.List (genericLength)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable qualified as Traversable
import Debug
import GHC.Generics qualified as GHC
import GHC.Natural (Natural)
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables (..),
 )
import Kore.Internal.MultiOr (
    MultiOr,
    patternToMaybeBool,
 )
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.TermLike (
    TermLike,
    TermLikeF (..),
 )
import Kore.Internal.Variable
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser (
    unparseAssoc',
 )
import Logic qualified
import Prelude.Kore hiding (
    map,
    traverse,
 )
import Pretty (
    Pretty (..),
 )

-- | 'MultiAnd' is a Matching logic and of its children

{- TODO (virgil): Make 'getMultiAnd' a non-empty list ("Data.NonEmpty").

An empty 'MultiAnd' corresponding to 'Top' actually discards information
about the sort of its child patterns! That is a problem for simplification,
which should preserve pattern sorts.

A non-empty 'MultiAnd' would also have a nice symmetry between 'Top' and
'Bottom' patterns.
-}

data MultiAnd child
    = MultiAndTop
    | MultiAndBottom child
    | MultiAnd (Set child)
    deriving stock (Eq, Ord, Show, Foldable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Hashable child => Hashable (MultiAnd child) where
    hashWithSalt salt = \case
        MultiAndTop -> salt `hashWithSalt` (0 :: Int)
        MultiAndBottom child -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` child
        MultiAnd children -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` (Set.toList children)

instance TopBottom (MultiAnd child) where
    isTop MultiAndTop = True
    isTop _ = False
    isBottom (MultiAndBottom _) = True
    isBottom _ = False

instance
    (Ord variable, HasFreeVariables a variable) =>
    HasFreeVariables (MultiAnd a) variable
    where
    freeVariables = foldMap' freeVariables

instance Debug child => Debug (MultiAnd child)

instance (Debug child, Diff child) => Diff (MultiAnd child)

instance Pretty child => Pretty (MultiAnd child) where
    pretty = unparseAssoc' "\\and{_}" "\\top{_}()" . (<$>) pretty . toList
    {-# INLINE pretty #-}

instance Ord child => Semigroup (MultiAnd child) where
    MultiAndTop <> b = b
    a <> MultiAndTop = a
    a@(MultiAndBottom _) <> _ = a
    _ <> b@(MultiAndBottom _) = b
    (MultiAnd a) <> (MultiAnd b) = MultiAnd (a <> b)

instance Ord child => Monoid (MultiAnd child) where
    mempty = MultiAndTop

instance
    InternalVariable variable =>
    From (TermLike variable) (MultiAnd (TermLike variable))
    where
    from = fromTermLike
    {-# INLINE from #-}

top :: MultiAnd term
top = MultiAndTop

-- | 'make' constructs a normalized 'MultiAnd'.
singleton :: TopBottom term => term -> MultiAnd term
singleton term
    | isTop term = MultiAndTop
    | isBottom term = MultiAndBottom term
    | otherwise = MultiAnd $ Set.singleton term

size :: MultiAnd a -> Natural
size = genericLength . toList

{- | Simplify the conjunction.

The arguments are simplified by filtering on @\\top@ and @\\bottom@. The
idempotency property of conjunction (@\\and(φ,φ)=φ@) is applied to remove
duplicated items from the result.

The idempotency property of conjunction (@\\and(φ,φ)=φ@) is applied to remove
duplicated items from the result.

Note: Items are compared with their Ord instance. This does not attempt
to account separately for things like α-equivalence, so, if that is not
included in the Ord instance, items containing @\\forall@ and
@\\exists@ may be considered inequal although they are equivalent in
a logical sense.
-}

{- | 'make' constructs a simplified/normalized 'MultiAnd'. It simplifies a set of children according to the `patternToMaybeBool`
function which evaluates to true/false/unknown.
-}
make ::
    (Ord child, TopBottom child, Foldable f) =>
    f child ->
    MultiAnd child
make ~patts = foldr go mempty patts
  where
    go element ~mand =
        case patternToMaybeBool element of
            Just False -> MultiAndBottom element
            Just True -> mand
            Nothing -> case mand of
                MultiAnd es -> MultiAnd $ Set.insert element es
                MultiAndTop -> MultiAnd $ Set.singleton element
                bottom -> bottom

fromTermLike ::
    InternalVariable variable =>
    TermLike variable ->
    MultiAnd (TermLike variable)
fromTermLike termLike =
    case Recursive.project termLike of
        _ :< AndF andF -> foldMap fromTermLike andF
        _ -> make [termLike]

map ::
    Ord child2 =>
    TopBottom child2 =>
    (child1 -> child2) ->
    MultiAnd child1 ->
    MultiAnd child2
map f = make . fmap f . toList
{-# INLINE map #-}

traverse ::
    Ord child2 =>
    TopBottom child2 =>
    Applicative f =>
    (child1 -> f child2) ->
    MultiAnd child1 ->
    f (MultiAnd child2)
traverse f = fmap make . Traversable.traverse f . toList
{-# INLINE traverse #-}

distributeAnd ::
    Ord term =>
    TopBottom term =>
    MultiAnd (MultiOr term) ->
    MultiOr (MultiAnd term)
distributeAnd multiAnd =
    MultiOr.observeAll $ traverse Logic.scatter multiAnd
{-# INLINE distributeAnd #-}

traverseOr ::
    Ord child2 =>
    TopBottom child2 =>
    Applicative f =>
    (child1 -> f (MultiOr child2)) ->
    MultiAnd child1 ->
    f (MultiOr (MultiAnd child2))
traverseOr f = fmap distributeAnd . traverse f
{-# INLINE traverseOr #-}

traverseOrAnd ::
    Ord child2 =>
    Applicative f =>
    (child1 -> f (MultiOr (MultiAnd child2))) ->
    MultiOr (MultiAnd child1) ->
    f (MultiOr (MultiAnd child2))
traverseOrAnd f = MultiOr.traverseOr (fmap (MultiOr.map fold) . traverseOr f)
{-# INLINE traverseOrAnd #-}
