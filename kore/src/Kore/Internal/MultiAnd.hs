{-|
Module      : Kore.Internal.MultiAnd
Description : Data structures and functions for manipulating
              And with any number of children.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE UndecidableInstances #-}

module Kore.Internal.MultiAnd
    ( MultiAnd
    , make
    , toPredicate
    , fromPredicate
    , fromTermLike
    , singleton
    , map
    , traverse
    , topPredicate
    , topPredicate_
    , fromPredicates
    ) where

import Prelude.Kore hiding
    ( map
    , traverse
    )

import Control.DeepSeq
    ( NFData
    )
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Foldable as Recursive
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set.NonEmpty as Set
import qualified Data.Traversable as Traversable
import qualified Generics.SOP as SOP
import qualified GHC.Exts as GHC
import qualified GHC.Generics as GHC

import Debug
import Kore.Internal.Predicate
    ( Predicate
    , getMultiAndPredicate
    , makeAndPredicate
    , makeTruePredicate
    , makeTruePredicate_
    )
import Kore.Internal.TermLike
    ( pattern And_
    , Sort
    , TermLike
    , TermLikeF (..)
    )
import Kore.Internal.Variable
import Kore.TopBottom
    ( TopBottom (..)
    )

{-| 'MultiAnd' is a Matching logic and of its children

-}
{- TODO (virgil): Make 'getMultiAnd' a non-empty list ("Data.NonEmpty").

An empty 'MultiAnd' corresponding to 'Top' actually discards information
about the sort of its child patterns! That is a problem for simplification,
which should preserve pattern sorts.

A non-empty 'MultiAnd' would also have a nice symmetry between 'Top' and
'Bottom' patterns.
-}
newtype MultiAnd child = MultiAnd { getMultiAnd :: NonEmpty child }
    deriving (Eq, Ord, Show)
    deriving (Foldable)
    deriving (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving newtype (GHC.IsList)

instance TopBottom child => TopBottom (MultiAnd child) where
    isTop (MultiAnd (child :| [])) = isTop child
    isTop _ = False
    isBottom (MultiAnd (child :| [])) = isBottom child
    isBottom _ = False

instance Debug child => Debug (MultiAnd child)

instance (Debug child, Diff child) => Diff (MultiAnd child)

instance (Ord child, TopBottom child) => Semigroup (MultiAnd child) where
    (MultiAnd a) <> (MultiAnd b) = make (a <> b)

instance
    InternalVariable variable
    => From (MultiAnd (Predicate variable)) (Predicate variable)
  where
    from = toPredicate
    {-# INLINE from #-}

instance
    InternalVariable variable
    => From (Predicate variable) (MultiAnd (Predicate variable))
  where
    from = fromPredicate
    {-# INLINE from #-}

instance
    InternalVariable variable
    => From (TermLike variable) (MultiAnd (TermLike variable))
  where
    from = fromTermLike
    {-# INLINE from #-}

{-| 'AndBool' is an some sort of Bool data type used when evaluating things
inside a 'MultiAnd'.
-}
-- TODO(virgil): Refactor, this is the same as OrBool. Make it a
-- Top | Bottom | Other or a Maybe Bool.
data AndBool = AndTrue | AndFalse | AndUnknown

{-|Does a very simple attempt to check whether a pattern
is top or bottom.
-}
-- TODO(virgil): Refactor, this is the same as patternToOrBool
patternToAndBool
    :: TopBottom term
    => term -> AndBool
patternToAndBool patt
  | isTop patt = AndTrue
  | isBottom patt = AndFalse
  | otherwise = AndUnknown

{-| 'make' constructs a normalized 'MultiAnd'.
-}
make :: (Ord term, TopBottom term) => NonEmpty term -> MultiAnd term
make patts = filterAnd (MultiAnd patts)

{-| 'make' constructs a normalized 'MultiAnd'.
-}
singleton :: (Ord term, TopBottom term) => term -> MultiAnd term
singleton term = make (term :| [])

{- | Simplify the conjunction.

The arguments are simplified by filtering on @\\top@ and @\\bottom@. The
idempotency property of conjunction (@\\and(φ,φ)=φ@) is applied to remove
duplicated items from the result.

See also: 'filterUnique'
-}
filterAnd
    :: (Ord term, TopBottom term)
    => MultiAnd term
    -> MultiAnd term
filterAnd =
    filterGeneric patternToAndBool . filterUnique


{- | Simplify the conjunction by eliminating duplicate elements.

The idempotency property of conjunction (@\\and(φ,φ)=φ@) is applied to remove
duplicated items from the result.

Note: Items are compared with their Ord instance. This does not attempt
to account separately for things like α-equivalence, so, if that is not
included in the Ord instance, items containing @\\forall@ and
@\\exists@ may be considered inequal although they are equivalent in
a logical sense.

-}
filterUnique :: Ord a => MultiAnd a -> MultiAnd a
filterUnique = MultiAnd . Set.toList . Set.fromList . getMultiAnd

{-| 'filterGeneric' simplifies a MultiAnd according to a function which
evaluates its children to true/false/unknown.
-}
filterGeneric
    :: (child -> AndBool)
    -> MultiAnd child
    -> MultiAnd child
filterGeneric andFilter (MultiAnd patts) =
    undefined
--     go andFilter [] patts
--   where
--     go  :: (child -> AndBool)
--         -> NonEmpty child
--         -> NonEmpty child
--         -> MultiAnd child
--     go _ filtered [] = MultiAnd (reverse filtered)
--     go filterAnd' filtered (element:unfiltered) =
--         case filterAnd' element of
--             AndFalse -> MultiAnd [element]
--             AndTrue -> go filterAnd' filtered unfiltered
--             AndUnknown -> go filterAnd' (element:filtered) unfiltered

toPredicate
    :: InternalVariable variable
    => MultiAnd (Predicate variable)
    -> Predicate variable
toPredicate (MultiAnd predicates) =
    foldr1 makeAndPredicate predicates

fromPredicate
    :: Ord variable
    => Predicate variable
    -> MultiAnd (Predicate variable)
fromPredicate = make . getMultiAndPredicate

fromTermLike
    :: InternalVariable variable
    => TermLike variable
    -> MultiAnd (TermLike variable)
fromTermLike termLike =
    case termLike of
        And_ _ child1 child2 -> fromTermLike child1 <> fromTermLike child2
        _                    -> make (termLike :| [])

map
    :: Ord child2
    => TopBottom child2
    => (child1 -> child2)
    -> MultiAnd child1
    -> MultiAnd child2
map f = make . fmap f . getMultiAnd
{-# INLINE map #-}

traverse
    :: Ord child2
    => TopBottom child2
    => Applicative f
    => (child1 -> f child2)
    -> MultiAnd child1
    -> f (MultiAnd child2)
traverse f = fmap make . Traversable.traverse f . getMultiAnd
{-# INLINE traverse #-}

topPredicate_
    :: InternalVariable variable
    => MultiAnd (Predicate variable)
topPredicate_ = singleton makeTruePredicate_

topPredicate
    :: InternalVariable variable
    => Sort
    -> MultiAnd (Predicate variable)
topPredicate sort = singleton (makeTruePredicate sort)

fromPredicates
    :: InternalVariable variable
    => [Predicate variable]
    -> MultiAnd (Predicate variable)
fromPredicates [] = topPredicate_
fromPredicates (predicate : rest) = make (predicate :| rest)
