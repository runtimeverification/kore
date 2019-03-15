{-|
Module      : Kore.Step.Respresentation.ExpandedPattern
Description : Data structures and functions for manipulating
              ExpandedPatterns, i.e. a representation of patterns
              optimized for the stepper.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Representation.ExpandedPattern
    ( Predicated (..)
    , ExpandedPattern
    , CommonExpandedPattern
    , PredicateSubstitution
    , CommonPredicateSubstitution
    , Kore.Step.Representation.ExpandedPattern.allVariables
    , erasePredicatedTerm
    , bottom
    , bottomOf
    , isBottom
    , isTop
    , Kore.Step.Representation.ExpandedPattern.mapVariables
    , predicateSubstitutionToExpandedPattern
    , substitutionToPredicate
    , toMLPattern
    , top
    , topOf
    , topPredicate
    , bottomPredicate
    , Kore.Step.Representation.ExpandedPattern.fromPurePattern
    , toPredicate
    , Kore.Step.Representation.ExpandedPattern.fromPredicate
    , Kore.Step.Representation.ExpandedPattern.freeVariables
    , freeEpVariables
    ) where

import           Control.DeepSeq
                 ( NFData )
import qualified Control.Monad as Monad
import           Data.Functor
                 ( ($>) )
import qualified Data.Functor.Foldable as Recursive
import           Data.Hashable
import           Data.Monoid
                 ( (<>) )
import qualified Data.Set as Set
import qualified Data.Text.Prettyprint.Doc as Pretty
import           GHC.Generics
                 ( Generic )
import           GHC.Stack
                 ( HasCallStack )

import           Kore.Annotation.Valid
import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.Predicate.Predicate as Predicate
import           Kore.Step.Pattern
import           Kore.TopBottom
                 ( TopBottom (..) )
import           Kore.Unification.Substitution
                 ( Substitution )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unparser
import           Kore.Variables.Free
                 ( freePureVariables, pureAllVariables )

{- | @Predicated@ represents a value conditioned on a predicate.

@Predicated level variable child@ represents a @child@ conditioned on a
@predicate@ and @substitution@ (which is a specialized form of predicate).

The 'Applicative' instance conjoins the predicates and substitutions so that the
result is conditioned on the combined predicates of the inputs. The combined
predicate and substitution are /not/ normalized.

There is intentionally no 'Monad' instance; such an instance would have
quadratic complexity.

 -}
data Predicated level variable child = Predicated
    { term :: child
    , predicate :: !(Predicate level variable)
    , substitution :: !(Substitution level variable)
    }
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

instance
    (Hashable child
    , Hashable (variable level)
    ) => Hashable (Predicated level variable child)

instance
    (NFData child, NFData (variable level)) =>
    NFData (Predicated level variable child)

instance
    ( MetaOrObject level
    , SortedVariable variable
    , Ord (variable level)
    , Show (variable level)
    , Unparse (variable level)
    ) =>
    Applicative (Predicated level variable)
  where
    pure a = Predicated a makeTruePredicate mempty
    (<*>) = Monad.ap

instance
    ( MetaOrObject level
    , SortedVariable variable
    , Ord (variable level)
    , Show (variable level)
    , Unparse (variable level)
    ) =>
    Monad (Predicated level variable)
  where
    return = pure
    {-# INLINE return #-}

    (>>=) predicated1 binding =
        Predicated
            { term = term2
            , predicate = predicate1 `makeAndPredicate` predicate2
            , substitution = substitution1 <> substitution2
            }
      where
        Predicated term1 predicate1 substitution1 = predicated1
        Predicated term2 predicate2 substitution2 = binding term1
    {-# INLINE (>>=) #-}

instance TopBottom term
    => TopBottom (Predicated level variable term)
  where
    isTop Predicated {term, predicate, substitution} =
        isTop term && isTop predicate && isTop substitution
    isBottom Predicated {term, predicate, substitution} =
        isBottom term || isBottom predicate || isBottom substitution

instance
    ( MetaOrObject level
    , SortedVariable variable
    , Ord (variable level)
    , Show (variable level)
    , Unparse (variable level)
    , Unparse child
    ) =>
    Unparse (Predicated level variable child)
  where
    unparse Predicated { term, predicate, substitution } =
        unparseAnd
            (below "/* term: */" (unparse term))
            (unparseAnd
                (below
                    "/* predicate: */"
                    (unparse predicate)
                )
                (below
                    "/* substitution: */"
                    (unparse $ substitutionToPredicate substitution)
                )
            )
      where
        unparseAnd first second =
            "\\and" <> parameters' ["_"] <> arguments' [first, second]
        below first second =
            (Pretty.align . Pretty.vsep) [first, second]

{- | The conjunction of a pattern, predicate, and substitution.

The form of @ExpandedPattern@ is intended to be convenient for Kore execution.

 -}
type ExpandedPattern level variable =
    Predicated level variable (StepPattern level variable)

{- | 'CommonExpandedPattern' particularizes 'ExpandedPattern' to 'Variable'.
-}
type CommonExpandedPattern level = ExpandedPattern level Variable

-- | A predicate and substitution without an accompanying term.
type PredicateSubstitution level variable = Predicated level variable ()

-- | A 'PredicateSubstitution' of the 'Variable' type.
type CommonPredicateSubstitution level = PredicateSubstitution level Variable

{-|'mapVariables' transforms all variables, including the quantified ones,
in an ExpandedPattern.
-}
mapVariables
    :: Ord (variableTo level)
    => (variableFrom level -> variableTo level)
    -> ExpandedPattern level variableFrom
    -> ExpandedPattern level variableTo
mapVariables
    variableMapper
    Predicated { term, predicate, substitution }
  =
    Predicated
        { term = Kore.Step.Pattern.mapVariables variableMapper term
        , predicate = Predicate.mapVariables variableMapper predicate
        , substitution =
            Substitution.mapVariables variableMapper substitution
        }

{-|'allVariables' extracts all variables, including the quantified ones,
from an ExpandedPattern.
-}
allVariables
    :: (Ord (variable level), Unparse (variable level))
    => ExpandedPattern level variable
    -> Set.Set (variable level)
allVariables
    Predicated { term, predicate, substitution }
  =
    pureAllVariables term
    <> Predicate.allVariables predicate
    <> allSubstitutionVars (Substitution.unwrap substitution)
  where
    allSubstitutionVars sub =
        foldl
            (\ x y -> x <> Set.singleton (fst y))
            Set.empty
            sub
        <> foldl
            (\ x y -> x <> pureAllVariables (snd y))
            Set.empty
            sub

-- TODO: This function's name is ridiculous. Refactor and move all
-- PredicateSubstitution stuff to its own file, which will allow this to be
-- called just 'freeVariables'.
{- | Extract the set of free variables from an expanded pattern.

    See also: 'Predicate.freeVariables'.
-}
freeEpVariables
    :: ( MetaOrObject level
       , Ord (variable level)
       , Show (variable level)
       , Unparse (variable level)
       , SortedVariable variable
       )
    => ExpandedPattern level variable
    -> Set.Set (variable level)
freeEpVariables ep@Predicated { term } =
    freePureVariables term
    <> Kore.Step.Representation.ExpandedPattern.freeVariables ep { term = () }

-- | Erase the @Predicated@ 'term' to yield a 'PredicateSubstitution'.
erasePredicatedTerm
    :: Predicated level variable child
    -> PredicateSubstitution level variable
erasePredicatedTerm = (<$) ()

{-|'toMLPattern' converts an ExpandedPattern to a StepPattern.
-}
toMLPattern
    ::  forall level variable.
        ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , HasCallStack
        )
    => ExpandedPattern level variable -> StepPattern level variable
toMLPattern
    Predicated { term, predicate, substitution }
  =
    simpleAnd
        (simpleAnd term predicate)
        (substitutionToPredicate substitution)
  where
    -- TODO: Most likely I defined this somewhere.
    simpleAnd
        :: StepPattern level variable
        -> Predicate level variable
        -> StepPattern level variable
    simpleAnd pattern'@(Recursive.project -> valid :< projected) =
        \case
            PredicateTrue -> pattern'
            PredicateFalse -> mkBottom patternSort
            predicate' ->
                case projected of
                    TopPattern _ ->
                        Predicate.fromPredicate patternSort predicate'
                    BottomPattern _ -> pattern'
                    _ -> mkAnd pattern' (Predicate.fromPredicate patternSort predicate')
      where
        Valid { patternSort } = valid

{-|'bottom' is an expanded pattern that has a bottom condition and that
should become Bottom when transformed to a ML pattern.
-}
bottom :: MetaOrObject level => ExpandedPattern level variable
bottom =
    Predicated
        { term      = mkBottom_
        , predicate = makeFalsePredicate
        , substitution = mempty
        }

{- | An 'ExpandedPattern' where the 'term' is 'Bottom' of the given 'Sort'.

The 'predicate' is set to 'makeFalsePredicate'.

 -}
bottomOf :: MetaOrObject level => Sort level -> ExpandedPattern level variable
bottomOf resultSort =
    Predicated
        { term      = mkBottom resultSort
        , predicate = makeFalsePredicate
        , substitution = mempty
        }

{-|'top' is an expanded pattern that has a top condition and that
should become Top when transformed to a ML pattern.
-}
top :: MetaOrObject level => ExpandedPattern level variable
top =
    Predicated
        { term      = mkTop_
        , predicate = makeTruePredicate
        , substitution = mempty
        }

{- | An 'ExpandedPattern' where the 'term' is 'Top' of the given 'Sort'.
 -}
topOf :: MetaOrObject level => Sort level -> ExpandedPattern level variable
topOf resultSort =
    Predicated
        { term      = mkTop resultSort
        , predicate = makeTruePredicate
        , substitution = mempty
        }

{- | Construct an 'ExpandedPattern' from a 'StepPattern'.

  The resulting @ExpandedPattern@ has a true predicate and an empty
  substitution.

  See also: 'makeTruePredicate', 'pure'

 -}
fromPurePattern
    :: MetaOrObject level
    => StepPattern level variable
    -> ExpandedPattern level variable
fromPurePattern term@(Recursive.project -> _ :< projected) =
    case projected of
        BottomPattern _ -> bottom
        _ ->
            Predicated
                { term
                , predicate = makeTruePredicate
                , substitution = mempty
                }

topPredicate :: MetaOrObject level => PredicateSubstitution level variable
topPredicate = top $> ()

bottomPredicate :: MetaOrObject level => PredicateSubstitution level variable
bottomPredicate = bottom $> ()

{- | Transform a predicate and substitution into a predicate only.

@toPredicate@ is intended for generalizing the 'Predicate' and 'Substitution' of
a 'PredicateSubstition' into only a 'Predicate'; i.e. when @term ~ ()@,

> Predicated level variable term ~ PredicateSubstitution level variable

@toPredicate@ is also used to extract the 'Predicate' and 'Substitution' while
discarding the 'term'.

See also: 'substitutionToPredicate'.

-}
toPredicate
    :: ( MetaOrObject level
       , SortedVariable variable
       , Ord (variable level)
       , Show (variable level)
       , Unparse (variable level)
       )
    => Predicated level variable term
    -> Predicate level variable
toPredicate Predicated { predicate, substitution } =
    makeAndPredicate
        predicate
        (substitutionToPredicate substitution)

{- | Construct a 'PredicateSubstitution' from the given 'Predicate'.

The result has an empty 'Substitution'.

 -}
fromPredicate
    :: Predicate level variable
    -> PredicateSubstitution level variable
fromPredicate predicate =
    Predicated { term = (), predicate, substitution = mempty }

{- | Extract the set of free variables from a predicate and substitution.

    See also: 'Predicate.freeVariables'.
-}

freeVariables
    :: ( MetaOrObject level
       , Ord (variable level)
       , Show (variable level)
       , Unparse (variable level)
       , SortedVariable variable
       )
    => PredicateSubstitution level variable
    -> Set.Set (variable level)
freeVariables = Predicate.freeVariables . toPredicate

predicateSubstitutionToExpandedPattern
    :: MetaOrObject level
    => PredicateSubstitution level variable
    -> ExpandedPattern level variable
predicateSubstitutionToExpandedPattern = (<$) mkTop_
