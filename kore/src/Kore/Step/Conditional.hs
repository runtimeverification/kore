{-|
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

Representation of conditional terms.
-}
module Kore.Step.Conditional
    ( Conditional (..)
    , withoutTerm
    , withCondition
    , andCondition
    , fromPredicate
    , fromSubstitution
    , andPredicate
    , Kore.Step.Conditional.freeVariables
    , toPredicate
    , Kore.Step.Conditional.mapVariables
    ) where

import           Control.DeepSeq
                 ( NFData )
import           Data.Hashable
import           Data.Monoid
                 ( (<>) )
import           Data.Set
                 ( Set )
import qualified Data.Text.Prettyprint.Doc as Pretty
import           GHC.Generics
                 ( Generic )

import           Kore.AST.Pure
import           Kore.Predicate.Predicate
                 ( Predicate )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.TopBottom
                 ( TopBottom (..) )
import           Kore.Unification.Substitution
                 ( Substitution )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unparser

{- | @Conditional@ represents a value conditioned on a predicate.

@Conditional level variable child@ represents a @child@ conditioned on a
@predicate@ and @substitution@ (which is a specialized form of predicate).

The 'Applicative' instance conjoins the predicates and substitutions so that the
result is conditioned on the combined predicates of the inputs. The combined
predicate and substitution are /not/ normalized.

There is intentionally no 'Monad' instance; such an instance would have
quadratic complexity.

 -}
data Conditional level variable child =
    Conditional
        { term :: child
        , predicate :: !(Predicate variable)
        , substitution :: !(Substitution variable)
        }
    deriving (Foldable, Functor, Generic, Traversable)

deriving instance
    (Eq child, Eq (variable Object)) =>
    Eq (Conditional level variable child)

deriving instance
    (Ord child, Ord (variable Object)) =>
    Ord (Conditional level variable child)

deriving instance
    (Show child, Show (variable Object)) =>
    Show (Conditional level variable child)

instance
    (Hashable child, Hashable (variable Object)) =>
    Hashable (Conditional level variable child)

instance
    (NFData child, NFData (variable Object)) =>
    NFData (Conditional level variable child)

instance
    ( MetaOrObject level
    , Ord (variable Object)
    , Unparse (variable Object)
    , SortedVariable variable
    , Semigroup term
    ) =>
    Semigroup (Conditional level variable term)
  where
    (<>) predicated1 predicated2 = (<>) <$> predicated1 <*> predicated2
    {-# INLINE (<>) #-}

instance
    ( MetaOrObject level
    , Ord (variable Object)
    , Unparse (variable Object)
    , SortedVariable variable
    , Monoid term
    ) =>
    Monoid (Conditional level variable term)
  where
    mempty =
        Conditional
            { term = mempty
            , predicate = Predicate.makeTruePredicate
            , substitution = mempty
            }
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

instance
    ( MetaOrObject level
    , SortedVariable variable
    , Ord (variable level)
    , Unparse (variable level)
    ) =>
    Applicative (Conditional level variable)
  where
    pure term =
        Conditional
            { term
            , predicate = Predicate.makeTruePredicate
            , substitution = mempty
            }

    (<*>) predicated1 predicated2 =
        Conditional
            { term = f a
            , predicate = Predicate.makeAndPredicate predicate1 predicate2
            , substitution = substitution1 <> substitution2
            }
      where
        Conditional f predicate1 substitution1 = predicated1
        Conditional a predicate2 substitution2 = predicated2

instance TopBottom term
    => TopBottom (Conditional level variable term)
  where
    isTop Conditional {term, predicate, substitution} =
        isTop term && isTop predicate && isTop substitution
    isBottom Conditional {term, predicate, substitution} =
        isBottom term || isBottom predicate || isBottom substitution

instance
    ( MetaOrObject level
    , SortedVariable variable
    , Ord (variable level)
    , Show (variable level)
    , Unparse (variable level)
    , Unparse child
    ) =>
    Unparse (Conditional level variable child)
  where
    unparse Conditional { term, predicate, substitution } =
        unparseAnd
            (below "/* term: */" (unparse term))
            (unparseAnd
                (below
                    "/* predicate: */"
                    (unparse predicate)
                )
                (below
                    "/* substitution: */"
                    (unparse $ Predicate.fromSubstitution substitution)
                )
            )
      where
        unparseAnd first second =
            "\\and" <> parameters' ["_"] <> arguments' [first, second]
        below first second =
            (Pretty.align . Pretty.vsep) [first, second]

{- | Forget the 'term', keeping only the attached conditions.
 -}
withoutTerm :: Conditional Object variable term -> Conditional Object variable ()
withoutTerm predicated = predicated { term = () }

{- | Attach the condition to the given 'term'.
 -}
withCondition
    :: term
    -> Conditional Object variable ()
    -- ^ Condition
    -> Conditional Object variable term
withCondition term predicated = predicated { term }

{- | Combine the conditions of both arguments, taking the 'term' of the first.
 -}
andCondition
    ::  ( Ord (variable Object)
        , Show (variable Object)
        , Unparse (variable Object)
        , SortedVariable variable
        )
    => Conditional Object variable term
    -> Conditional Object variable ()
    -> Conditional Object variable term
andCondition = (<*)

{- | Construct a 'Conditional' holding the given 'Predicate'.

The result has an empty 'Substitution'.

 -}
fromPredicate
    :: Ord (variable Object)
    => Predicate variable
    -> Conditional Object variable ()
fromPredicate predicate =
    Conditional { term = (), predicate, substitution = mempty }

{- | Construct a 'Conditional' holding the given 'Substitution'.

The result has a true 'Predicate'.

 -}
fromSubstitution
    :: Ord (variable Object)
    => Substitution variable
    -> Conditional Object variable ()
fromSubstitution substitution =
    Conditional
        { term = ()
        , predicate = Predicate.makeTruePredicate
        , substitution
        }

{- | Combine the predicate with the conditions of the first argument.
 -}
andPredicate
    ::  ( Ord (variable Object)
        , Show (variable Object)
        , Unparse (variable Object)
        , SortedVariable variable
        )
    => Conditional Object variable term
    -> Predicate variable
    -> Conditional Object variable term
andPredicate config predicate = config `andCondition` fromPredicate predicate

{- | Extract the set of free variables from a 'Conditional' term.

See also: 'Predicate.freeVariables'.
-}
freeVariables
    :: ( MetaOrObject Object
       , Ord (variable Object)
       , Show (variable Object)
       , Unparse (variable Object)
       , SortedVariable variable
       )
    => (term -> Set (variable Object))
    -- ^ Extract the free variables of @term@.
    -> Conditional Object variable term
    -> Set (variable Object)
freeVariables getFreeVariables Conditional { term, predicate, substitution } =
    getFreeVariables term
    <> Predicate.freeVariables predicate
    <> Substitution.freeVariables substitution

{- | Transform a predicate and substitution into a predicate only.

@toPredicate@ is intended for generalizing the 'Predicate' and 'Substitution' of
a 'PredicateSubstition' into only a 'Predicate'; i.e. when @term ~ ()@,

> Conditional Object variable term ~ PredicateSubstitution Object variable

@toPredicate@ is also used to extract the 'Predicate' and 'Substitution' while
discarding the 'term'.

See also: 'substitutionToPredicate'.

-}
toPredicate
    :: ( MetaOrObject Object
       , SortedVariable variable
       , Ord (variable Object)
       , Show (variable Object)
       , Unparse (variable Object)
       )
    => Conditional Object variable term
    -> Predicate variable
toPredicate Conditional { predicate, substitution } =
    Predicate.makeAndPredicate
        predicate
        (Predicate.fromSubstitution substitution)

{- | Transform all variables (free and quantified) in a 'Conditional' term.

-}
mapVariables
    :: Ord (variableTo Object)
    => ((variableFrom Object -> variableTo Object) -> termFrom -> termTo)
    -> (variableFrom Object -> variableTo Object)
    -> Conditional Object variableFrom termFrom
    -> Conditional Object variableTo   termTo
mapVariables
    mapTermVariables
    mapVariable
    Conditional { term, predicate, substitution }
  =
    Conditional
        { term = mapTermVariables mapVariable term
        , predicate = Predicate.mapVariables mapVariable predicate
        , substitution = Substitution.mapVariables mapVariable substitution
        }
