{-|
Module      : Kore.Step.Simplification.And
Description : Tools for And pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.And
    ( makeEvaluate
    , simplify
    , simplifyEvaluated
    , And (..)
    ) where

import           Control.Applicative
                 ( Alternative (empty) )
import qualified Control.Monad.Trans as Monad.Trans
import           Data.List
                 ( foldl1', nub )
import           GHC.Stack
                 ( HasCallStack )

import           Kore.AST.Common
                 ( And (..) )
import           Kore.AST.Valid
import           Kore.Attribute.Symbol
                 ( StepperAttributes )
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Conditional
                 ( Conditional (..) )
import qualified Kore.Step.Conditional as Conditional
import qualified Kore.Step.Or as Or
                 ( OrOfExpandedPattern )
import           Kore.Step.Pattern as Pattern
import qualified Kore.Step.Representation.MultiOr as MultiOr
                 ( make )
import qualified Kore.Step.Representation.OrOfExpandedPattern as OrOfExpandedPattern
                 ( isFalse, isTrue )
import qualified Kore.Step.Simplification.AndTerms as AndTerms
                 ( termAnd )
import           Kore.Step.Simplification.Data
                 ( BranchT, PredicateSubstitutionSimplifier,
                 SimplificationProof (..), Simplifier, StepPatternSimplifier,
                 gather, scatter )
import qualified Kore.Step.Substitution as Substitution
import           Kore.Step.TermLike
import           Kore.Unparser
import           Kore.Variables.Fresh

{-|'simplify' simplifies an 'And' of 'Or.Pattern'.

To do that, it first distributes the terms, making it an Or of And patterns,
each And having 'Pattern's as children, then it simplifies each of
those.

Since an Pattern is of the form term /\ predicate /\ substitution,
making an and between two Patterns roughly means and-ing each of their
components separately.

This means that a bottom component anywhere makes the result bottom, while
top can always be ignored.

When we 'and' two terms:
by Proposition 5.24 from (1),
    x and functional-pattern = functional-pattern and [x=phi]
We can generalize that to:
    x and function-pattern
        = function-pattern and ceil(function-pattern) and [x=phi]
        but note that ceil(function-pattern) is not actually needed.
We can still generalize that to:
    function-like-pattern1 and function-like-pattern2
        = function-pattern1 and function-pattern1 == function-pattern2
Also, we have
    constructor1(s1, ..., sk) and constructor2(t1, ..., tk):
        if constructor1 != constructor2 then this is bottom
        else it is
            constructor1(s1 and t1, ..., sk and tk)
    * constructor - 'inj' (sort injection) pairs become bottom
    * injection-injection pairs with the same injection work the same as
      identical constructors
    domain-value1 and domain-value1, where both are string-based:
        domain-value1 if they are equal
        bottom otherwise
    the same for two string literals and two chars
-}
simplify
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from axiom IDs to axiom evaluators
    -> And level (Or.Pattern level variable)
    -> Simplifier
        ( Or.Pattern level variable
        , SimplificationProof level
        )
simplify
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    And
        { andFirst = first
        , andSecond = second
        }
  =
    simplifyEvaluated
        tools
        substitutionSimplifier
        simplifier
        axiomIdToSimplifier
        first
        second

{-| simplifies an And given its two 'Or.Pattern' children.

See 'simplify' for details.
-}
{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (And level) (Valid level) (Or.Pattern level variable)

instead of two 'Or.Pattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Valid' annotation will eventually cache information
besides the pattern sort, which will make it even more useful to carry around.

-}
simplifyEvaluated
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from axiom IDs to axiom evaluators
    -> Or.Pattern level variable
    -> Or.Pattern level variable
    -> Simplifier
        (Or.Pattern level variable, SimplificationProof level)
simplifyEvaluated
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    first
    second
  | OrOfExpandedPattern.isFalse first =
    return (MultiOr.make [], SimplificationProof)
  | OrOfExpandedPattern.isFalse second =
    return (MultiOr.make [], SimplificationProof)

  | OrOfExpandedPattern.isTrue first =
    return (second, SimplificationProof)
  | OrOfExpandedPattern.isTrue second =
    return (first, SimplificationProof)

  | otherwise = do
    result <-
        gather $ do
            first1 <- scatter first
            second1 <- scatter second
            makeEvaluate
                tools
                substitutionSimplifier
                simplifier
                axiomIdToSimplifier
                first1
                second1
    return (MultiOr.make result, SimplificationProof)

{-|'makeEvaluate' simplifies an 'And' of 'Pattern's.

See the comment for 'simplify' to find more details.
-}
makeEvaluate
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , HasCallStack
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from axiom IDs to axiom evaluators
    -> Pattern level variable
    -> Pattern level variable
    -> BranchT Simplifier (Pattern level variable)
makeEvaluate
    tools substitutionSimplifier simplifier axiomIdToSimplifier first second
  | Pattern.isBottom first || Pattern.isBottom second = empty
  | Pattern.isTop first = return second
  | Pattern.isTop second = return first
  | otherwise =
    makeEvaluateNonBool
        tools
        substitutionSimplifier
        simplifier
        axiomIdToSimplifier
        first
        second

makeEvaluateNonBool
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , HasCallStack
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from axiom IDs to axiom evaluators
    -> Pattern level variable
    -> Pattern level variable
    -> BranchT Simplifier (Pattern level variable)
makeEvaluateNonBool
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    first@Conditional { term = firstTerm }
    second@Conditional { term = secondTerm }
  = do
    (terms, _proof) <-
        Monad.Trans.lift $ makeTermAnd
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            firstTerm
            secondTerm
    let firstCondition = Conditional.withoutTerm first
        secondCondition = Conditional.withoutTerm second
        initialConditions = firstCondition <> secondCondition
        merged = Conditional.andCondition terms initialConditions
    normalized <-
        Substitution.normalize
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            merged
    return
        (applyAndIdempotence <$> normalized)
            { predicate =
                applyAndIdempotence <$> Conditional.predicate normalized
            }

applyAndIdempotence
    ::  ( Ord (variable level)
        , MetaOrObject level
        , Show (variable level)
        , Unparse (variable level)
        , SortedVariable variable
        )
    => TermLike variable
    -> TermLike variable
applyAndIdempotence patt =
    foldl1' mkAnd (nub (children patt))
  where
    children (And_ _ p1 p2) = children p1 ++ children p2
    children p = [p]

makeTermAnd
    ::  ( MetaOrObject level
        , FreshVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , SortedVariable variable
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from axiom IDs to axiom evaluators
    -> TermLike variable
    -> TermLike variable
    -> Simplifier (Pattern level variable, SimplificationProof level)
makeTermAnd = AndTerms.termAnd
