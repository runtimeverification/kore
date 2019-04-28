{-|
Module      : Kore.Step.Simplification.Implies
Description : Tools for Implies pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Implies
    ( simplify
    , simplifyEvaluated
    ) where

import           Kore.AST.Common
                 ( Implies (..) )
import           Kore.AST.Valid
import qualified Kore.Attribute.Symbol as Attribute
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import qualified Kore.Predicate.Predicate as Syntax.Predicate
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.OrPattern
                 ( OrPattern )
import qualified Kore.Step.OrPattern as OrPattern
import           Kore.Step.Pattern as Pattern
import qualified Kore.Step.Representation.MultiOr as MultiOr
import           Kore.Step.Simplification.Data
                 ( PredicateSimplifier, SimplificationProof (..), Simplifier,
                 TermLikeSimplifier )
import qualified Kore.Step.Simplification.Not as Not
                 ( makeEvaluate, simplifyEvaluated )
import           Kore.Unparser
import           Kore.Variables.Fresh
                 ( FreshVariable )

{-|'simplify' simplifies an 'Implies' pattern with 'OrPattern'
children.

Right now this uses the following simplifications:

* a -> (b or c) = (a -> b) or (a -> c)
* bottom -> b = top
* top -> b = b
* a -> top = top
* a -> bottom = not a

and it has a special case for children with top terms.
-}
simplify
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Show (variable Object)
        , Unparse (variable Object)
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier Object
    -> TermLikeSimplifier Object
    -> BuiltinAndAxiomSimplifierMap Object
    -> Implies Object (OrPattern Object variable)
    -> Simplifier
        (OrPattern Object variable , SimplificationProof Object)
simplify
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    Implies
        { impliesFirst = first
        , impliesSecond = second
        }
  =
    simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        first
        second

{-| simplifies an Implies given its two 'OrPattern' children.

See 'simplify' for details.
-}
-- TODO: Maybe transform this to (not a) \/ b
{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Implies Object) (Valid Object) (OrPattern Object variable)

instead of two 'OrPattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Valid' annotation will eventually cache information
besides the pattern sort, which will make it even more useful to carry around.

-}
simplifyEvaluated
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Show (variable Object)
        , Unparse (variable Object)
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier Object
    -> TermLikeSimplifier Object
    -> BuiltinAndAxiomSimplifierMap Object
    -> OrPattern Object variable
    -> OrPattern Object variable
    -> Simplifier
        (OrPattern Object variable, SimplificationProof Object)
simplifyEvaluated
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    first
    second
  | OrPattern.isTrue first =
    return (second, SimplificationProof)
  | OrPattern.isFalse first =
    return (MultiOr.make [Pattern.top], SimplificationProof)
  | OrPattern.isTrue second =
    return (MultiOr.make [Pattern.top], SimplificationProof)
  | OrPattern.isFalse second = do
    result <-
        Not.simplifyEvaluated
            tools
            predicateSimplifier
            termSimplifier
            axiomSimplifiers
            first
    return (result, SimplificationProof)
  | otherwise = do
    results <- traverse (simplifyEvaluateHalfImplies' first) second
    return (MultiOr.flatten results, SimplificationProof)
  where
    simplifyEvaluateHalfImplies' =
        simplifyEvaluateHalfImplies
            tools
            predicateSimplifier
            termSimplifier
            axiomSimplifiers

simplifyEvaluateHalfImplies
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Show (variable Object)
        , Unparse (variable Object)
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier Object
    -> TermLikeSimplifier Object
    -> BuiltinAndAxiomSimplifierMap Object
    -> OrPattern Object variable
    -> Pattern Object variable
    -> Simplifier (OrPattern Object variable)
simplifyEvaluateHalfImplies
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    first
    second
  | OrPattern.isTrue first =
    return (MultiOr.make [second])
  | OrPattern.isFalse first =
    return (MultiOr.make [Pattern.top])
  | Pattern.isTop second =
    return (MultiOr.make [Pattern.top])
  | Pattern.isBottom second =
    Not.simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        first
  | otherwise =
    -- TODO: Also merge predicate-only patterns for 'Or'
    return $ case MultiOr.extractPatterns first of
        [firstP] -> makeEvaluateImplies firstP second
        _ -> makeEvaluateImplies (OrPattern.toExpandedPattern first) second

makeEvaluateImplies
    ::  ( SortedVariable variable
        , Ord (variable Object)
        , Show (variable Object)
        , Unparse (variable Object)
        )
    => Pattern Object variable
    -> Pattern Object variable
    -> OrPattern Object variable
makeEvaluateImplies
    first second
  | Pattern.isTop first =
    MultiOr.make [second]
  | Pattern.isBottom first =
    MultiOr.make [Pattern.top]
  | Pattern.isTop second =
    MultiOr.make [Pattern.top]
  | Pattern.isBottom second =
    Not.makeEvaluate first
  | otherwise =
    makeEvaluateImpliesNonBool first second

makeEvaluateImpliesNonBool
    ::  ( SortedVariable variable
        , Ord (variable Object)
        , Show (variable Object)
        , Unparse (variable Object)
        )
    => Pattern Object variable
    -> Pattern Object variable
    -> OrPattern Object variable
makeEvaluateImpliesNonBool
    pattern1@Conditional
        { term = firstTerm
        , predicate = firstPredicate
        , substitution = firstSubstitution
        }
    pattern2@Conditional
        { term = secondTerm
        , predicate = secondPredicate
        , substitution = secondSubstitution
        }
  | isTop firstTerm, isTop secondTerm =
    MultiOr.make
        [ Conditional
            { term = firstTerm
            , predicate =
                Syntax.Predicate.makeImpliesPredicate
                    (Syntax.Predicate.makeAndPredicate
                        firstPredicate
                        (Syntax.Predicate.fromSubstitution firstSubstitution)
                    )
                    (Syntax.Predicate.makeAndPredicate
                        secondPredicate
                        (Syntax.Predicate.fromSubstitution secondSubstitution)
                    )
            , substitution = mempty
            }
        ]
  | otherwise =
    MultiOr.make
        [ Conditional
            { term =
                mkImplies
                    (Pattern.toMLPattern pattern1)
                    (Pattern.toMLPattern pattern2)
            , predicate = Syntax.Predicate.makeTruePredicate
            , substitution = mempty
            }
        ]
