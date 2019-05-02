{-|
Module      : Kore.Step.Simplification.Floor
Description : Tools for Floor pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Floor
    ( simplify
    , makeEvaluateFloor
    ) where

import           Kore.AST.Valid
import           Kore.Predicate.Predicate
                 ( makeAndPredicate, makeFloorPredicate )
import           Kore.Step.OrPattern
                 ( OrPattern )
import qualified Kore.Step.OrPattern as OrPattern
import           Kore.Step.Pattern as Pattern
import qualified Kore.Step.Representation.MultiOr as MultiOr
                 ( extractPatterns )
import           Kore.Syntax.Floor
import           Kore.Unparser

{-| 'simplify' simplifies a 'Floor' of 'OrPattern'.

We also take into account that
* floor(top) = top
* floor(bottom) = bottom
* floor leaves predicates and substitutions unchanged
* floor transforms terms into predicates

However, we don't take into account things like
floor(a and b) = floor(a) and floor(b).
-}
simplify
    ::  ( SortedVariable variable
        , Unparse variable
        , Show variable
        , Ord variable
        )
    => Floor Sort (OrPattern variable)
    -> OrPattern variable
simplify Floor { floorChild = child } =
    simplifyEvaluatedFloor child

{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Floor Object) (Valid Object) (OrPattern variable)

instead of an 'OrPattern' argument. The type of 'makeEvaluateFloor'
may be changed analogously. The 'Valid' annotation will eventually cache
information besides the pattern sort, which will make it even more useful to
carry around.

-}
simplifyEvaluatedFloor
    ::  ( SortedVariable variable
        , Show variable
        , Ord variable
        , Unparse variable
        )
    => OrPattern variable
    -> OrPattern variable
simplifyEvaluatedFloor child =
    case MultiOr.extractPatterns child of
        [childP] -> makeEvaluateFloor childP
        _ ->
            makeEvaluateFloor
                (OrPattern.toExpandedPattern child)

{-| 'makeEvaluateFloor' simplifies a 'Floor' of 'Pattern'.

See 'simplify' for details.
-}
makeEvaluateFloor
    ::  ( SortedVariable variable
        , Show variable
        , Ord variable
        , Unparse variable
        )
    => Pattern variable
    -> OrPattern variable
makeEvaluateFloor child
  | Pattern.isTop child    = OrPattern.top
  | Pattern.isBottom child = OrPattern.bottom
  | otherwise              = makeEvaluateNonBoolFloor child

makeEvaluateNonBoolFloor
    ::  ( SortedVariable variable
        , Show variable
        , Ord variable
        , Unparse variable
        )
    => Pattern variable
    -> OrPattern variable
makeEvaluateNonBoolFloor patt@Conditional { term = Top_ _ } =
    OrPattern.fromPattern patt
-- TODO(virgil): Also evaluate functional patterns to bottom for non-singleton
-- sorts, and maybe other cases also
makeEvaluateNonBoolFloor
    Conditional {term, predicate, substitution}
  =
    OrPattern.fromPattern Conditional
        { term = mkTop_
        , predicate = makeAndPredicate (makeFloorPredicate term) predicate
        , substitution = substitution
        }
