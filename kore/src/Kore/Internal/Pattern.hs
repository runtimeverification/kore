{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause

Representation of program configurations as conditional patterns.
-}
module Kore.Internal.Pattern (
    Pattern,
    coerceSort,
    syncSort,
    patternSort,
    fromCondition,
    fromTermAndPredicate,
    fromPredicateSorted,
    bottomOf,
    isBottom,
    isTop,
    Kore.Internal.Pattern.mapVariables,
    splitTerm,
    toTermLike,
    topOf,
    fromTermLike,
    Kore.Internal.Pattern.freeElementVariables,
    isSimplified,
    hasSimplifiedChildren,
    hasSimplifiedChildrenIgnoreConditions,
    forgetSimplified,
    markSimplified,
    simplifiedAttribute,
    assign,
    requireDefined,
    fromMultiAnd,

    -- * Re-exports
    Conditional (..),
    Conditional.andCondition,
    Conditional.isPredicate,
    withCondition,
    withConditionUnsorted,
    Conditional.withoutTerm,
    Condition,
) where

import Kore.Attribute.Pattern.FreeVariables (
    freeVariables,
    getFreeElementVariables,
 )
import Kore.Attribute.Pattern.Simplified qualified as Attribute (
    Simplified,
 )
import Kore.Internal.Condition (
    Condition,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.Conditional (
    Conditional (..),
 )
import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition.SideCondition as SideCondition (
    Representation,
 )
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike (
    InternalVariable,
    Sort,
    TermLike,
    mkAnd,
    mkBottom,
    mkTop,
    termLikeSort,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Syntax.Variable
import Kore.TopBottom (
    TopBottom (..),
 )
import Prelude.Kore

{- | The conjunction of a pattern, predicate, and substitution.
The form of @Pattern@ is intended to be a convenient representation of a
program configuration for Kore execution.
-}
type Pattern variable = Conditional variable (TermLike variable)

fromTermAndPredicate ::
    InternalVariable variable =>
    TermLike variable ->
    Predicate variable ->
    Pattern variable
fromTermAndPredicate term predicate =
    Conditional
        { term
        , predicate
        , substitution = mempty
        }
fromCondition ::
    InternalVariable variable =>
    Sort ->
    Condition variable ->
    Pattern variable
fromCondition sort = (<$) (mkTop sort)
fromPredicateSorted ::
    InternalVariable variable =>
    Sort ->
    Predicate variable ->
    Pattern variable
fromPredicateSorted sort = fromCondition sort . Condition.fromPredicate
isSimplified :: SideCondition.Representation -> Pattern variable -> Bool
isSimplified sideCondition (splitTerm -> (t, p)) =
    TermLike.isSimplified sideCondition t
        && Condition.isSimplified sideCondition p

{- | Checks whether the conjunction a 'Pattern' has simplified children.
A 'Pattern' is a conjunction at the top level:
@
\\and{S}('term', \\and{S}('predicate', 'substitution'))
@
where 'predicate' itself is generally a conjunction of many clauses. The
children of the 'Pattern' are considered simplified if the 'term' and
'substitution' are simplified and the individual clauses of the 'predicate' are
simplified.
-}
hasSimplifiedChildren ::
    Hashable variable =>
    Eq variable =>
    SideCondition.Representation ->
    Pattern variable ->
    Bool
hasSimplifiedChildren sideCondition patt =
    TermLike.isSimplified sideCondition term
        && all (Predicate.isSimplified sideCondition) clauses
        && Substitution.isSimplified sideCondition substitution
  where
    Conditional{term, predicate, substitution} = patt
    clauses = Predicate.toMultiAnd predicate

{- | Similar to 'hasSimplifiedChildren', only that it ignores the conditions
used to simplify the children.
-}
hasSimplifiedChildrenIgnoreConditions ::
    Hashable variable =>
    Eq variable =>
    Pattern variable ->
    Bool
hasSimplifiedChildrenIgnoreConditions patt =
    TermLike.isSimplifiedSomeCondition term
        && all Predicate.isSimplifiedSomeCondition clauses
        && Substitution.isSimplifiedSomeCondition substitution
  where
    Conditional{term, predicate, substitution} = patt
    clauses = Predicate.toMultiAnd predicate

forgetSimplified ::
    InternalVariable variable => Pattern variable -> Pattern variable
forgetSimplified patt =
    TermLike.forgetSimplified term
        `withCondition` Condition.forgetSimplified condition
  where
    (term, condition) = Conditional.splitTerm patt
markSimplified ::
    InternalVariable variable => Pattern variable -> Pattern variable
markSimplified patt =
    TermLike.markSimplified term
        `withCondition` Condition.markSimplified condition
  where
    (term, condition) = Conditional.splitTerm patt
simplifiedAttribute :: Pattern variable -> Attribute.Simplified
simplifiedAttribute (splitTerm -> (t, p)) =
    TermLike.simplifiedAttribute t <> Condition.simplifiedAttribute p
freeElementVariables ::
    InternalVariable variable =>
    Pattern variable ->
    [ElementVariable variable]
freeElementVariables =
    getFreeElementVariables . freeVariables

{- |'mapVariables' transforms all variables, including the quantified ones,
in an Pattern.
-}
mapVariables ::
    (InternalVariable variable1, InternalVariable variable2) =>
    AdjSomeVariableName (variable1 -> variable2) ->
    Pattern variable1 ->
    Pattern variable2
mapVariables adj Conditional{term, predicate, substitution} =
    Conditional
        { term = TermLike.mapVariables adj term
        , predicate = Predicate.mapVariables adj predicate
        , substitution = Substitution.mapVariables adj substitution
        }

{- | Convert an 'Pattern' to an ordinary 'TermLike'.
Conversion relies on the interpretation of 'Pattern' as a conjunction of
patterns. Conversion erases the distinction between terms, predicates, and
substitutions; this function should be used with care where that distinction is
important.
-}
toTermLike ::
    forall variable.
    (InternalVariable variable, HasCallStack) =>
    Pattern variable ->
    TermLike variable
toTermLike Conditional{term, predicate, substitution} =
    simpleAnd
        (simpleAnd term predicate)
        (Substitution.toPredicate substitution)
  where
    simpleAnd ::
        TermLike variable ->
        Predicate variable ->
        TermLike variable
    simpleAnd pattern' predicate'
        | isTop predicate' = pattern'
        | isBottom predicate' = mkBottom sort
        | isTop pattern' = predicateTermLike
        | isBottom pattern' = pattern'
        | otherwise = mkAnd pattern' predicateTermLike
      where
        predicateTermLike = Predicate.fromPredicate sort predicate'
        sort = termLikeSort pattern'

{- | An 'Pattern' where the 'term' is 'Bottom' of the given 'Sort'.

The 'predicate' is set to 'makeFalsePredicate'.
-}
bottomOf :: InternalVariable variable => Sort -> Pattern variable
bottomOf resultSort =
    Conditional
        { term = mkBottom resultSort
        , predicate = Predicate.makeFalsePredicate
        , substitution = mempty
        }

-- | An 'Pattern' where the 'term' is 'Top' of the given 'Sort'.
topOf :: InternalVariable variable => Sort -> Pattern variable
topOf resultSort =
    Conditional
        { term = mkTop resultSort
        , predicate = Predicate.makeTruePredicate
        , substitution = mempty
        }

{- | Construct an 'Pattern' from a 'TermLike'.

The resulting @Pattern@ has a true predicate and an empty
substitution, unless it is trivially 'Bottom'.

See also: 'makeTruePredicate', 'pure'
-}
fromTermLike ::
    InternalVariable variable =>
    TermLike variable ->
    Pattern variable
fromTermLike term
    | isBottom term = bottomOf (termLikeSort term)
    | otherwise =
        Conditional
            { term
            , predicate = Predicate.makeTruePredicate
            , substitution = mempty
            }

withCondition ::
    InternalVariable variable =>
    TermLike variable ->
    -- | Condition
    Conditional variable () ->
    Pattern variable
withCondition
    term
    Conditional
        { term = ()
        , predicate
        , substitution
        } =
        syncSort Conditional{term, predicate, substitution}

withConditionUnsorted ::
    TermLike variable ->
    -- | Condition
    Conditional variable () ->
    Pattern variable
withConditionUnsorted
    term
    Conditional{term = (), predicate, substitution} =
        Conditional{term, predicate, substitution}

splitTerm :: Pattern variable -> (TermLike variable, Condition variable)
splitTerm = Conditional.splitTerm

coerceSort ::
    (HasCallStack, InternalVariable variable) =>
    Sort ->
    Pattern variable ->
    Pattern variable
coerceSort
    sort
    Conditional{term, predicate, substitution} =
        Conditional
            { term =
                if isTop term
                    then mkTop sort
                    else TermLike.sameTermLikeSort sort term
            , -- Need to override this since a 'ceil' (say) over a predicate is that
              -- predicate with a different sort.
              predicate = predicate
            , substitution
            }

patternSort :: Pattern variable -> Sort
patternSort Conditional{term} = termLikeSort term

syncSort ::
    (HasCallStack, InternalVariable variable) =>
    Pattern variable ->
    Pattern variable
syncSort patt = coerceSort (patternSort patt) patt

assign ::
    InternalVariable variable =>
    SomeVariable variable ->
    TermLike variable ->
    Pattern variable
assign variable term =
    withCondition assignedTerm $
        Condition.fromSingleSubstitution
            assignment
  where
    assignment = Substitution.assign variable term
    assignedTerm = Substitution.assignedTerm assignment

{- | Add a 'Predicate' requiring that the 'term' of a 'Pattern' is defined.

@requireDefined@ effectively implements:

@
φ = φ ∧ ⌈φ⌉
@
-}
requireDefined ::
    InternalVariable variable =>
    Pattern variable ->
    Pattern variable
requireDefined Conditional{term, predicate, substitution} =
    Conditional
        { term
        , substitution
        , predicate =
            Predicate.makeAndPredicate predicate $
                Predicate.makeCeilPredicate term
        }

fromMultiAnd ::
    InternalVariable variable =>
    Sort ->
    MultiAnd (Pattern variable) ->
    Pattern variable
fromMultiAnd sort patterns =
    foldr
        ( \pattern1 ->
            pure
                . maybe
                    pattern1
                    (\pattern2 -> mkAnd <$> pattern1 <*> pattern2)
        )
        Nothing
        patterns
        & fromMaybe (topOf sort)
