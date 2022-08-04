{- |
Module      : Kore.Simplify.Not
Description : Tools for Not pattern simplification.
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Simplify.Not (
    makeEvaluate,
    makeEvaluatePredicate,
    simplify,
    simplifyEvaluatedPredicate,
    notSimplifier,
) where

import Kore.Internal.Condition (
    Condition,
 )
import Kore.Internal.Conditional (
    Conditional (Conditional),
 )
import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import Kore.Internal.MultiAnd qualified as MultiAnd
import Kore.Internal.MultiOr (
    MultiOr,
 )
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.OrCondition (
    OrCondition,
 )
import Kore.Internal.OrCondition qualified as OrCondition
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    makeAndPredicate,
    makeNotPredicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike
import Kore.Internal.TermLike qualified as TermLike (
    markSimplified,
    termLikeSort,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.And qualified as And
import Kore.Simplify.NotSimplifier
import Kore.Simplify.Simplify
import Kore.TopBottom (
    TopBottom (..),
 )
import Logic
import Prelude.Kore

{- |'simplify' simplifies a 'Not' pattern with an 'OrPattern'
child.

Right now this uses the following:

* not top = bottom
* not bottom = top
-}
simplify ::
    MonadSimplify simplifier =>
    SideCondition RewritingVariableName ->
    Not Sort (OrPattern RewritingVariableName) ->
    simplifier (OrPattern RewritingVariableName)
simplify sideCondition not'@Not{notSort} =
    OrPattern.observeAllT $ do
        let evaluated = MultiAnd.map makeEvaluateNot (distributeNot not')
        andPattern <- scatterAnd evaluated
        mkMultiAndPattern notSort sideCondition andPattern

simplifyEvaluatedPredicate ::
    MonadSimplify simplifier =>
    OrCondition RewritingVariableName ->
    simplifier (OrCondition RewritingVariableName)
simplifyEvaluatedPredicate notChild =
    OrCondition.observeAllT $ do
        let not' = Not{notChild = notChild, notSort = ()}
        andPredicate <-
            scatterAnd
                ( MultiAnd.map
                    makeEvaluateNotPredicate
                    (distributeNot not')
                )
        mkMultiAndPredicate andPredicate

{- |'makeEvaluate' simplifies a 'Not' pattern given its 'Pattern'
child.

See 'simplify' for details.
-}
makeEvaluate ::
    Pattern RewritingVariableName ->
    OrPattern RewritingVariableName
makeEvaluate = makeEvaluateNot . Not ()

makeEvaluateNot ::
    Not sort (Pattern RewritingVariableName) ->
    OrPattern RewritingVariableName
makeEvaluateNot Not{notChild} =
    MultiOr.merge
        (MultiOr.map Pattern.fromTermLike $ makeTermNot term)
        ( makeEvaluatePredicate condition
            & Pattern.fromCondition (termLikeSort term)
            & MultiOr.singleton
        )
  where
    (term, condition) = Conditional.splitTerm notChild

{- | Given a not's @Internal.Condition@ argument, simplifies the @not@.

Right now there is no actual simplification, this function just creates
a negated @Internal.Condition@.

I.e. if we want to simplify @not (predicate and substitution)@, we may pass
@predicate and substitution@ to this function, which will convert
@predicate and substitution@ into a @Kore.Internal.Predicate@ and will apply
a @not@ on top of that.
-}
makeEvaluatePredicate ::
    Condition RewritingVariableName ->
    Condition RewritingVariableName
makeEvaluatePredicate
    Conditional
        { term = ()
        , predicate
        , substitution
        } =
        Conditional
            { term = ()
            , predicate =
                Predicate.markSimplified $
                    makeNotPredicate $
                        makeAndPredicate predicate $
                            Substitution.toPredicate substitution
            , substitution = mempty
            }

makeEvaluateNotPredicate ::
    Not sort (Condition RewritingVariableName) ->
    OrCondition RewritingVariableName
makeEvaluateNotPredicate Not{notChild = predicate} =
    OrCondition.fromConditions [makeEvaluatePredicate predicate]

makeTermNot ::
    TermLike RewritingVariableName ->
    MultiOr (TermLike RewritingVariableName)
-- TODO: maybe other simplifications like
-- not ceil = floor not
-- not forall = exists not
makeTermNot (Not_ _ term) = MultiOr.singleton term
makeTermNot (And_ _ term1 term2) =
    MultiOr.merge (makeTermNot term1) (makeTermNot term2)
makeTermNot term
    | isBottom term = MultiOr.singleton (mkTop sort)
    | isTop term = MultiOr.singleton (mkBottom sort)
    | otherwise = MultiOr.singleton $ TermLike.markSimplified $ mkNot term
  where
    sort = TermLike.termLikeSort term

-- | Distribute 'Not' over 'MultiOr' using de Morgan's identity.
distributeNot ::
    (Hashable sort, Eq sort, Hashable child, Eq child, TopBottom child) =>
    Not sort (MultiOr child) ->
    MultiAnd (Not sort child)
distributeNot notOr@Not{notChild} =
    MultiAnd.make $ worker <$> toList notChild
  where
    worker child = notOr{notChild = child}

-- | Distribute 'MultiAnd' over 'MultiOr' and 'scatter' into 'LogicT'.
scatterAnd ::
    Hashable child =>
    Ord child =>
    TopBottom child =>
    MultiAnd (MultiOr child) ->
    LogicT m (MultiAnd child)
scatterAnd = scatter . MultiAnd.distributeAnd

-- | Conjoin and simplify a 'MultiAnd' of 'Pattern'.
mkMultiAndPattern ::
    MonadSimplify simplifier =>
    Sort ->
    SideCondition RewritingVariableName ->
    MultiAnd (Pattern RewritingVariableName) ->
    LogicT simplifier (Pattern RewritingVariableName)
mkMultiAndPattern resultSort = And.makeEvaluate resultSort notSimplifier

-- | Conjoin and simplify a 'MultiAnd' of 'Condition'.
mkMultiAndPredicate ::
    MultiAnd (Condition RewritingVariableName) ->
    LogicT simplifier (Condition RewritingVariableName)
mkMultiAndPredicate predicates =
    -- Using fold because the Monoid instance of Condition
    -- implements And semantics.
    return $ fold predicates

notSimplifier :: MonadSimplify simplifier => NotSimplifier simplifier
notSimplifier = NotSimplifier simplify
