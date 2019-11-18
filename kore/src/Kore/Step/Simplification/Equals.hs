{-|
Module      : Kore.Step.Simplification.Equals
Description : Tools for Equals pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Equals
    ( makeEvaluate
    , makeEvaluateTermsToPredicate
    , simplify
    , termEquals
    ) where

import Control.Error
    ( MaybeT (..)
    )
import qualified Control.Monad as Monad
    ( when
    )
import qualified Control.Monad.Trans as Monad.Trans
import Data.List
    ( foldl'
    )
import Data.Maybe
    ( fromMaybe
    )
import qualified GHC.Stack as GHC

import Branch
    ( BranchT
    )
import qualified Branch as BranchT
import qualified Kore.Internal.Condition as Condition
import qualified Kore.Internal.Conditional as Conditional
    ( Conditional (..)
    )
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Internal.OrCondition
    ( OrCondition
    )
import qualified Kore.Internal.OrCondition as OrCondition
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( pattern PredicateTrue
    , makeAndPredicate
    , makeCeilPredicate
    , makeEqualsPredicate
    , makeNotPredicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.TermLike
    ( pattern Equals_
    , TermLike
    , pattern Top_
    , isFunctionPattern
    , mkAnd
    , mkBottom_
    , mkCeil_
    , mkIff
    , mkImplies
    , mkNot
    , mkOr
    , mkTop_
    )
import Kore.Internal.Variable
    ( InternalVariable
    )
import Kore.Sort
    ( Sort
    )
import Kore.Step.Simplification.AndTerms
    ( maybeTermEquals
    )
import Kore.Step.Simplification.Simplify
import Kore.Step.Simplification.SubstitutionSimplifier
    ( SubstitutionSimplifier (SubstitutionSimplifier)
    )
import qualified Kore.Step.Simplification.SubstitutionSimplifier as SubstitutionSimplifier
    ( SubstitutionSimplifier (..)
    , substitutionSimplifier
    )
import Kore.Syntax.Equals
    ( Equals (Equals)
    )
import qualified Kore.Syntax.Equals as Equals.DoNotUse
import qualified Kore.Unification.Substitution as Substitution
import Kore.Unification.UnifierT as UnifierT
    ( scatter
    )
import Kore.Unification.UnifierT
    ( UnifierT
    , runUnifierT
    )
import Kore.Unification.Unify
    ( MonadUnify
    )

{-|'simplify' simplifies an 'Equals' pattern made of 'OrPattern's.

This uses the following simplifications
(t = term, s = substitution, p = predicate):

* Equals(a, a) = true
* Equals(phi, psi1 or psi2 or ... or psin), when phi is functional
    = or
        ( not ceil (phi) and not ceil(psi1) and ... and not ceil (psin)
        , and
            ( ceil(phi)
            , ceil(psi1) or ceil(psi2) or  ... or ceil(psin)
            , ceil(psi1) implies phi == psi1)
            , ceil(psi2) implies phi == psi2)
            ...
            , ceil(psin) implies phi == psin)
            )
        )
* Equals(t1 and t2) = ceil(t1 and t2) or (not ceil(t1) and not ceil(t2))
    if t1 and t2 are functions.
* Equals(t1 and p1 and s1, t2 and p2 and s2) =
    Or(
        And(
            Equals(t1, t2)
            And(ceil(t1) and p1 and s1, ceil(t2) and p2 and s2))
        And(not(ceil(t1) and p1 and s1), not(ceil(t2) and p2 and s2))
    )
    + If t1 and t2 can't be bottom, then this becomes
      Equals(t1 and p1 and s1, t2 and p2 and s2) =
        Or(
            And(
                Equals(t1, t2)
                And(p1 and s1, p2 and s2))
            And(not(p1 and s1), not(p2 and s2))
        )
    + If the two terms are constructors, then this becomes
      Equals(
        constr1(t1, t2, ...) and p1 and s1,
        constr2(t1', t2', ...) and p2 and s2)
        = Or(
            and(
                (p1 and s2) iff (p2 and s2),
                constr1 == constr2,
                ceil(constr1(t1, t2, ...), constr2(t1', t2', ...))
                Equals(t1, t1'), Equals(t2, t2'), ...
                )
            and(
                not(ceil(constr1(t1, t2, ...)) and p1 and s1),
                not(ceil(constr2(t1', t2', ...)), p2 and s2)
                )
        )
      Note that when expanding Equals(t1, t1') recursively we don't need to
      put the ceil conditions again, since we already asserted that.
      Also note that ceil(constr(...)) is simplifiable.
    + If the first term is a variable and the second is functional,
      then we get a substitution:
        Or(
            And(
                [t1 = t2]
                And(p1 and s1, p2 and s2))
            And(not(p1 and s1), not(p2 and s2))
        )
    + If the terms are Top, this becomes
      Equals(p1 and s1, p2 and s2) = Iff(p1 and s1, p2 and s2)
    + If the predicate and substitution are Top, then the result is any of
      Equals(t1, t2)
      Or(
          Equals(t1, t2)
          And(not(ceil(t1) and p1 and s1), not(ceil(t2) and p2 and s2))
      )


Normalization of the compared terms is not implemented yet, so
Equals(a and b, b and a) will not be evaluated to Top.
-}
simplify
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => Equals Sort (OrPattern variable)
    -> simplifier (OrPattern variable)
simplify
    Equals { equalsFirst = first, equalsSecond = second }
  =
    simplifyEvaluated first second

{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Equals Sort) (Attribute.Pattern variable) (OrPattern variable)

instead of two 'OrPattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually cache
information besides the pattern sort, which will make it even more useful to
carry around.

-}
simplifyEvaluated
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => OrPattern variable
    -> OrPattern variable
    -> simplifier (OrPattern variable)
simplifyEvaluated first second
  | first == second = return OrPattern.top
  -- TODO: Maybe simplify equalities with top and bottom to ceil and floor
  | otherwise = do
    let isFunctionConditional Conditional {term} = isFunctionPattern term
    case (firstPatterns, secondPatterns) of
        ([firstP], [secondP]) ->
            makeEvaluate firstP secondP
        ([firstP], _)
            | isFunctionConditional firstP ->
                OrPattern.fromPattern
                    <$> makeEvaluateFunctionalOr firstP secondPatterns
        (_, [secondP])
            | isFunctionConditional secondP ->
                OrPattern.fromPattern
                    <$> makeEvaluateFunctionalOr secondP firstPatterns
        _
            | OrPattern.isPredicate first && OrPattern.isPredicate second ->
                return $ OrPattern.fromPattern $ Pattern.fromTermLike
                    (mkIff
                        (OrPattern.toTermLike first)
                        (OrPattern.toTermLike second)
                    )
            | otherwise ->
                makeEvaluate
                    (OrPattern.toPattern first)
                    (OrPattern.toPattern second)
  where
    firstPatterns = MultiOr.extractPatterns first
    secondPatterns = MultiOr.extractPatterns second

makeEvaluateFunctionalOr
    :: forall variable simplifier
    .  (SimplifierVariable variable, MonadSimplify simplifier)
    => Pattern variable
    -> [Pattern variable]
    -> simplifier (Pattern variable)
makeEvaluateFunctionalOr first seconds = do
    Monad.when (length seconds == 1)
        (error . unlines $
            [ "Unexpected 'seconds' singleton."
            , "Please call makeEvaluate or switch the argument order."
            ]
        )
    let secondCeils = map (mkCeil_ . Pattern.toTermLike) seconds
    firstEqualsSeconds <-
        mapM
            (makeEvaluateEqualsIfSecondNotBottom first)
            (zip seconds secondCeils)

    let firstCeil = mkCeil_ (Pattern.toTermLike first)
        secondNotCeils = map mkNot secondCeils
        oneNotBottom = foldl' mkOr mkBottom_ secondCeils
        allAreBottom =
            foldr
                mkAnd
                mkTop_
                (mkNot firstCeil : secondNotCeils)
        oneIsNotBottomEquals =
            foldr
                mkAnd
                firstCeil
                (oneNotBottom : firstEqualsSeconds)
    return
        (Pattern.fromTermLike
            (mkOr
                allAreBottom
                oneIsNotBottomEquals
            )
        )
  where
    makeEvaluateEqualsIfSecondNotBottom
        Conditional {term = firstTerm}
        (Conditional {term = secondTerm}, secondCeil)
      = do
        equality <- makeEvaluateTermsAssumesNoBottom firstTerm secondTerm
        return
            (mkImplies
                secondCeil
                (OrPattern.toTermLike equality)
            )

{-| evaluates an 'Equals' given its two 'Pattern' children.

See 'simplify' for detailed documentation.
-}
makeEvaluate
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => Pattern variable
    -> Pattern variable
    -> simplifier (OrPattern variable)
makeEvaluate first second
  | first == second
  = return OrPattern.top
makeEvaluate
    first@Conditional { term = Top_ _ }
    second@Conditional { term = Top_ _ }
  =
    return $ OrPattern.fromPattern $ Pattern.fromTermLike
        (mkIff
            (Pattern.toTermLike first {term = mkTop_})  -- remove the sort.
            (Pattern.toTermLike second {term = mkTop_})  -- remove the sort.
        )
makeEvaluate
    Conditional
        { term = firstTerm
        , predicate = PredicateTrue
        , substitution = (Substitution.unwrap -> [])
        }
    Conditional
        { term = secondTerm
        , predicate = PredicateTrue
        , substitution = (Substitution.unwrap -> [])
        }
  = do
    result <- makeEvaluateTermsToPredicate firstTerm secondTerm
    return (Pattern.fromCondition <$> result)
makeEvaluate
    first@Conditional { term = firstTerm }
    second@Conditional { term = secondTerm }
  = do
    termEquality <- makeEvaluateTermsAssumesNoBottom firstTerm secondTerm
    let firstCeil =
            (mkCeil_ . Pattern.toTermLike)
                first { term = if termsAreEqual then mkTop_ else firstTerm }
        secondCeil =
            (mkCeil_ . Pattern.toTermLike)
                second { term = if termsAreEqual then mkTop_ else secondTerm }
        termEqualityPattern =
            checkOrChange
                firstTerm
                secondTerm
                termEquality
                (WhenUnchanged
                    (noSimplificationPossiblePattern firstTerm secondTerm)
                )
                (WhenChanged (OrPattern.toPattern termEquality))
    return
        (OrPattern.fromPattern $ Pattern.fromTermLike
            (mkOr
                (mkAnd
                    (Pattern.toTermLike termEqualityPattern)
                    (mkAnd firstCeil secondCeil)
                )
                (mkAnd (mkNot firstCeil) (mkNot secondCeil))
            )
        )
  where
    termsAreEqual = firstTerm == secondTerm

-- Do not export this. This not valid as a standalone function, it
-- assumes that some extra conditions will be added on the outside
makeEvaluateTermsAssumesNoBottom
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => TermLike variable
    -> TermLike variable
    -> simplifier (OrPattern variable)
makeEvaluateTermsAssumesNoBottom firstTerm secondTerm = do
    result <-
        runMaybeT
        $ makeEvaluateTermsAssumesNoBottomMaybe firstTerm secondTerm
    (return . fromMaybe def) result
  where
    def = OrPattern.fromPattern
        (noSimplificationPossiblePattern firstTerm secondTerm)

-- Do not export this. This not valid as a standalone function, it
-- assumes that some extra conditions will be added on the outside
makeEvaluateTermsAssumesNoBottomMaybe
    :: forall variable simplifier
    .  (SimplifierVariable variable, MonadSimplify simplifier)
    => TermLike variable
    -> TermLike variable
    -> MaybeT simplifier (OrPattern variable)
makeEvaluateTermsAssumesNoBottomMaybe first second = do
    result <- termEquals first second
    return (Pattern.fromCondition <$> result)

{-| Combines two terms with 'Equals' into a predicate-substitution.

It does not attempt to fully simplify the terms (the not-ceil parts used to
catch the bottom=bottom case and everything above it), but, if the patterns are
total, this should not be needed anyway.
TODO(virgil): Fully simplify the terms (right now we're not simplifying not
because it returns an 'or').

See 'simplify' for detailed documentation.
-}
makeEvaluateTermsToPredicate
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => TermLike variable
    -> TermLike variable
    -> simplifier (OrCondition variable)
makeEvaluateTermsToPredicate first second
  | first == second = return OrCondition.top
  | otherwise = do
    result <- runMaybeT $ termEquals first second
    case result of
        Nothing -> return
            (OrCondition.fromCondition
                (noSimplificationPossibleCondition first second)
            )
        Just predicatedOr -> do
            let firstCeil = makeCeilPredicate first
                secondCeil = makeCeilPredicate second
                firstCeilNegation = makeNotPredicate firstCeil
                secondCeilNegation = makeNotPredicate secondCeil
                ceilNegationAnd =
                    makeAndPredicate firstCeilNegation secondCeilNegation

            return $ checkOrConditionChange
                first
                second
                predicatedOr
                (WhenUnchanged
                    (OrCondition.fromCondition
                        (noSimplificationPossibleCondition first second)
                    )
                )
                (WhenChanged
                    (MultiOr.merge predicatedOr
                        (OrCondition.fromCondition
                            (Condition.fromPredicate ceilNegationAnd)
                        )
                    )
                )

{- | Simplify an equality relation of two patterns.

@termEquals@ assumes the result will be part of a predicate with a special
condition for testing @⊥ = ⊥@ equality.

The comment for 'Kore.Step.Simplification.And.simplify' describes all
the special cases handled by this.

 -}
termEquals
    :: (SimplifierVariable variable, MonadSimplify simplifier)
    => GHC.HasCallStack
    => TermLike variable
    -> TermLike variable
    -> MaybeT simplifier (OrCondition variable)
termEquals first second = MaybeT $ do
    maybeResults <- BranchT.gather $ runMaybeT $ termEqualsAnd first second
    case sequence maybeResults of
        Nothing -> return Nothing
        Just results -> return $ Just $
            MultiOr.make (map Condition.eraseConditionalTerm results)

termEqualsAnd
    :: forall variable simplifier
    .  (SimplifierVariable variable, MonadSimplify simplifier)
    => GHC.HasCallStack
    => TermLike variable
    -> TermLike variable
    -> MaybeT (BranchT simplifier) (Pattern variable)
termEqualsAnd p1 p2 =
    MaybeT $ run $ do
        notNormalized@Conditional {substitution} <- maybeTermEqualsWorker p1 p2
        let SubstitutionSimplifier { simplifySubstitution } =
                SubstitutionSimplifier.substitutionSimplifier
            replaceSubstitution
                :: Pattern variable -> Condition variable -> Pattern variable
            replaceSubstitution patt condition =
                patt {Conditional.substitution = mempty}
                `Condition.andCondition` condition
        orSubstitution <- simplifySubstitution substitution
        (Monad.Trans.lift . UnifierT.scatter)
            (replaceSubstitution notNormalized <$> orSubstitution)
  where
    run :: MaybeT (UnifierT (BranchT simplifier)) (Pattern variable)
        -> (BranchT simplifier) (Maybe (Pattern variable))
    run it = (runUnifierT . runMaybeT) it >>= either missingCase BranchT.scatter
    missingCase = const (return Nothing)

    maybeTermEqualsWorker
        :: forall unifier
        .  MonadUnify unifier
        => TermLike variable
        -> TermLike variable
        -> MaybeT unifier (Pattern variable)
    maybeTermEqualsWorker = maybeTermEquals termEqualsAndWorker

    termEqualsAndWorker
        :: forall unifier
        .  MonadUnify unifier
        => TermLike variable
        -> TermLike variable
        -> unifier (Pattern variable)
    termEqualsAndWorker first second =
        either ignoreErrors scatterResults
        =<< (runUnifierT . runMaybeT) (maybeTermEqualsWorker first second)
      where
        ignoreErrors _ = return equalsPredicate
        scatterResults =
            maybe
                (return equalsPredicate) -- default if no results
                (BranchT.alternate . BranchT.scatter)
            . sequence
        equalsPredicate = noSimplificationPossiblePattern first second

newtype WhenChanged a = WhenChanged a
newtype WhenUnchanged a = WhenUnchanged a

checkOrChange
    :: InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> OrPattern variable
    -> WhenUnchanged a
    -> WhenChanged a
    -> a
checkOrChange
    first
    second
    maybeChanged
    whenUnchanged
    whenChanged@(WhenChanged defaultValue)
  =
    case OrPattern.toPatterns maybeChanged of
        [patt] ->
            checkPatternChange first second patt whenUnchanged whenChanged
        _ -> defaultValue

checkOrConditionChange
    :: InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> OrCondition variable
    -> WhenUnchanged a
    -> WhenChanged a
    -> a
checkOrConditionChange
    first
    second
    maybeChanged
    whenUnchanged
    whenChanged@(WhenChanged defaultValue)
  =
    case OrCondition.toConditions maybeChanged of
        [condition] ->
            checkConditionChange
                first second condition whenUnchanged whenChanged
        _ -> defaultValue

checkPatternChange
    :: InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> Pattern variable
    -> WhenUnchanged a
    -> WhenChanged a
    -> a
checkPatternChange
    first
    second
    Conditional {term, predicate, substitution}
    whenUnchanged
    whenChanged@(WhenChanged defaultValue)
  | isTop predicate && Substitution.null substitution
  = termChecker term
  | isTop term && Substitution.null substitution
  = termChecker (Predicate.unwrapPredicate predicate)
  | otherwise = defaultValue
  where
    termChecker maybeTermChanged =
        checkTermChange
            first
            second
            maybeTermChanged
            whenUnchanged
            whenChanged

checkConditionChange
    :: InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> Condition variable
    -> WhenUnchanged a
    -> WhenChanged a
    -> a
checkConditionChange
    first
    second
    Conditional {term = (), predicate, substitution}
    whenUnchanged
    whenChanged@(WhenChanged defaultValue)
  | Substitution.null substitution
  = termChecker (Predicate.unwrapPredicate predicate)
  | otherwise = defaultValue
  where
    termChecker maybeTermChanged =
        checkTermChange
            first
            second
            maybeTermChanged
            whenUnchanged
            whenChanged

checkTermChange
    :: InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> TermLike variable
    -> WhenUnchanged a
    -> WhenChanged a
    -> a
checkTermChange
    first
    second
    maybeChanged
    (WhenUnchanged whenUnchanged)
    (WhenChanged whenChanged)
  =
    case maybeChanged of
        Equals_ _ _ t1 t2
            | (t1 == first && t2 == second)
                || (t2 == first && t1 == second) -> whenUnchanged
        _ -> whenChanged

noSimplificationPossibleCondition
    :: InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> Condition variable
noSimplificationPossibleCondition first second =
    Condition.fromPredicate
    $ Predicate.markSimplified
    $ makeEqualsPredicate first second

noSimplificationPossiblePattern
    :: InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> Pattern variable
noSimplificationPossiblePattern first second =
    Pattern.fromCondition (noSimplificationPossibleCondition first second)
