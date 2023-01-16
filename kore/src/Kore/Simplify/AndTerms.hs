{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Simplify.AndTerms (
    termUnification,
    maybeTermAnd,
    maybeTermEquals,
    TermSimplifier,
    TermTransformationOld,
    cannotUnifyDistinctDomainValues,
    functionAnd,
    matchFunctionAnd,
    compareForEquals,
    FunctionAnd (..),
) where

import Control.Error (
    MaybeT (..),
 )
import Control.Error qualified as Error
import Data.String (
    fromString,
 )
import Data.Text (
    Text,
 )
import Kore.Builtin.Bool qualified as Builtin.Bool
import Kore.Builtin.Builtin qualified as Builtin
import Kore.Builtin.Endianness qualified as Builtin.Endianness
import Kore.Builtin.Int qualified as Builtin.Int
import Kore.Builtin.InternalBytes (
    matchBytes,
    unifyBytes,
 )
import Kore.Builtin.KEqual qualified as Builtin.KEqual

-- import Kore.Builtin.List qualified as Builtin.List
-- import Kore.Builtin.Map qualified as Builtin.Map
-- import Kore.Builtin.Set qualified as Builtin.Set
import Kore.Builtin.Signedness qualified as Builtin.Signedness
import Kore.Builtin.String qualified as Builtin.String
import Kore.Internal.Condition as Condition
import Kore.Internal.OrCondition qualified as OrCondition
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    makeEqualsPredicate,
    makeNotPredicate,
    pattern PredicateTrue,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition (
    topTODO,
 )
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.Symbol qualified as Symbol
import Kore.Internal.TermLike
import Kore.Log.DebugUnification (
    debugUnificationSolved,
    debugUnificationUnsolved,
    whileDebugUnification,
 )
import Kore.Log.DebugUnifyBottom (
    debugUnifyBottom,
    debugUnifyBottomAndReturnBottom,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Exists qualified as Exists
import Kore.Simplify.ExpandAlias
import Kore.Simplify.InjSimplifier
import Kore.Simplify.NoConfusion
import Kore.Simplify.NotSimplifier
import Kore.Simplify.Overloading as Overloading
import Kore.Simplify.Simplify as Simplifier
import Kore.Sort (
    sameSort,
 )
import Kore.Unification.Unify as Unify
import Kore.Unparser
import Pair
import Prelude.Kore
import Pretty qualified

{- | Unify two terms without discarding the terms.

We want to keep the terms because substitution relies on the result not being
@\\bottom@.

When a case is not implemented, @termUnification@ will create a @\\ceil@ of
the conjunction of the two terms.

The comment for 'Kore.Simplify.And.simplify' describes all
the special cases handled by this.
-}
termUnification ::
    forall unifier.
    MonadUnify unifier =>
    NotSimplifier Simplifier =>
    HasCallStack =>
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    unifier (Pattern RewritingVariableName)
termUnification = \term1 term2 ->
    whileDebugUnification term1 term2 $ do
        result <- termUnificationWorker term1 term2
        debugUnificationSolved result
        pure result
  where
    termUnificationWorker ::
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        unifier (Pattern RewritingVariableName)
    termUnificationWorker term1 term2 = do
        let maybeTermUnification ::
                MaybeT unifier (Pattern RewritingVariableName)
            maybeTermUnification =
                maybeTermAnd termUnificationWorker term1 term2
        Error.maybeT
            (incompleteUnificationPattern term1 term2)
            pure
            maybeTermUnification

    incompleteUnificationPattern term1 term2 = do
        debugUnificationUnsolved term1 term2
        mkAnd term1 term2
            & Pattern.fromTermLike
            & return

maybeTermEquals ::
    MonadUnify unifier =>
    NotSimplifier Simplifier =>
    HasCallStack =>
    -- | Used to simplify subterm "and".
    TermSimplifier RewritingVariableName unifier ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    MaybeT unifier (Pattern RewritingVariableName)
maybeTermEquals childTransformers first second = do
    injSimplifier <- Simplifier.askInjSimplifier
    overloadSimplifier <- Simplifier.askOverloadSimplifier
    -- tools <- Simplifier.askMetadataTools
    worker injSimplifier overloadSimplifier --tools
  where
    worker injSimplifier overloadSimplifier --tools
        | Just unifyData <- Builtin.Int.matchInt first second =
            lift $ Builtin.Int.unifyInt unifyData
        | Just unifyData <- Builtin.Bool.matchBools first second =
            lift $ Builtin.Bool.unifyBool unifyData
        | Just unifyData <- Builtin.String.matchString first second =
            lift $ Builtin.String.unifyString unifyData
        | Just unifyData <- matchDomainValue first second =
            lift $ unifyDomainValue unifyData
        | Just unifyData <- matchStringLiteral first second =
            lift $ unifyStringLiteral unifyData
        | Just term <- matchEqualsAndEquals first second =
            lift $ equalAndEquals term
        | Just unifyData <- matchBytes first second =
            lift $ unifyBytes unifyData
        | Just unifyData <- matchBottomTermEquals first second =
            lift $
                bottomTermEquals
                    (sameSort (termLikeSort first) (termLikeSort second))
                    SideCondition.topTODO
                    unifyData
        | Just unifyData <- matchVariableFunctionEquals first second =
            lift $ variableFunctionEquals unifyData
        | Just unifyData <- matchEqualInjectiveHeadsAndEquals first second =
            lift $ equalInjectiveHeadsAndEquals childTransformers unifyData
        | Just unifyData <- matchInj injSimplifier first second =
            lift $ unifySortInjection childTransformers unifyData
        | Just unifyData <- matchConstructorSortInjectionAndEquals first second =
            lift $ constructorSortInjectionAndEquals unifyData
        | Just unifyData <- matchDifferentConstructors overloadSimplifier first second =
            lift $ constructorAndEqualsAssumesDifferentHeads unifyData
        | Just unifyData <- unifyOverloading overloadSimplifier (Pair first second) =
            lift $ overloadedConstructorSortInjectionAndEquals childTransformers unifyData
        | Just unifyData <- Builtin.Bool.matchUnifyBoolAnd first second =
            lift $ Builtin.Bool.unifyBoolAnd childTransformers unifyData
        | Just unifyData <- Builtin.Bool.matchUnifyBoolOr first second =
            lift $ Builtin.Bool.unifyBoolOr childTransformers unifyData
        | Just boolNotData <- Builtin.Bool.matchUnifyBoolNot first second =
            lift $ Builtin.Bool.unifyBoolNot childTransformers boolNotData
        | Just unifyData <- Builtin.Int.matchUnifyIntEq first second =
            lift $ Builtin.unifyEq childTransformers unifyData
        | Just unifyData <- Builtin.String.matchUnifyStringEq first second =
            lift $ Builtin.unifyEq childTransformers unifyData
        | Just unifyData <- Builtin.KEqual.matchUnifyKequalsEq first second =
            lift $ Builtin.unifyEq childTransformers unifyData
        | Just unifyData <- Builtin.Endianness.matchUnifyEqualsEndianness first second =
            lift $ Builtin.Endianness.unifyEquals unifyData
        | Just unifyData <- Builtin.Signedness.matchUnifyEqualsSignedness first second =
            lift $ Builtin.Signedness.unifyEquals unifyData
        | Just unifyData <- matchDomainValueAndConstructorErrors first second =
            lift $ domainValueAndConstructorErrors unifyData
        | otherwise = empty

maybeTermAnd ::
    MonadUnify unifier =>
    NotSimplifier Simplifier =>
    HasCallStack =>
    -- | Used to simplify subterm "and".
    TermSimplifier RewritingVariableName unifier ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    MaybeT unifier (Pattern RewritingVariableName)
maybeTermAnd childTransformers first second = do
    injSimplifier <- Simplifier.askInjSimplifier
    overloadSimplifier <- Simplifier.askOverloadSimplifier
    -- tools <- Simplifier.askMetadataTools
    worker injSimplifier overloadSimplifier --tools
  where
    worker injSimplifier overloadSimplifier --tools
        | Just unifyData <- matchExpandAlias first second =
            let UnifyExpandAlias{term1, term2} = unifyData
             in maybeTermAnd
                    childTransformers
                    term1
                    term2
        | Just unifyData <- matchBoolAnd first second =
            lift $ boolAnd unifyData
        | Just unifyData <- Builtin.Int.matchInt first second =
            lift $ Builtin.Int.unifyInt unifyData
        | Just unifyData <- Builtin.Bool.matchBools first second =
            lift $ Builtin.Bool.unifyBool unifyData
        | Just unifyData <- Builtin.String.matchString first second =
            lift $ Builtin.String.unifyString unifyData
        | Just unifyData <- matchDomainValue first second =
            lift $ unifyDomainValue unifyData
        | Just unifyData <- matchStringLiteral first second =
            lift $ unifyStringLiteral unifyData
        | Just term <- matchEqualsAndEquals first second =
            lift $ equalAndEquals term
        | Just unifyData <- matchBytes first second =
            lift $ unifyBytes unifyData
        | Just matched <- matchVariables first second =
            lift $ unifyVariables matched
        | Just matched <- matchVariableFunction second first =
            lift $ unifyVariableFunction matched
        | Just unifyData <- matchEqualInjectiveHeadsAndEquals first second =
            lift $ equalInjectiveHeadsAndEquals childTransformers unifyData
        | Just unifyData <- matchInj injSimplifier first second =
            lift $ unifySortInjection childTransformers unifyData
        | Just unifyData <- matchConstructorSortInjectionAndEquals first second =
            lift $ constructorSortInjectionAndEquals unifyData
        | Just unifyData <- matchDifferentConstructors overloadSimplifier first second =
            lift $ constructorAndEqualsAssumesDifferentHeads unifyData
        | Just unifyData <- unifyOverloading overloadSimplifier (Pair first second) =
            lift $ overloadedConstructorSortInjectionAndEquals childTransformers unifyData
        | Just unifyData <- Builtin.Bool.matchUnifyBoolAnd first second =
            lift $ Builtin.Bool.unifyBoolAnd childTransformers unifyData
        | Just unifyData <- Builtin.Bool.matchUnifyBoolOr first second =
            lift $ Builtin.Bool.unifyBoolOr childTransformers unifyData
        | Just boolNotData <- Builtin.Bool.matchUnifyBoolNot first second =
            lift $ Builtin.Bool.unifyBoolNot childTransformers boolNotData
        | Just unifyData <- Builtin.KEqual.matchUnifyKequalsEq first second =
            lift $ Builtin.unifyEq childTransformers unifyData
        | Just unifyData <- Builtin.Int.matchUnifyIntEq first second =
            lift $ Builtin.unifyEq childTransformers unifyData
        | Just unifyData <- Builtin.String.matchUnifyStringEq first second =
            lift $ Builtin.unifyEq childTransformers unifyData
        | Just unifyData <- Builtin.KEqual.matchIfThenElse first second =
            lift $ Builtin.KEqual.unifyIfThenElse childTransformers unifyData
        | Just unifyData <- Builtin.Endianness.matchUnifyEqualsEndianness first second =
            lift $ Builtin.Endianness.unifyEquals unifyData
        | Just unifyData <- Builtin.Signedness.matchUnifyEqualsSignedness first second =
            lift $ Builtin.Signedness.unifyEquals unifyData
        | Just unifyData <- matchDomainValueAndConstructorErrors first second =
            lift $ domainValueAndConstructorErrors unifyData
        | Just unifyData <- matchFunctionAnd first second =
            return $ functionAnd unifyData
        | otherwise = empty

{- | Construct the conjunction or unification of two terms.

Each @TermTransformationOld@ should represent one unification case and each
unification case should be handled by only one @TermTransformationOld@. If the
pattern heads do not match the case under consideration, call 'empty' to allow
another case to handle the patterns. If the pattern heads do match the
unification case, then use 'lift' to wrap the implementation
of that case.

All the @TermTransformationOld@s and similar functions defined in this module
call 'empty' unless given patterns matching their unification case.
-}
type TermTransformationOld variable unifier =
    TermSimplifier variable unifier ->
    TermLike variable ->
    TermLike variable ->
    MaybeT unifier (Pattern variable)

data UnifyBoolAnd
    = UnifyBoolAndBottom !Sort !(TermLike RewritingVariableName)
    | UnifyBoolAndTop !(TermLike RewritingVariableName)

{- | Matches

@
\\and{_}(\\bottom, _)
@

and

@
\\and{_}(\\top, _),
@

symmetric in the two arguments.
-}
matchBoolAnd ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe UnifyBoolAnd
matchBoolAnd term1 term2
    | Pattern.isBottom term1 =
        let sort = termLikeSort term1
         in Just $ UnifyBoolAndBottom sort term2
    | Pattern.isTop term1 =
        Just $ UnifyBoolAndTop term2
    | Pattern.isBottom term2 =
        let sort = termLikeSort term2
         in Just $ UnifyBoolAndBottom sort term1
    | Pattern.isTop term2 =
        Just $ UnifyBoolAndTop term1
    | otherwise =
        Nothing
{-# INLINE matchBoolAnd #-}

-- | Simplify the conjunction of terms where one is a predicate.
boolAnd ::
    MonadUnify unifier =>
    UnifyBoolAnd ->
    unifier (Pattern RewritingVariableName)
boolAnd unifyData =
    case unifyData of
        UnifyBoolAndBottom sort term -> do
            explainBoolAndBottom term sort
            return $ Pattern.fromTermLike $ mkBottom sort
        UnifyBoolAndTop term -> do
            return $ Pattern.fromTermLike term

explainBoolAndBottom ::
    MonadUnify unifier =>
    TermLike RewritingVariableName ->
    Sort ->
    unifier ()
explainBoolAndBottom term sort =
    debugUnifyBottom "Cannot unify bottom." (mkBottom sort) term

{- | Matches

@
\\equals{_, _}(t, t)
@

and

@
\\and{_}(t, t)
@
-}
matchEqualsAndEquals ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe (TermLike RewritingVariableName)
matchEqualsAndEquals first second
    | first == second =
        Just first
    | otherwise = Nothing
{-# INLINE matchEqualsAndEquals #-}

-- | Returns the term as a pattern.
equalAndEquals ::
    Monad unifier =>
    TermLike RewritingVariableName ->
    unifier (Pattern RewritingVariableName)
equalAndEquals term =
    -- TODO (thomas.tuegel): Preserve simplified flags.
    return (Pattern.fromTermLike term)

data BottomTermEquals = BottomTermEquals
    { sort :: !Sort
    , term :: !(TermLike RewritingVariableName)
    }

{- | Matches

@
\\equals{_, _}(\\bottom, _),
@

symmetric in the two arguments.
-}
matchBottomTermEquals ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe BottomTermEquals
matchBottomTermEquals first second
    | Bottom_ sort <- first =
        Just BottomTermEquals{sort, term = second}
    | Bottom_ sort <- second =
        Just BottomTermEquals{sort, term = first}
    | otherwise = Nothing
{-# INLINE matchBottomTermEquals #-}

-- | Unify two patterns where the first is @\\bottom@.
bottomTermEquals ::
    MonadUnify unifier =>
    Sort ->
    SideCondition RewritingVariableName ->
    BottomTermEquals ->
    unifier (Pattern RewritingVariableName)
bottomTermEquals
    resultSort
    sideCondition
    unifyData =
        do
            -- MonadUnify
            secondCeil <- liftSimplifier $ makeEvaluateTermCeil sideCondition term
            case toList secondCeil of
                [] -> return (Pattern.topOf resultSort)
                [Conditional{predicate = PredicateTrue, substitution}]
                    | substitution == mempty ->
                        debugUnifyBottomAndReturnBottom
                            "Cannot unify bottom with non-bottom pattern."
                            (mkBottom sort)
                            term
                _ ->
                    return
                        Conditional
                            { term = mkTop resultSort
                            , predicate =
                                makeNotPredicate $
                                    OrCondition.toPredicate $
                                        OrPattern.map Condition.toPredicate secondCeil
                            , substitution = mempty
                            }
      where
        BottomTermEquals{sort, term} = unifyData

data UnifyVariables = UnifyVariables
    {variable1, variable2 :: !(ElementVariable RewritingVariableName)}

-- | Match the unification of two element variables.
matchVariables ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe UnifyVariables
matchVariables first second = do
    ElemVar_ variable1 <- pure first
    ElemVar_ variable2 <- pure second
    pure UnifyVariables{variable1, variable2}
{-# INLINE matchVariables #-}

unifyVariables ::
    MonadUnify unifier =>
    UnifyVariables ->
    unifier (Pattern RewritingVariableName)
unifyVariables UnifyVariables{variable1, variable2} =
    pure $ Pattern.assign (inject variable1) (mkElemVar variable2)

data UnifyVariableFunction = UnifyVariableFunction
    { variable :: !(ElementVariable RewritingVariableName)
    , term :: !(TermLike RewritingVariableName)
    }

-- | Match the unification of an element variable with a function-like term.
matchVariableFunction ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe UnifyVariableFunction
matchVariableFunction = \first second ->
    worker first second <|> worker second first
  where
    worker first term = do
        ElemVar_ variable <- pure first
        guard (isFunctionPattern term)
        pure UnifyVariableFunction{variable, term}
{-# INLINE matchVariableFunction #-}

unifyVariableFunction ::
    MonadUnify unifier =>
    UnifyVariableFunction ->
    unifier (Pattern RewritingVariableName)
unifyVariableFunction UnifyVariableFunction{variable, term} =
    Condition.assign (inject variable) term
        & Pattern.withCondition term
        & pure

data VariableFunctionEquals = VariableFunctionEquals
    { var :: !(ElementVariable RewritingVariableName)
    , term1, term2 :: !(TermLike RewritingVariableName)
    }

{- | Matches

@
\\equals{_, _}(x, f(_)),
@

symmetric in the two arguments.
-}
matchVariableFunctionEquals ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe VariableFunctionEquals
matchVariableFunctionEquals first second
    | ElemVar_ var <- first
      , isFunctionPattern second =
        Just VariableFunctionEquals{term1 = first, term2 = second, var}
    | ElemVar_ var <- second
      , isFunctionPattern first =
        Just VariableFunctionEquals{term1 = second, term2 = first, var}
    | otherwise = Nothing
{-# INLINE matchVariableFunctionEquals #-}

{- | Unify a variable with a function pattern.

See also: 'isFunctionPattern'
-}
variableFunctionEquals ::
    MonadUnify unifier =>
    VariableFunctionEquals ->
    unifier (Pattern RewritingVariableName)
variableFunctionEquals
    unifyData =
        do
            -- MonadUnify
            predicate <- do
                resultOr <- liftSimplifier $ makeEvaluateTermCeil SideCondition.topTODO term2
                case toList resultOr of
                    [] ->
                        debugUnifyBottomAndReturnBottom
                            "Unification of variable and bottom \
                            \when attempting to simplify equals."
                            term1
                            term2
                    resultConditions -> Unify.scatter resultConditions
            let result =
                    predicate
                        <> Condition.fromSingleSubstitution
                            (Substitution.assign (inject var) term2)
            return (Pattern.withCondition term2 result)
      where
        VariableFunctionEquals{term1, term2, var} = unifyData

data UnifyInjData = UnifyInjData
    { term1, term2 :: !(TermLike RewritingVariableName)
    , unifyInj :: !(UnifyInj (InjPair RewritingVariableName))
    }

{- | Matches

@
\\equals{_, _}(inj{sub, super}(children), inj{sub', super'}(children'))
@

and

@
\\and{_}(inj{sub, super}(children), inj{sub', super'}(children'))
@

when either

* @super /= super'@
* @sub == sub'@
* @sub@ is a subsort of @sub'@ or vice-versa.
* @children@ or @children'@ satisfies @hasConstructorLikeTop@.
* the subsorts of @sub, sub'@ are disjoint.
-}
matchInj ::
    InjSimplifier ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe UnifyInjData
matchInj injSimplifier first second
    | Inj_ inj1 <- first
      , Inj_ inj2 <- second =
        UnifyInjData first second
            <$> matchInjs injSimplifier inj1 inj2
    | otherwise = Nothing
{-# INLINE matchInj #-}

{- | Simplify the conjunction of two sort injections.

Assumes that the two heads were already tested for equality and were found
to be different.

This simplifies cases where there is a subsort relation between the injected
sorts of the conjoined patterns, such as,

@
    \inj{src1, dst}(a) ∧ \inj{src2, dst}(b)
    ===
    \inj{src2, dst}(\inj{src1, src2}(a) ∧ b)
@

when @src1@ is a subsort of @src2@.
-}
unifySortInjection ::
    forall unifier.
    MonadUnify unifier =>
    TermSimplifier RewritingVariableName unifier ->
    UnifyInjData ->
    unifier (Pattern RewritingVariableName)
unifySortInjection termMerger unifyData = do
    InjSimplifier{unifyInjs} <- Simplifier.askInjSimplifier
    unifyInjs unifyInj & maybe distinct merge
  where
    distinct =
        debugUnifyBottomAndReturnBottom "Distinct sort injections" term1 term2
    merge inj@Inj{injChild = Pair child1 child2} = do
        childPattern <- termMerger child1 child2
        InjSimplifier{evaluateInj} <- askInjSimplifier
        let (childTerm, childCondition) = Pattern.splitTerm childPattern
            inj' = evaluateInj inj{injChild = childTerm}
        return $ Pattern.withCondition inj' childCondition
    UnifyInjData{unifyInj, term1, term2} = unifyData

data ConstructorSortInjectionAndEquals = ConstructorSortInjectionAndEquals
    { term1, term2 :: !(TermLike RewritingVariableName)
    }

{- | Matches

@
\\equals{_, _}(inj{_,_}(_), c(_))
@

@
\\equals{_, _}(c(_), inj{_,_}(_))
@

and

@
\\and{_}(inj{_,_}(_), c(_))
@

@
\\and{_}(c(_), inj{_,_}(_))
@

when @c@ has the @constructor@ attribute.
-}
matchConstructorSortInjectionAndEquals ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe ConstructorSortInjectionAndEquals
matchConstructorSortInjectionAndEquals first second
    | Inj_ _ <- first
      , App_ symbol _ <- second
      , Symbol.isConstructor symbol =
        Just ConstructorSortInjectionAndEquals{term1 = first, term2 = second}
    | Inj_ _ <- second
      , App_ symbol _ <- first
      , Symbol.isConstructor symbol =
        Just ConstructorSortInjectionAndEquals{term1 = first, term2 = second}
    | otherwise = Nothing
{-# INLINE matchConstructorSortInjectionAndEquals #-}

{- | Unify a constructor application pattern with a sort injection pattern.

Sort injections clash with constructors, so @constructorSortInjectionAndEquals@
returns @\\bottom@.
-}
constructorSortInjectionAndEquals ::
    MonadUnify unifier =>
    ConstructorSortInjectionAndEquals ->
    unifier a
constructorSortInjectionAndEquals unifyData =
    noConfusionInjectionConstructor term1 term2
  where
    ConstructorSortInjectionAndEquals{term1, term2} = unifyData

noConfusionInjectionConstructor ::
    MonadUnify unifier =>
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    unifier a
noConfusionInjectionConstructor term1 term2 =
    debugUnifyBottomAndReturnBottom
        "No confusion: sort injections and constructors"
        term1
        term2

{- |
 If the two constructors form an overload pair, apply the overloading axioms
 on the terms to make the constructors equal, then retry unification on them.

See <https://github.com/runtimeverification/haskell-backend/blob/master/docs/2019-08-27-Unification-modulo-overloaded-constructors.md>
-}
overloadedConstructorSortInjectionAndEquals ::
    MonadUnify unifier =>
    TermSimplifier RewritingVariableName unifier ->
    OverloadingData ->
    unifier (Pattern RewritingVariableName)
overloadedConstructorSortInjectionAndEquals termMerger unifyData =
    case matchResult of
        Resolution (Simple (Pair firstTerm' secondTerm')) ->
            termMerger firstTerm' secondTerm'
        Resolution
            ( WithNarrowing
                    Narrowing
                        { narrowingSubst
                        , narrowingVars
                        , overloadPair = Pair firstTerm' secondTerm'
                        }
                ) -> do
                boundPattern <- do
                    merged <- termMerger firstTerm' secondTerm'
                    liftSimplifier $
                        Exists.makeEvaluate SideCondition.topTODO narrowingVars $
                            merged `Pattern.andCondition` narrowingSubst
                case OrPattern.toPatterns boundPattern of
                    [result] -> return result
                    [] ->
                        debugUnifyBottomAndReturnBottom
                            ( "exists simplification for overloaded"
                                <> " constructors returned no pattern"
                            )
                            term1
                            term2
                    _ -> scatter boundPattern
        ClashResult message ->
            debugUnifyBottomAndReturnBottom (fromString message) term1 term2
  where
    OverloadingData{term1, term2, matchResult} = unifyData

data DVConstrError
    = DVConstr !(TermLike RewritingVariableName) !(TermLike RewritingVariableName)
    | ConstrDV !(TermLike RewritingVariableName) !(TermLike RewritingVariableName)

{- | Matches

@
\\equals{_, _}(\\dv{_}(_), c(_))
@

@
\\equals{_, _}(c(_), \\dv{_}(_))
@

@
\\and{_}(\\dv{_}(_), c(_))
@

@
\\and{_}(c(_), \\dv{_}(_))
@

when @c@ is a constructor.
-}
matchDomainValueAndConstructorErrors ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe DVConstrError
matchDomainValueAndConstructorErrors first second
    | DV_ _ _ <- first
      , App_ secondHead _ <- second
      , Symbol.isConstructor secondHead =
        Just $ DVConstr first second
    | App_ firstHead _ <- first
      , Symbol.isConstructor firstHead
      , DV_ _ _ <- second =
        Just $ ConstrDV first second
    | otherwise = Nothing

{- | Unifcation or equality for a domain value pattern vs a constructor
application.

This unification case throws an error because domain values may not occur in a
sort with constructors.
-}
domainValueAndConstructorErrors ::
    HasCallStack =>
    DVConstrError ->
    unifier a
domainValueAndConstructorErrors unifyData =
    error $
        show
            ( Pretty.vsep
                [ cannotHandle
                , fromString $ unparseToString term1
                , fromString $ unparseToString term2
                , ""
                ]
            )
  where
    (term1, term2, cannotHandle) =
        case unifyData of
            DVConstr a b -> (a, b, "Cannot handle DomainValue and Constructor:")
            ConstrDV a b -> (a, b, "Cannot handle Constructor and DomainValue:")

data UnifyDomainValue = UnifyDomainValue
    { val1, val2 :: !(TermLike RewritingVariableName)
    , term1, term2 :: !(TermLike RewritingVariableName)
    }

{- | Matches

@
\\equals{_, _}(\\dv{s}(_), \\dv{s}(_))
@

and

@
\\and{_}(\\dv{s}(_), \\dv{s}(_))
@
-}
matchDomainValue ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe UnifyDomainValue
matchDomainValue term1 term2
    | DV_ sort1 val1 <- term1
      , DV_ sort2 val2 <- term2
      , sort1 == sort2 =
        Just UnifyDomainValue{val1, val2, term1, term2}
    | otherwise = Nothing
{-# INLINE matchDomainValue #-}

{- | Unify two domain values.

The two patterns are assumed to be inequal; therefore this case always return
@\\bottom@.

See also: 'equalAndEquals'
-}

-- TODO (thomas.tuegel): This unification case assumes that \dv is injective,
-- but it is not.
unifyDomainValue ::
    forall unifier.
    MonadUnify unifier =>
    UnifyDomainValue ->
    unifier (Pattern RewritingVariableName)
unifyDomainValue unifyData
    | val1 == val2 =
        return $ Pattern.fromTermLike term1
    | otherwise = cannotUnifyDomainValues term1 term2
  where
    UnifyDomainValue{val1, val2, term1, term2} = unifyData

cannotUnifyDistinctDomainValues :: Text
cannotUnifyDistinctDomainValues = "distinct domain values"

cannotUnifyDomainValues ::
    MonadUnify unifier =>
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    unifier a
cannotUnifyDomainValues term1 term2 =
    debugUnifyBottomAndReturnBottom cannotUnifyDistinctDomainValues term1 term2

-- | @UnifyStringLiteral@ represents unification of two string literals.
data UnifyStringLiteral = UnifyStringLiteral
    { txt1, txt2 :: !Text
    , term1, term2 :: !(TermLike RewritingVariableName)
    }

-- | Matches the unification problem @"txt1"@ with @"txt2"@.
matchStringLiteral ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe UnifyStringLiteral
matchStringLiteral term1 term2
    | StringLiteral_ txt1 <- term1
      , StringLiteral_ txt2 <- term2 =
        Just UnifyStringLiteral{txt1, txt2, term1, term2}
    | otherwise = Nothing
{-# INLINE matchStringLiteral #-}

-- | Finish solving the 'UnifyStringLiteral' problem.
unifyStringLiteral ::
    forall unifier.
    MonadUnify unifier =>
    UnifyStringLiteral ->
    unifier (Pattern RewritingVariableName)
unifyStringLiteral unifyData
    | txt1 == txt2 = return $ Pattern.fromTermLike term1
    | otherwise =
        debugUnifyBottomAndReturnBottom "distinct string literals" term1 term2
  where
    UnifyStringLiteral{txt1, txt2, term1, term2} = unifyData

data FunctionAnd = FunctionAnd
    { term1, term2 :: !(TermLike RewritingVariableName)
    }

{- | Matches

@
\\and{_}(f(_), g(_))
@
-}
matchFunctionAnd ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe FunctionAnd
matchFunctionAnd term1 term2
    | isFunctionPattern term1
      , isFunctionPattern term2 =
        Just FunctionAnd{term1, term2}
    | otherwise = Nothing
{-# INLINE matchFunctionAnd #-}

{- | Unify any two function patterns.

The function patterns are unified by creating an @\\equals@ predicate. If either
argument is constructor-like, that argument will be the resulting 'term';
otherwise, the lesser argument is the resulting 'term'. The term always appears
on the left-hand side of the @\\equals@ predicate, and the other argument
appears on the right-hand side.
-}
functionAnd ::
    FunctionAnd ->
    Pattern RewritingVariableName
functionAnd FunctionAnd{term1, term2} =
    makeEqualsPredicate first' second'
        & Predicate.markSimplified
        -- Ceil predicate not needed since first being
        -- bottom will make the entire term bottom. However,
        -- one must be careful to not just drop the term.
        & Condition.fromPredicate
        & Pattern.withCondition first' -- different for Equals
  where
    (first', second') = minMaxBy compareForEquals term1 term2

{- | Normal ordering for terms in @\equals(_, _)@.

The normal ordering is arbitrary, but important to avoid duplication.
-}
compareForEquals ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Ordering
compareForEquals first second
    | isConstructorLike first = LT
    | isConstructorLike second = GT
    | otherwise = compare first second
