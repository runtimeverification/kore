module Test.Kore.Strategies.Goal
    ( test_checkImplication
    ) where

import Prelude.Kore

import Test.Tasty
import Test.Tasty.HUnit.Ext

import qualified Kore.Internal.Condition as Condition
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern
    ( Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( makeAndPredicate
    , makeCeilPredicate
    , makeEqualsPredicate
    , makeExistsPredicate
    , makeNotPredicate
    )
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike
    ( ElementVariable
    , VariableName
    , mkElemVar
    )
import qualified Kore.Internal.TermLike as TermLike
import Kore.Rewriting.RewritingVariable
    ( mkConfigVariable
    , mkRewritingPattern
    )
import Kore.Step.ClaimPattern
    ( ClaimPattern
    , claimPattern
    )
import Kore.Strategies.Goal
    ( CheckImplicationResult (..)
    , checkImplicationWorker
    )

import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Simplification
    ( runSimplifier
    , runSimplifierNoSMT
    )

test_checkImplication :: [TestTree]
test_checkImplication =
    [ testCase "Variable unification" $ do
        let config = mkElemVar Mock.x & Pattern.fromTermLike
            dest = mkElemVar Mock.y & OrPattern.fromTermLike
            existentials = [Mock.y]
        actual <-
            checkImplication (mkGoal config dest existentials)
        assertEqual "" Implied actual
    , testCase "Constructors do not unify" $ do
        let config = Mock.a & Pattern.fromTermLike
            dest = Mock.b & OrPattern.fromTermLike
            existentials = []
            goal = mkGoal config dest existentials
        actual <-
            checkImplication goal
        assertEqual "" (NotImplied goal) actual
    , testCase "Variable unification, conditions match" $ do
        let config =
                Pattern.withCondition
                    (mkElemVar Mock.x)
                    ( Substitution.wrap
                        [Substitution.assign (inject Mock.x) Mock.a]
                    & Condition.fromSubstitution
                    )
            dest =
                Pattern.withCondition
                    (mkElemVar Mock.y)
                    ( Substitution.wrap
                        [Substitution.assign (inject Mock.y) Mock.a]
                    & Condition.fromSubstitution
                    )
                & OrPattern.fromPattern
            existentials = [Mock.y]
            goal = mkGoal config dest existentials
        actual <- checkImplication goal
        assertEqual "" Implied actual
    , testCase "???is this right? Variable unification, conditions don't match" $ do
        let config =
                Pattern.withCondition
                    (mkElemVar Mock.x)
                    ( Substitution.wrap
                        [Substitution.assign (inject Mock.x) Mock.a]
                    & Condition.fromSubstitution
                    )
            dest =
                Pattern.withCondition
                    (mkElemVar Mock.y)
                    ( Substitution.wrap
                        [Substitution.assign (inject Mock.y) Mock.b]
                    & Condition.fromSubstitution
                    )
                & OrPattern.fromPattern
            existentials = [Mock.y]
            goal = mkGoal config dest existentials
        actual <- checkImplication goal
        assertEqual "" (NotImpliedStuck goal) actual
    , testCase "Function unification, definedness condition and remainder" $ do
        let config = Mock.f (mkElemVar Mock.x) & Pattern.fromTermLike
            dest =
                Mock.f (mkElemVar Mock.y) & Pattern.fromTermLike
                & OrPattern.fromPattern
            existentials = [Mock.y]
            goal = mkGoal config dest existentials
            stuckConfig =
                Pattern.fromTermAndPredicate
                    (Mock.f (mkElemVar Mock.x))
                    (makeAndPredicate
                        (makeCeilPredicate Mock.testSort
                            (Mock.f (mkElemVar Mock.x))
                        )
                        (makeNotPredicate
                            (makeExistsPredicate Mock.y
                                (makeEqualsPredicate Mock.testSort
                                    (Mock.f (mkElemVar Mock.x))
                                    (Mock.f (mkElemVar Mock.y))
                                )
                            )
                        )
                    )
            stuckGoal =
                mkGoal stuckConfig dest existentials
        actual <- checkImplicationNoSMT goal
        assertEqual "" (NotImpliedStuck stuckGoal) actual
    , testCase "Branching RHS" $ do
        let config = Mock.a & Pattern.fromTermLike
            dest =
                [ Mock.a & Pattern.fromTermLike
                , Mock.b & Pattern.fromTermLike
                ]
                & OrPattern.fromPatterns
            existentials = []
            goal = mkGoal config dest existentials
        actual <- checkImplication goal
        assertEqual "" Implied actual
    , testCase "Branching RHS with condition" $ do
        let config = Mock.a & Pattern.fromTermLike
            dest =
                [ Pattern.fromTermAndPredicate
                    (mkElemVar Mock.x)
                    (makeEqualsPredicate Mock.testSort
                        (mkElemVar Mock.x)
                        Mock.a
                    )
                , Pattern.fromTermAndPredicate
                    (mkElemVar Mock.x)
                    (makeEqualsPredicate Mock.testSort
                        (mkElemVar Mock.x)
                        Mock.b
                    )
                ]
                & OrPattern.fromPatterns
            existentials = [Mock.x]
            goal = mkGoal config dest existentials
        actual <- checkImplication goal
        assertEqual "" Implied actual
    ]

mkGoal
    :: Pattern VariableName
    -> OrPattern VariableName
    -> [ElementVariable VariableName]
    -> ClaimPattern
mkGoal
    (mkRewritingPattern -> leftPatt)
    (fmap mkRewritingPattern -> rightPatts)
    (fmap (TermLike.mapElementVariable (pure mkConfigVariable)) ->
        existentialVars
    )
  =
    claimPattern leftPatt rightPatts existentialVars


checkImplication :: ClaimPattern -> IO (CheckImplicationResult ClaimPattern)
checkImplication =
    runSimplifier Mock.env . checkImplicationWorker

checkImplicationNoSMT :: ClaimPattern -> IO (CheckImplicationResult ClaimPattern)
checkImplicationNoSMT =
    runSimplifierNoSMT Mock.env . checkImplicationWorker
