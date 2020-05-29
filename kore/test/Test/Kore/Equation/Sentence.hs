module Test.Kore.Equation.Sentence
    ( test_fromSentenceAxiom
    ) where

import Prelude.Kore

import Test.Tasty

import Data.Default
    ( def
    )

import Kore.Equation
import Kore.Internal.Predicate
    ( wrapPredicate
    )
import Kore.Internal.TermLike

import Test.Expect
import Test.Kore
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Tasty.HUnit.Ext

test_fromSentenceAxiom :: [TestTree]
test_fromSentenceAxiom =
    [ testCase "⌈I1 / I2⌉" $ do
        let term = Mock.tdivInt varI1 varI2
            original = mkCeil sortR term
            equation = mkEquation sortR (mkCeil sortR term) (mkTop sortR)
        assertions original equation
    , testCase "I1 < I2 = I2 >= I1" $ do
        let left = Mock.lessInt varI1 varI2
            right = Mock.greaterEqInt varI2 varI1
            original = mkEquals sortR left right
            equation = mkEquation sortR left right
        assertions original equation
    , testCase "⌈f(x))⌉ → f(x) = g(x) ∧ ⌈h(x)⌉" $ do
        let requires = mkCeil sortR (Mock.f Mock.a)
            ensures = mkCeil sortR (Mock.h Mock.b)
            left = Mock.f (mkElemVar Mock.x)
            right = Mock.g (mkElemVar Mock.x)
            original =
                mkImplies requires
                $ mkAnd (mkEquals sortR left right) ensures
            equation =
                (mkEquation sortR left right)
                    { requires = wrapPredicate requires
                    , ensures = wrapPredicate ensures
                    }
        assertions original equation
    , testCase "New equation form: ⌈f(x))⌉ → f(x) = g(x) ∧ ⌈h(x)⌉" $ do
        let requires = mkCeil sortR (Mock.f Mock.a)
            ensures = mkCeil sortR (Mock.h Mock.b)
            argument = mkIn sortR (mkElemVar Mock.y) (mkElemVar Mock.x)
            argument' =
                mkAnd argument (mkTop sortR)
            left = Mock.f (mkElemVar Mock.y)
            right = Mock.g (mkElemVar Mock.y)
            original =
                mkImplies (mkAnd requires argument')
                $ mkAnd (mkEquals sortR left right) ensures
            equation =
                (mkEquation sortR left right)
                    { requires = wrapPredicate requires
                    , argument = wrapPredicate argument
                    , ensures = wrapPredicate ensures
                    }
        assertions original equation
    ]
  where
    sortVariableR = SortVariable (testId "R")
    sortR = SortVariableSort sortVariableR
    toTermLike = from @(Equation Variable) @(TermLike Variable)
    assertions
        :: HasCallStack
        => TermLike Variable
        -> Equation Variable
        -> Assertion
    assertions original equation = do
        actual <- expectRight $ test original
        assertEqual "Expected equation" equation actual
        assertEqual "Expected original pattern" original (toTermLike actual)
    test original = fromSentenceAxiom (def, mkAxiom [sortVariableR] original)

varI1, varI2 :: TermLike Variable
varI1 =
    mkElemVar $ ElementVariable Variable
        { variableName = testId "VarI1"
        , variableCounter = mempty
        , variableSort = Mock.intSort
        }

varI2 =
    mkElemVar $ ElementVariable Variable
        { variableName = testId "VarI2"
        , variableCounter = mempty
        , variableSort = Mock.intSort
        }
