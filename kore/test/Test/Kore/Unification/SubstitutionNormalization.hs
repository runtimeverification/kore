module Test.Kore.Unification.SubstitutionNormalization
    (test_substitutionNormalization) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Control.Monad.Except as Except
import qualified Data.Default as Default
import qualified Data.Map.Strict as Map

import qualified Kore.Internal.Pattern as Conditional
import           Kore.Internal.Symbol
import           Kore.Internal.TermLike
import           Kore.Step.Simplification.Data
                 ( evalSimplifier )
import           Kore.SubstVar
                 ( SubstVar (..) )
import           Kore.TopBottom ( isBottom )
import           Kore.Unification.Error
                 ( SubstitutionError (..) )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unification.SubstitutionNormalization

import           Test.Kore
import           Test.Kore.Comparators ()
import qualified Test.Kore.Step.MockSymbols as Mock
import qualified Test.SMT
import           Test.Tasty.HUnit.Extensions

data NormalizationResult
  = Substitution ![(SubstVar Variable, TermLike Variable)]
  | SubstitutionBottom
  | Error !SubstitutionError
  deriving (Show)

instance SumEqualWithExplanation NormalizationResult
  where
    sumConstructorPair (Substitution s1) (Substitution s2) =
        SumConstructorSameWithArguments (EqWrap "Substitution" s1 s2)
    sumConstructorPair pattern1@(Substitution _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)
    sumConstructorPair SubstitutionBottom SubstitutionBottom =
        SumConstructorSameNoArguments
    sumConstructorPair pattern1@SubstitutionBottom pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)
    sumConstructorPair (Error s1) (Error s2) =
        SumConstructorSameWithArguments (EqWrap "Error" s1 s2)
    sumConstructorPair pattern1@(Error _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

instance EqualWithExplanation NormalizationResult where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

test_substitutionNormalization :: [TestTree]
test_substitutionNormalization =
    [ testCase "Empty substitution" $
        assertEqualWithExplanation "" (Substitution [])
            =<< runNormalizeSubstitution []
    , testCase "Simple substitution" $
        assertEqualWithExplanation "" (Substitution [(RegVar $ v1 Mock.testSort, mkTop_)])
            =<< runNormalizeSubstitution [(RegVar $ v1 Mock.testSort, mkTop_)]
    , testCase "Simple unnormalized substitution" $
        assertEqualWithExplanation ""
            (Substitution
                [ (RegVar $ v1 Mock.testSort, mkTop Mock.testSort)
                , (RegVar $ x1 Mock.testSort, mkTop Mock.testSort)
                ]
            )
            =<< runNormalizeSubstitution
                [ (RegVar $ v1 Mock.testSort, mkVar $ x1 Mock.testSort)
                , (RegVar $ x1 Mock.testSort, mkTop Mock.testSort)
                ]
    , testCase "Unnormalized substitution with 'and'" $
        assertEqualWithExplanation ""
            (Substitution
                [   ( RegVar $ v1 Mock.testSort
                    , mkAnd mkTop_ (mkTop Mock.testSort)
                    )
                , (RegVar $ x1 Mock.testSort, mkTop Mock.testSort)
                ]
            )
            =<< runNormalizeSubstitution
                [   (RegVar $ v1 Mock.testSort
                    , mkAnd (mkVar $ x1 Mock.testSort) mkTop_
                    )
                ,   (RegVar $ x1 Mock.testSort, mkTop Mock.testSort)
                ]
    , testCase "Simplest cycle" $ do
        let var1 = v1 Mock.testSort
        assertEqualWithExplanation "" (Substitution [])
            =<< runNormalizeSubstitution [(RegVar var1, mkVar $ v1 Mock.testSort)]
    , testCase "Cycle with extra substitution" $ do
        let
            var1 = v1 Mock.testSort
            varx1 = x1 Mock.testSort
        assertEqualWithExplanation "" (Substitution [(RegVar varx1, mkVar var1)])
            =<< runNormalizeSubstitution
                    [ (RegVar var1, mkVar var1)
                    , (RegVar varx1, mkVar var1)
                    ]
    , testCase "SetVariable Simplest cycle" $ do
        let var1 = Mock.makeTestSubstVar "@x"
        assertEqualWithExplanation "" (Substitution [])
            =<< runNormalizeSubstitution [(var1, mkSubstVar var1)]
    , testCase "SetVariable Cycle with extra substitution" $ do
        let
            var1 = Mock.makeTestSubstVar "@v"
            varx1 = Mock.makeTestSubstVar "@x"
        assertEqualWithExplanation "" (Substitution [(varx1, mkSubstVar var1)])
            =<< runNormalizeSubstitution
                    [ (var1, mkSubstVar var1)
                    , (varx1, mkSubstVar var1)
                    ]
    , testCase "Function cycle" $ do
        let var1 = v1 Mock.testSort
        assertEqualWithExplanation ""
            (Error (NonCtorCircularVariableDependency [RegVar var1]))
            =<< runNormalizeSubstitution
                [ (RegVar  var1 , mkApplySymbol f [mkVar var1] ) ]
    , testCase "onlyThisLength 2 cycle" $ do
        let
            var1 = v1 Mock.testSort
            varx1 = x1 Mock.testSort
        assertEqualWithExplanation "" (Substitution [])
            =<< runNormalizeSubstitution
                [ (RegVar var1, mkVar varx1)
                , (RegVar varx1, mkVar var1)
                ]
     , testCase "SetVariable Length 2 cycle" $ do
        let
            var1 = Mock.makeTestSubstVar "@v"
            varx1 = Mock.makeTestSubstVar "@x"
        assertEqualWithExplanation "" (Substitution [])
            =<< runNormalizeSubstitution
                [ (var1, mkSubstVar varx1)
                , (varx1, mkSubstVar var1)
                ]
     , testCase "Cycle with 'and'" $ do
        let
            var1 = v1 Mock.testSort
            varx1 = x1 Mock.testSort
        assertEqualWithExplanation "" (Substitution [])
            =<< runNormalizeSubstitution
                [ (RegVar var1, mkAnd (mkVar varx1) mkTop_)
                , (RegVar varx1, mkAnd (mkVar var1) mkTop_)
                ]
    , testCase "Length 2 non-ctor cycle" $ do
        let
            var1 = v1 Mock.testSort
            varx1 = x1 Mock.testSort
        assertEqualWithExplanation ""
            (Error (NonCtorCircularVariableDependency [RegVar var1, RegVar varx1]))
            =<< runNormalizeSubstitution
                [ (RegVar var1, mkApplySymbol f [mkVar varx1])
                , (RegVar varx1, mkVar var1)
                ]
    , testCase "Constructor cycle" $
        assertEqualWithExplanation "" SubstitutionBottom
            =<< runNormalizeSubstitution
                [ (RegVar Mock.x, Mock.constr10 (mkVar Mock.x)) ]
    , testCase "SetVariable Constructor cycle" $ do
        let var1 = Mock.makeTestSubstVar "@x"
        assertEqualWithExplanation "" (Substitution [(var1, mkBottom Mock.testSort)])
            =<< runNormalizeSubstitution
                [ (var1, Mock.constr10 (mkSubstVar var1)) ]
    , testCase "Constructor with side function cycle" $
        assertEqualWithExplanation "" SubstitutionBottom
            =<< runNormalizeSubstitution
                [(RegVar Mock.x, Mock.constr20 (Mock.f (mkVar Mock.x)) (mkVar Mock.x))]
    , testCase "Constructor with function cycle" $
        assertEqualWithExplanation ""
            (Error (NonCtorCircularVariableDependency [RegVar Mock.x]))
            =<< runNormalizeSubstitution
                [ (RegVar Mock.x, Mock.constr10 (Mock.f (mkVar Mock.x))) ]
    ]
  where
    v1 :: Sort -> Variable
    v1 = Variable (testId "v1") mempty
    x1 :: Sort -> Variable
    x1 = Variable (testId "x1") mempty
    f = Symbol
        { symbolConstructor = testId "f"
        , symbolParams = []
        , symbolAttributes = Default.def
        , symbolSorts = applicationSorts [Mock.testSort] Mock.testSort
        }

runNormalizeSubstitution
    :: [(SubstVar Variable, TermLike Variable)]
    -> IO NormalizationResult
runNormalizeSubstitution substitution = do
    normalizedSubstitution <- Test.SMT.runSMT
        $ evalSimplifier Mock.env
        $ Except.runExceptT
        $ normalizeSubstitution (Map.fromList substitution)
    case normalizedSubstitution of
        Left err -> return (Error err)
        Right predicate
          | isBottom predicate -> return SubstitutionBottom
          | otherwise -> return . Substitution
              . Substitution.unwrap . Conditional.substitution
              $ predicate
