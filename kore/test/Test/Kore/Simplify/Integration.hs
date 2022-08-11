module Test.Kore.Simplify.Integration (
    test_simplificationIntegration,
    test_simplificationIntegrationUnification,
    test_substituteMap,
    test_substituteList,
    test_substitute,
    test_simplifySideCondition,
) where

import Control.Lens qualified as Lens
import Data.Default qualified as Default
import Data.Generics.Product
import Data.Map.Strict qualified as Map
import Kore.Builtin.Bool qualified as Bool
import Kore.Builtin.Builtin qualified as Builtin
import Kore.Builtin.Int qualified as Int
import Kore.Builtin.List qualified as List
import Kore.Builtin.Map qualified as Map
import Kore.Builtin.Set qualified as Set
import Kore.Equation (
    Equation (..),
    mkEquation,
 )
import Kore.Equation qualified as Equation
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition (
    fromConditionWithReplacements,
    toRepresentation,
    top,
 )
import Kore.Internal.SideCondition.SideCondition qualified as SideCondition (
    Representation,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Rewrite.Axiom.EvaluationStrategy (
    builtinEvaluation,
    simplifierWithFallback,
 )
import Kore.Rewrite.Axiom.Identifier (AxiomIdentifier)
import Kore.Rewrite.Axiom.Identifier qualified as AxiomIdentifier (
    AxiomIdentifier (..),
 )
import Kore.Rewrite.Axiom.Registry (
    mkEvaluatorRegistry,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
    mkConfigVariable,
    mkEquationVariable,
    mkRuleVariable,
 )
import Kore.Rewrite.SMT.Declaration (
    declareSortsSymbols,
 )
import Kore.Simplify.Pattern qualified as Pattern (
    makeEvaluate,
 )
import Kore.Simplify.Simplify
import Kore.Unparser
import Prelude.Kore
import Test.Kore
import Test.Kore.Equation.Common (
    functionAxiomUnification,
    functionAxiomUnification_,
 )
import Test.Kore.Internal.OrPattern qualified as OrPattern
import Test.Kore.Internal.Pattern (
    Conditional (..),
 )
import Test.Kore.Internal.Pattern qualified as Pattern
import Test.Kore.Internal.Predicate as Predicate
import Test.Kore.Internal.Substitution as Substitution hiding (
    test_substitute,
 )
import Test.Kore.Rewrite.MockSymbols qualified as Mock
import Test.Kore.Simplify
import Test.SMT qualified as Test
import Test.Tasty
import Test.Tasty.HUnit.Ext

type SideCondition' = SideCondition RewritingVariableName

test_simplificationIntegration :: [TestTree]
test_simplificationIntegration =
    [ testCase "owise condition - main case" $ do
        let expect = OrPattern.fromPatterns []
        actual <-
            evaluate
                Conditional
                    { term =
                        -- Use the exact form we expect from an owise condition
                        -- for f(constr10(x)) = something
                        --     f(x) = something-else [owise]
                        mkAnd
                            ( mkNot
                                ( mkOr
                                    ( mkExists
                                        Mock.xConfig
                                        ( mkAnd
                                            (mkTop Mock.testSort)
                                            ( mkAnd
                                                ( (mkCeil Mock.testSort)
                                                    ( mkAnd
                                                        ( Mock.constr10
                                                            ( mkElemVar
                                                                Mock.xConfig
                                                            )
                                                        )
                                                        (Mock.constr10 Mock.a)
                                                    )
                                                )
                                                (mkTop Mock.testSort)
                                            )
                                        )
                                    )
                                    (mkBottom Mock.testSort)
                                )
                            )
                            (mkTop Mock.testSort)
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    , testCase "owise condition - owise case" $ do
        let expect = OrPattern.fromPatterns [Pattern.topOf Mock.testSort]
        actual <-
            evaluate
                Conditional
                    { term =
                        -- Use the exact form we expect from an owise condition
                        -- for f(constr10(x)) = something
                        --     f(x) = something-else [owise]
                        mkAnd
                            ( mkNot
                                ( mkOr
                                    ( mkExists
                                        Mock.xConfig
                                        ( mkAnd
                                            (mkTop Mock.testSort)
                                            ( mkAnd
                                                ( (mkCeil Mock.testSort)
                                                    ( mkAnd
                                                        ( Mock.constr10
                                                            ( mkElemVar
                                                                Mock.xConfig
                                                            )
                                                        )
                                                        (Mock.constr11 Mock.a)
                                                    )
                                                )
                                                (mkTop Mock.testSort)
                                            )
                                        )
                                    )
                                    (mkBottom Mock.testSort)
                                )
                            )
                            (mkTop Mock.testSort)
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    , testCase "map-like simplification" $ do
        let expects =
                OrPattern.fromPatterns
                    [ Conditional
                        { term = (mkTop Mock.testSort)
                        , predicate =
                            makeAndPredicate
                                ( makeAndPredicate
                                    (makeCeilPredicate Mock.cf)
                                    (makeCeilPredicate (Mock.plain10 Mock.cf))
                                )
                                ( makeAndPredicate
                                    ( makeCeilPredicate
                                        ( mkAnd
                                            (Mock.plain10 Mock.cf)
                                            (Mock.plain10 (mkElemVar Mock.xConfig))
                                        )
                                    )
                                    ( makeCeilPredicate
                                        (Mock.plain10 (mkElemVar Mock.xConfig))
                                    )
                                )
                        , substitution =
                            Substitution.unsafeWrap
                                [(inject Mock.yConfig, Mock.b)]
                        }
                    ]
        actuals <-
            evaluate
                Conditional
                    { term =
                        (mkCeil Mock.testSort)
                            ( mkAnd
                                ( Mock.constr20
                                    (Mock.plain10 Mock.cf)
                                    Mock.b
                                )
                                ( Mock.constr20
                                    (Mock.plain10 (mkElemVar Mock.xConfig))
                                    (mkElemVar Mock.yConfig)
                                )
                            )
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        Pattern.assertEquivalentPatterns expects actuals
    , testCase "map function, non-matching" $ do
        let initial =
                Pattern.fromTermLike $
                    Mock.function20MapTest (Mock.builtinMap []) Mock.a
            expect = OrPattern.fromPattern initial
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application
                            Mock.function20MapTestId
                        ,
                            [ mkEquation
                                ( Mock.function20MapTest
                                    ( Mock.concatMap
                                        ( Mock.elementMap
                                            (mkElemVar Mock.x)
                                            (mkElemVar Mock.y)
                                        )
                                        (mkElemVar Mock.m)
                                    )
                                    (mkElemVar Mock.x)
                                )
                                (mkElemVar Mock.y)
                                & Equation.mapVariables
                                    (pure mkRuleVariable)
                            ]
                        )
                    ]
                )
                initial
        assertEqual "" expect actual
    , testCase "function application with top predicate" $ do
        let requirement = \var ->
                makeEqualsPredicate
                    (Mock.f (mkElemVar var))
                    (Mock.g Mock.b)
            expect =
                OrPattern.fromTermLike $
                    Mock.functionalConstr11 $ Mock.g Mock.a
        actual <-
            evaluateConditionalWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application Mock.functionalConstr10Id
                        ,
                            [ axiom
                                (Mock.functionalConstr10 (mkElemVar Mock.xConfig))
                                (Mock.g Mock.a)
                                (requirement Mock.xConfig)
                            ]
                        )
                    ]
                )
                ( SideCondition.fromConditionWithReplacements
                    . from @(Predicate _)
                    $ requirement Mock.xConfig
                )
                ( Pattern.fromTermLike $
                    mkExists Mock.zConfig $
                        Mock.functionalConstr11 $
                            Mock.functionalConstr10 (mkElemVar Mock.xConfig)
                )
        assertEqual "" expect actual
    , testCase "no function branching" $ do
        let expect =
                OrPattern.fromPatterns
                    [ Conditional
                        { term = Mock.functional10 (mkElemVar Mock.xConfig)
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application Mock.functional10Id
                        ,
                            [ conditionalEqualityPattern
                                (Mock.functional10 (mkElemVar Mock.xConfig))
                                (makeEqualsPredicate Mock.cf Mock.a)
                                (mkElemVar Mock.xConfig)
                            , conditionalEqualityPattern
                                (Mock.functional10 (mkElemVar Mock.xConfig))
                                ( makeNotPredicate
                                    (makeEqualsPredicate Mock.cf Mock.a)
                                )
                                (mkElemVar Mock.xConfig)
                            ]
                        )
                    ]
                )
                Conditional
                    { term = Mock.functional10 (mkElemVar Mock.xConfig)
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    , testCase "exists variable equality" $ do
        let expect = OrPattern.topOf Mock.testSort
        actual <-
            evaluateWithAxioms
                Map.empty
                Conditional
                    { term =
                        mkExists
                            Mock.xConfig
                            ( (mkEquals Mock.testSort)
                                (mkElemVar Mock.xConfig)
                                (mkElemVar Mock.yConfig)
                            )
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    , testCase "exists variable equality reverse" $ do
        let expect = OrPattern.topOf Mock.testSort
        actual <-
            evaluateWithAxioms
                Map.empty
                Conditional
                    { term =
                        mkExists
                            Mock.xConfig
                            ( (mkEquals Mock.testSort)
                                (mkElemVar Mock.yConfig)
                                (mkElemVar Mock.xConfig)
                            )
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    , testCase "exists variable equality" $ do
        actual <-
            evaluateWithAxioms Map.empty $
                Pattern.fromTermLike $
                    mkExists Mock.xConfig $
                        (mkEquals Mock.testSort) (mkElemVar Mock.xConfig) (mkElemVar Mock.yConfig)
        assertEqual "" (OrPattern.topOf Mock.testSort) actual
    , testCase "exists variable equality reverse" $ do
        actual <-
            evaluateWithAxioms Map.empty $
                Pattern.fromTermLike $
                    mkExists Mock.xConfig $
                        (mkEquals Mock.testSort) (mkElemVar Mock.yConfig) (mkElemVar Mock.xConfig)
        assertEqual "" (OrPattern.topOf Mock.testSort) actual
    , testCase "simplification with top predicate (exists variable capture)" $
        do
            let requirement = \var ->
                    makeEqualsPredicate
                        (Mock.f (mkElemVar var))
                        (Mock.g Mock.b)
                expect =
                    OrPattern.fromPatterns
                        [ Conditional
                            { term =
                                mkExists
                                    Mock.var_xConfig_0
                                    (mkElemVar Mock.var_xConfig_0)
                            , predicate = requirement Mock.xConfig
                            , substitution = mempty
                            }
                        ]
            actual <-
                evaluateWithAxioms
                    ( Map.fromList
                        [
                            ( AxiomIdentifier.Application Mock.functionalConstr10Id
                            ,
                                [ axiom
                                    (Mock.functionalConstr10 (mkElemVar Mock.xConfig))
                                    (Mock.g Mock.a)
                                    (requirement Mock.xConfig)
                                ]
                            )
                        ]
                    )
                    Conditional
                        { term = mkExists Mock.xConfig (mkElemVar Mock.xConfig)
                        , predicate = requirement Mock.xConfig
                        , substitution = mempty
                        }
            assertEqual "" expect actual
    , testCase "simplification with top predicate (forall variable capture)" $
        do
            let requirement = \var ->
                    makeEqualsPredicate
                        (Mock.f (mkElemVar var))
                        (Mock.g Mock.b)
                expect =
                    OrPattern.fromPatterns
                        [ Conditional
                            { term =
                                mkForall
                                    Mock.var_xConfig_0
                                    (mkElemVar Mock.var_xConfig_0)
                            , predicate = requirement Mock.xConfig
                            , substitution = mempty
                            }
                        ]
            actual <-
                evaluateWithAxioms
                    ( Map.fromList
                        [
                            ( AxiomIdentifier.Application Mock.functionalConstr10Id
                            ,
                                [ axiom
                                    (Mock.functionalConstr10 (mkElemVar Mock.xConfig))
                                    (Mock.g Mock.a)
                                    (requirement Mock.xConfig)
                                ]
                            )
                        ]
                    )
                    Conditional
                        { term = mkForall Mock.xConfig (mkElemVar Mock.xConfig)
                        , predicate = requirement Mock.xConfig
                        , substitution = mempty
                        }
            assertEqual "" expect actual
    , testCase "simplification with top predicate (nu variable capture)" $ do
        let requirement = \var ->
                makeEqualsPredicate
                    (Mock.f (mkSetVar var))
                    (Mock.g Mock.b)
            expect =
                OrPattern.fromPatterns
                    [ Conditional
                        { term =
                            mkNu
                                Mock.var_setXConfig_0
                                (mkSetVar Mock.var_setXConfig_0)
                        , predicate = requirement Mock.setXConfig
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application Mock.functionalConstr10Id
                        ,
                            [ axiom
                                (Mock.functionalConstr10 (mkElemVar Mock.xConfig))
                                (Mock.g Mock.a)
                                (requirement Mock.setXConfig)
                            ]
                        )
                    ]
                )
                Conditional
                    { term = mkNu Mock.setXConfig (mkSetVar Mock.setXConfig)
                    , predicate = requirement Mock.setXConfig
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    , testCase "simplification with top predicate (mu variable capture)" $ do
        let requirement = \var ->
                makeEqualsPredicate
                    (Mock.f (mkSetVar var))
                    (Mock.g Mock.b)
            expect =
                OrPattern.fromPatterns
                    [ Conditional
                        { term =
                            mkMu
                                Mock.var_setXConfig_0
                                (mkSetVar Mock.var_setXConfig_0)
                        , predicate = requirement Mock.setXConfig
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application Mock.functionalConstr10Id
                        ,
                            [ axiom
                                (Mock.functionalConstr10 (mkElemVar Mock.xConfig))
                                (Mock.g Mock.a)
                                (requirement Mock.setXConfig)
                            ]
                        )
                    ]
                )
                Conditional
                    { term = mkMu Mock.setXConfig (mkSetVar Mock.setXConfig)
                    , predicate = requirement Mock.setXConfig
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    , testCase "Iff simplification" $ do
        let expected =
                OrPattern.fromPatterns
                    [ Conditional
                        { term = mkNot Mock.bSort0
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluate
                Conditional
                    { term = mkIff Mock.bSort0 (mkBottom Mock.testSort0)
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertEqual "" expected actual
    , testCase "Builtin and simplification failure" $ do
        let m =
                mkSetVariable (testId "m") Mock.listSort
                    & mapSetVariable (pure mkConfigVariable)
            ue =
                mkSetVariable (testId "ue") Mock.listSort
                    & mapSetVariable (pure mkConfigVariable)
        actual <-
            evaluate
                Conditional
                    { term =
                        mkAnd
                            ( Mock.concatList
                                ( mkImplies
                                    ( mkImplies
                                        (mkBottom Mock.listSort)
                                        (mkTop Mock.listSort)
                                    )
                                    ((mkIn Mock.listSort) Mock.cfSort0 Mock.cgSort0)
                                )
                                ( mkImplies
                                    ( mkAnd
                                        (mkMu m (mkBottom Mock.listSort))
                                        (mkBottom Mock.listSort)
                                    )
                                    (mkImplies Mock.unitList (mkNu ue Mock.unitList))
                                )
                            )
                            Mock.unitList
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }

        for_ (toList actual) $ \pattern' -> do
            let message = (show . unparse) pattern'
            let (term, condition) = Pattern.splitTerm pattern'
            assertBool "Expected simplified term" (TermLike.isSimplified sideRepresentation term)
            assertBool (unlines ["Expected simplified condition:", message]) (Condition.isSimplified sideRepresentation condition)
            assertBool message (Pattern.isSimplified sideRepresentation pattern')
    , testCase "Nu-And simplification" $ do
        let gt =
                mkSetVariable (testId "gt") Mock.testSort1
                    & mapSetVariable (pure mkConfigVariable)
            g =
                mkSetVariable (testId "g") Mock.testSort1
                    & mapSetVariable (pure mkConfigVariable)
        actual <-
            evaluate
                Conditional
                    { term =
                        mkNu
                            gt
                            ( mkAnd
                                ( mkAnd
                                    (mkTop Mock.testSort1)
                                    (mkNu g (mkOr Mock.aSort1 (mkSetVar g)))
                                )
                                (mkTop Mock.testSort1)
                            )
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertBool "" (OrPattern.isSimplified sideRepresentation actual)
    , testCase "And-list simplification" $ do
        actual <-
            evaluate
                Conditional
                    { term =
                        mkAnd
                            (Mock.elementList Mock.plain00)
                            (Mock.elementList Mock.functional00)
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertBool "" (OrPattern.isSimplified sideRepresentation actual)
    , testCase "Nu over distributed and simplification" $ do
        let k =
                mkSetVariable (testId "k") Mock.testSort
                    & mapSetVariable (pure mkConfigVariable)
        actual <-
            evaluate
                ( mkMu
                    k
                    ( mkAnd
                        (Mock.functionalConstr21 Mock.cf Mock.cf)
                        (Mock.functionalConstr21 Mock.ch Mock.cg)
                    )
                    & Pattern.fromTermLike
                )
        assertBool "" (OrPattern.isSimplified sideRepresentation actual)
    , testCase "nu-not-and-or simplification" $ do
        let q =
                mkSetVariable (testId "q") Mock.testSort
                    & mapSetVariable (pure mkConfigVariable)
        actual <-
            evaluate
                ( mkNu
                    q
                    ( mkNot
                        ( mkAnd
                            (Mock.g Mock.ch)
                            (mkOr Mock.cf Mock.cg)
                        )
                    )
                    & Pattern.fromTermLike
                )
        assertBool "" (OrPattern.isSimplified sideRepresentation actual)
    , testCase "Predicate simplifier simplifies child predicates" $ do
        actual <-
            evaluate
                ( makeFloorPredicate
                    ( mkIn
                        Mock.testSort
                        Mock.cf
                        Mock.cf
                    )
                    & Pattern.fromPredicateSorted Mock.testSort
                )
        assertBool "" (OrPattern.isSimplified sideRepresentation actual)
    , testCase "equals-predicate with sort change simplification" $ do
        actual <-
            evaluate
                Conditional
                    { term =
                        mkEquals
                            Mock.testSort
                            ( mkIn
                                Mock.subSort
                                (mkStringLiteral "a")
                                (mkStringLiteral "b")
                            )
                            (mkBottom Mock.subSort)
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertBool "" (OrPattern.isSimplified sideRepresentation actual)
    , testCase "Preserves predicate sort" $ do
        let patt =
                Conditional
                    { term = mkTop Mock.listSort
                    , predicate = makeInPredicate Mock.cf Mock.cg
                    , substitution = mempty
                    }
            expected =
                Conditional
                    { term = mkTop Mock.listSort
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate Mock.cf)
                            (makeEqualsPredicate Mock.cf Mock.cg)
                    , substitution = mempty
                    }
        actual <- evaluate patt
        assertEqual "" (OrPattern.fromPattern expected) actual
    , testCase "Simplify equals with variable" $ do
        let patt =
                Conditional
                    { term = mkTop Mock.testSort
                    , predicate = makeEqualsPredicate (Mock.functional10 (mkElemVar Mock.xConfig)) (Mock.functional10 (mkElemVar Mock.yConfig))
                    , substitution = mempty
                    }
            expected = OrPattern.topOf Mock.testSort
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Equals (AxiomIdentifier.Application Mock.functional10Id) AxiomIdentifier.Variable
                        ,
                            [ axiom
                                (mkEquals Mock.boolSort (Mock.functional10 (mkElemVar Mock.xRule)) (mkElemVar Mock.yRule))
                                (mkTop Mock.testSort)
                                makeTruePredicate
                            ]
                        )
                    ]
                )
                patt
        assertEqual "" expected actual
    , testCase "Simplify equals with dv" $ do
        let patt =
                Conditional
                    { term = mkTop Mock.testSort
                    , predicate = makeEqualsPredicate (Mock.fBool (mkElemVar Mock.xConfigBool)) (Bool.asInternal Mock.boolSort True)
                    , substitution = mempty
                    }
            expected = OrPattern.topOf Mock.testSort
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Equals (AxiomIdentifier.Application Mock.fBoolId) AxiomIdentifier.DV
                        ,
                            [ axiom
                                (mkEquals Mock.boolSort (Mock.fBool (mkElemVar Mock.xRuleBool)) (Bool.asInternal Mock.boolSort True))
                                (mkTop Mock.testSort)
                                makeTruePredicate
                            ]
                        )
                    ]
                )
                patt
        assertEqual "" expected actual
    ]

test_simplificationIntegrationUnification :: [TestTree]
test_simplificationIntegrationUnification =
    [ testCase "map function, non-matching" $ do
        let initial =
                Pattern.fromTermLike $
                    Mock.function20MapTest (Mock.builtinMap []) Mock.a
            expect = OrPattern.fromPattern initial
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application
                            Mock.function20MapTestId
                        ,
                            [ functionAxiomUnification_
                                Mock.function20MapTestSymbol
                                [ Mock.concatMap
                                    ( Mock.elementMap
                                        (mkElemVar Mock.xConfig)
                                        (mkElemVar Mock.yConfig)
                                    )
                                    (mkElemVar Mock.mConfig)
                                , mkElemVar Mock.xConfig
                                ]
                                (mkElemVar Mock.yConfig)
                            ]
                        )
                    ]
                )
                initial
        assertEqual "" expect actual
    , testCase "TESTING function application with top predicate" $ do
        let requirement = \var ->
                makeEqualsPredicate
                    (Mock.f (mkElemVar var))
                    (Mock.g Mock.b)
            expect =
                OrPattern.fromTermLike $
                    Mock.functionalConstr11 $ Mock.g Mock.a
        actual <-
            evaluateConditionalWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application Mock.functionalConstr10Id
                        ,
                            [ functionAxiomUnification
                                Mock.functionalConstr10Symbol
                                [mkElemVar Mock.xConfig]
                                (Mock.g Mock.a)
                                (requirement Mock.xConfig)
                            ]
                        )
                    ]
                )
                ( SideCondition.fromConditionWithReplacements
                    . from @(Predicate _)
                    $ requirement Mock.xConfig
                )
                ( Pattern.fromTermLike $
                    mkExists Mock.zConfig $
                        Mock.functionalConstr11 $
                            Mock.functionalConstr10 (mkElemVar Mock.xConfig)
                )
        assertEqual "" expect actual
    , testCase "no function branching" $ do
        let expect =
                OrPattern.fromPatterns
                    [ Conditional
                        { term = Mock.functional10 (mkElemVar Mock.xConfig)
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application Mock.functional10Id
                        ,
                            [ functionAxiomUnification
                                Mock.functional10Symbol
                                [mkElemVar Mock.xConfig]
                                (mkElemVar Mock.xConfig)
                                (makeEqualsPredicate Mock.cf Mock.a)
                            , functionAxiomUnification
                                Mock.functional10Symbol
                                [mkElemVar Mock.xConfig]
                                (mkElemVar Mock.xConfig)
                                ( makeNotPredicate
                                    (makeEqualsPredicate Mock.cf Mock.a)
                                )
                            ]
                        )
                    ]
                )
                Conditional
                    { term = Mock.functional10 (mkElemVar Mock.xConfig)
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    , testCase "simplification with top predicate (exists variable capture)" $
        do
            let requirement = \var ->
                    makeEqualsPredicate
                        (Mock.f (mkElemVar var))
                        (Mock.g Mock.b)
                expect =
                    OrPattern.fromPatterns
                        [ Conditional
                            { term =
                                mkExists
                                    Mock.var_xConfig_0
                                    (mkElemVar Mock.var_xConfig_0)
                            , predicate = requirement Mock.xConfig
                            , substitution = mempty
                            }
                        ]
            actual <-
                evaluateWithAxioms
                    ( Map.fromList
                        [
                            ( AxiomIdentifier.Application Mock.functionalConstr10Id
                            ,
                                [ functionAxiomUnification
                                    Mock.functionalConstr10Symbol
                                    [mkElemVar Mock.xConfig]
                                    (Mock.g Mock.a)
                                    (requirement Mock.xConfig)
                                ]
                            )
                        ]
                    )
                    Conditional
                        { term = mkExists Mock.xConfig (mkElemVar Mock.xConfig)
                        , predicate = requirement Mock.xConfig
                        , substitution = mempty
                        }
            assertEqual "" expect actual
    , testCase "simplification with top predicate (forall variable capture)" $
        do
            let requirement = \var ->
                    makeEqualsPredicate
                        (Mock.f (mkElemVar var))
                        (Mock.g Mock.b)
                expect =
                    OrPattern.fromPatterns
                        [ Conditional
                            { term =
                                mkForall
                                    Mock.var_xConfig_0
                                    (mkElemVar Mock.var_xConfig_0)
                            , predicate = requirement Mock.xConfig
                            , substitution = mempty
                            }
                        ]
            actual <-
                evaluateWithAxioms
                    ( Map.fromList
                        [
                            ( AxiomIdentifier.Application Mock.functionalConstr10Id
                            ,
                                [ functionAxiomUnification
                                    Mock.functionalConstr10Symbol
                                    [mkElemVar Mock.xConfig]
                                    (Mock.g Mock.a)
                                    (requirement Mock.xConfig)
                                ]
                            )
                        ]
                    )
                    Conditional
                        { term = mkForall Mock.xConfig (mkElemVar Mock.xConfig)
                        , predicate = requirement Mock.xConfig
                        , substitution = mempty
                        }
            assertEqual "" expect actual
    , testCase "simplification with top predicate (nu variable capture)" $ do
        let requirement = \var ->
                makeEqualsPredicate
                    (Mock.f (mkSetVar var))
                    (Mock.g Mock.b)
            expect =
                OrPattern.fromPatterns
                    [ Conditional
                        { term =
                            mkNu
                                Mock.var_setXConfig_0
                                (mkSetVar Mock.var_setXConfig_0)
                        , predicate = requirement Mock.setXConfig
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application Mock.functionalConstr10Id
                        ,
                            [ functionAxiomUnification
                                Mock.functionalConstr10Symbol
                                [mkElemVar Mock.xConfig]
                                (Mock.g Mock.a)
                                (requirement Mock.setXConfig)
                            ]
                        )
                    ]
                )
                Conditional
                    { term = mkNu Mock.setXConfig (mkSetVar Mock.setXConfig)
                    , predicate = requirement Mock.setXConfig
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    , testCase "simplification with top predicate (mu variable capture)" $ do
        let requirement = \var ->
                makeEqualsPredicate
                    (Mock.f (mkSetVar var))
                    (Mock.g Mock.b)
            expect =
                OrPattern.fromPatterns
                    [ Conditional
                        { term =
                            mkMu
                                Mock.var_setXConfig_0
                                (mkSetVar Mock.var_setXConfig_0)
                        , predicate = requirement Mock.setXConfig
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluateWithAxioms
                ( Map.fromList
                    [
                        ( AxiomIdentifier.Application Mock.functionalConstr10Id
                        ,
                            [ functionAxiomUnification
                                Mock.functionalConstr10Symbol
                                [mkElemVar Mock.xConfig]
                                (Mock.g Mock.a)
                                (requirement Mock.setXConfig)
                            ]
                        )
                    ]
                )
                Conditional
                    { term = mkMu Mock.setXConfig (mkSetVar Mock.setXConfig)
                    , predicate = requirement Mock.setXConfig
                    , substitution = mempty
                    }
        assertEqual "" expect actual
    ]

conditionalEqualityPattern ::
    TermLike RewritingVariableName ->
    Predicate.Predicate RewritingVariableName ->
    TermLike RewritingVariableName ->
    Equation RewritingVariableName
conditionalEqualityPattern left requires right =
    mkEquation left right
        & Lens.set (field @"requires") requires

test_substitute :: [TestTree]
test_substitute =
    [ testCase "Substitution under unary functional constructor" $ do
        let expect =
                OrPattern.fromPatterns
                    [ Pattern.Conditional
                        { term =
                            Mock.functionalConstr20
                                Mock.a
                                (Mock.functionalConstr10 Mock.a)
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [ (inject Mock.xConfig, Mock.a)
                                ,
                                    ( inject Mock.yConfig
                                    , Mock.functionalConstr10 Mock.a
                                    )
                                ]
                        }
                    ]
        actual <-
            evaluate
                ( Pattern.fromTermLike
                    ( mkAnd
                        ( Mock.functionalConstr20
                            (mkElemVar Mock.xConfig)
                            (Mock.functionalConstr10 (mkElemVar Mock.xConfig))
                        )
                        (Mock.functionalConstr20 Mock.a (mkElemVar Mock.yConfig))
                    )
                )
        assertEqual
            "Expected substitution under unary functional constructor"
            expect
            actual
    , testCase "Substitution" $ do
        let expect =
                OrPattern.fromPatterns
                    [ Pattern.Conditional
                        { term =
                            Mock.functionalConstr20 Mock.a Mock.a
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [ (inject Mock.xConfig, Mock.a)
                                , (inject Mock.yConfig, Mock.a)
                                ]
                        }
                    ]
        actual <-
            evaluate
                ( Pattern.fromTermLike
                    ( mkAnd
                        ( Mock.functionalConstr20
                            (mkElemVar Mock.xConfig)
                            (mkElemVar Mock.xConfig)
                        )
                        (Mock.functionalConstr20 Mock.a (mkElemVar Mock.yConfig))
                    )
                )
        assertEqual "Expected substitution" expect actual
    ]

test_substituteMap :: [TestTree]
test_substituteMap =
    [ testCase "Substitution applied to Map elements" $ do
        let testMapX =
                Mock.sortInjection Mock.testSort $
                    mkDomainBuiltinMap [(Mock.a, mkElemVar Mock.xConfig)]
            testMapA =
                Mock.sortInjection Mock.testSort $
                    mkDomainBuiltinMap [(Mock.a, Mock.a)]
            expect =
                OrPattern.fromPatterns
                    [ Pattern.Conditional
                        { term = Mock.functionalConstr20 Mock.a testMapA
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [ (inject Mock.xConfig, Mock.a)
                                , (inject Mock.yConfig, testMapA)
                                ]
                        }
                    ]
        actual <-
            (evaluate . Pattern.fromTermLike)
                ( mkAnd
                    (Mock.functionalConstr20 (mkElemVar Mock.xConfig) testMapX)
                    (Mock.functionalConstr20 Mock.a (mkElemVar Mock.yConfig))
                )
        assertEqual
            "Expected substitution applied to Map elements"
            expect
            actual
    ]
  where
    mkDomainBuiltinMap = Mock.builtinMap

test_substituteList :: [TestTree]
test_substituteList =
    [ testCase "Substitution applied to List elements" $ do
        let testListX =
                Mock.sortInjection Mock.testSort $
                    mkDomainBuiltinList [Mock.a, mkElemVar Mock.xConfig]
            testListA =
                Mock.sortInjection Mock.testSort $
                    mkDomainBuiltinList [Mock.a, Mock.a]
            expect =
                OrPattern.fromPatterns
                    [ Pattern.Conditional
                        { term = Mock.functionalConstr20 Mock.a testListA
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [ (inject Mock.xConfig, Mock.a)
                                , (inject Mock.yConfig, testListA)
                                ]
                        }
                    ]
        actual <-
            (evaluate . Pattern.fromTermLike)
                ( mkAnd
                    (Mock.functionalConstr20 (mkElemVar Mock.xConfig) testListX)
                    (Mock.functionalConstr20 Mock.a (mkElemVar Mock.yConfig))
                )
        assertEqual
            "Expected substitution applied to List elements"
            expect
            actual
    ]
  where
    mkDomainBuiltinList = Mock.builtinList

test_simplifySideCondition :: [TestTree]
test_simplifySideCondition =
    [ testCase "Simplifies function application in side condition" $ do
        let configuration =
                Pattern.fromTermAndPredicate
                    Mock.a
                    ( makeAndPredicate
                        ( makeEqualsPredicate
                            (Mock.f Mock.a)
                            Mock.b
                        )
                        ( makeEqualsPredicate
                            (Mock.g Mock.a)
                            (Mock.g Mock.b)
                        )
                    )
            expected =
                Pattern.fromTermAndPredicate
                    Mock.a
                    ( makeEqualsPredicate
                        (Mock.g Mock.a)
                        (Mock.g Mock.b)
                    )
                    & OrPattern.fromPattern
            axioms =
                Map.fromList
                    [
                        ( AxiomIdentifier.Application Mock.fId
                        ,
                            [ functionAxiomUnification
                                Mock.fSymbol
                                [Mock.a]
                                Mock.b
                                ( makeEqualsPredicate
                                    (Mock.g Mock.a)
                                    (Mock.g Mock.b)
                                )
                            ]
                        )
                    ]

        actual <- evaluateWithAxioms axioms configuration
        assertEqual "" expected actual
    ]

evaluate ::
    Pattern.Pattern RewritingVariableName ->
    IO (OrPattern.OrPattern RewritingVariableName)
evaluate = evaluateWithAxioms Map.empty

evaluateWithAxioms ::
    Map.Map AxiomIdentifier.AxiomIdentifier [Equation RewritingVariableName] ->
    Pattern.Pattern RewritingVariableName ->
    IO (OrPattern.OrPattern RewritingVariableName)
evaluateWithAxioms axioms =
    evaluateConditionalWithAxioms axioms SideCondition.top

evaluateConditionalWithAxioms ::
    Map.Map AxiomIdentifier.AxiomIdentifier [Equation RewritingVariableName] ->
    SideCondition' ->
    Pattern.Pattern RewritingVariableName ->
    IO (OrPattern.OrPattern RewritingVariableName)
evaluateConditionalWithAxioms axioms sideCondition patt =
    Test.runSMT userInit $ do
        newEnv <- runSimplifier Mock.env initialize
        runSimplifier newEnv . Pattern.makeEvaluate sideCondition $ patt
  where
    userInit = declareSortsSymbols Mock.smtDeclarations
    initialize :: Simplifier Env
    initialize = do
        processedEquations <- Equation.simplifyExtractedEquations axioms
        let simplifierAxioms :: BuiltinAndAxiomSimplifierMap
            simplifierAxioms =
                Map.unionWith
                    simplifierWithFallback
                    builtinAxioms
                    (mkEvaluatorRegistry processedEquations)
        return Mock.env{simplifierAxioms}

-- | A selection of builtin axioms used in the tests above.
builtinAxioms :: BuiltinAndAxiomSimplifierMap
builtinAxioms =
    Map.fromList
        [
            ( AxiomIdentifier.Application Mock.concatMapId
            , Builtin.functionEvaluator Map.evalConcat
            )
        ,
            ( AxiomIdentifier.Application Mock.elementMapId
            , Builtin.functionEvaluator Map.evalElement
            )
        ,
            ( AxiomIdentifier.Application Mock.unitMapId
            , Builtin.functionEvaluator Map.evalUnit
            )
        ,
            ( AxiomIdentifier.Application Mock.concatSetId
            , Builtin.functionEvaluator Set.evalConcat
            )
        ,
            ( AxiomIdentifier.Application Mock.concatSetId
            , Builtin.functionEvaluator Set.evalConcat
            )
        ,
            ( AxiomIdentifier.Application Mock.elementSetId
            , Builtin.functionEvaluator Set.evalElement
            )
        ,
            ( AxiomIdentifier.Application Mock.unitSetId
            , Builtin.functionEvaluator Set.evalUnit
            )
        ,
            ( AxiomIdentifier.Application Mock.concatListId
            , Builtin.functionEvaluator List.evalConcat
            )
        ,
            ( AxiomIdentifier.Application Mock.elementListId
            , Builtin.functionEvaluator List.evalElement
            )
        ,
            ( AxiomIdentifier.Application Mock.unitListId
            , Builtin.functionEvaluator List.evalUnit
            )
        ,
            ( AxiomIdentifier.Application Mock.concatListId
            , Builtin.functionEvaluator List.evalConcat
            )
        ,
            ( AxiomIdentifier.Application Mock.tdivIntId
            , builtinEvaluation (Int.builtinFunctions Map.! Int.tdivKey)
            )
        ]

axiom ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Predicate RewritingVariableName ->
    Equation RewritingVariableName
axiom left right requires =
    Equation
        { left
        , requires
        , argument = Nothing
        , antiLeft = Nothing
        , right
        , ensures = Predicate.makeTruePredicate
        , attributes = Default.def
        }

sideRepresentation :: SideCondition.Representation
sideRepresentation =
    SideCondition.toRepresentation (SideCondition.top :: SideCondition')
