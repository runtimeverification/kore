module Test.Kore.Step.Axiom.EvaluationStrategy
    ( test_definitionEvaluation
    , test_firstFullEvaluation
    , test_simplifierWithFallback
    , test_builtinEvaluation
    , test_attemptEquations
    ) where

import Prelude.Kore

import Test.Tasty

import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    )

import qualified Kore.Equation as Equation
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern as Pattern
    ( Conditional (Conditional)
    )
import qualified Kore.Internal.Pattern as Pattern
    ( Conditional (..)
    )
import Kore.Internal.Predicate
    ( Predicate
    , makeEqualsPredicate
    , makeEqualsPredicate
    , makeNotPredicate
    , makeTruePredicate
    , makeTruePredicate
    )
import qualified Kore.Internal.SideCondition as SideCondition
    ( assumeTruePredicate
    )
import Kore.Internal.TermLike
import Kore.Rewriting.RewritingVariable
    ( mkRuleVariable
    )
import Kore.Step.Axiom.EvaluationStrategy
import Kore.Step.Simplification.Simplify
import qualified Kore.Step.Simplification.Simplify as AttemptedAxiom
    ( AttemptedAxiom (..)
    )

import Test.Kore.Equation.Common
    ( axiom
    , axiom'
    , axiom_
    , concrete
    )
import qualified Test.Kore.Step.MockSymbols as Mock
import Test.Kore.Step.Simplification
import Test.Tasty.HUnit.Ext

test_attemptEquations :: [TestTree]
test_attemptEquations =
    [ testCase "Stops attempting equations at first successful result" $ do
        counter <- newIORef (0 :: Int)
        let condition =
                SideCondition.assumeTruePredicate makeTruePredicate
            term = Mock.functionalConstr10 Mock.a
            equations =
                [ notApplicable1, applicable, notApplicable2, applicable
                ]
        _ <-
            attemptEquations
                (attemptEquationAndAccumulateErrors' counter condition term)
                equations
            & runSimplifier Mock.env
        updatedCounter <- readIORef counter
        assertEqual "" 2 updatedCounter
    ]
  where
    attemptEquationAndAccumulateErrors' counter condition term equation = do
        liftIO $ modifyIORef' counter (+ 1)
        attemptEquationAndAccumulateErrors condition term equation
    applicable =
        axiom
          (Mock.functionalConstr10 (mkElemVar Mock.x))
          Mock.a
          (makeEqualsPredicate (mkElemVar Mock.x) Mock.a)
    notApplicable1 =
        axiom
          (Mock.functionalConstr10 (mkElemVar Mock.x))
          Mock.c
          (makeEqualsPredicate (mkElemVar Mock.x) Mock.c)
    notApplicable2 =
        axiom
          (Mock.functionalConstr10 (mkElemVar Mock.x))
          Mock.b
          (makeEqualsPredicate (mkElemVar Mock.x) Mock.b)

test_definitionEvaluation :: [TestTree]
test_definitionEvaluation =
    [ testCase "Simple evaluation" $ do
        let expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.g Mock.c
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        actual <-
            evaluate
                (definitionEvaluation
                    [ axiom'
                        (Mock.functionalConstr10 (mkElemVar Mock.x))
                        (Mock.g (mkElemVar Mock.x))
                        makeTruePredicate
                    ]
                )
                (Mock.functionalConstr10 Mock.c)
        assertEqual "" expect actual
    {-
    Uncomment this if we ever go back to having remainders for functions.

    , testCase "Evaluation with remainder" $ do
        let requirement = makeEqualsPredicate
                (Mock.f Mock.a)
                (Mock.g Mock.b)
            expect =
                AttemptedAxiom.Applied AttemptedAxiomResults
                    { results =
                        OrPattern.fromPattern Conditional
                            { term = Mock.g Mock.a
                            , predicate = requirement
                            , substitution = mempty
                            }
                    , remainders =
                        OrPattern.fromPatterns
                        $ map (fmap mkEvaluated)
                            [ Conditional
                                { term = Mock.functionalConstr10 Mock.a
                                , predicate = makeNotPredicate requirement
                                , substitution = mempty
                                }
                            ]
                    }
        actual <-
            evaluate
                (definitionEvaluation
                    [ axiom
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                        requirement
                    ]
                )
                (Mock.functionalConstr10 Mock.a)
        assertEqual "" expect actual
    -}
    , testCase "Failed evaluation" $ do
        let expect = AttemptedAxiom.NotApplicable
        actual <-
            evaluate
                (definitionEvaluation
                    [ axiom'
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                        makeTruePredicate
                    ]
                )
                (Mock.functionalConstr10 Mock.b)
        assertEqual "" expect actual
    {-
    Uncomment this if we ever go back to having multiple evaluation branches
    for functions.

    , testCase "Evaluation with multiple branches SMT prunes remainders" $ do
        let initial = Mock.functionalConstr10 Mock.a
            final1 = Mock.g Mock.a
            final2 = Mock.g Mock.b
            requirement1 = makeEqualsPredicate
                (Mock.f Mock.a)
                (Mock.g Mock.b)
            requirement2 = makeNotPredicate requirement1
            axiom1 = axiom initial final1 requirement1
            axiom2 = axiom initial final2 requirement2
            evaluator = definitionEvaluation [axiom1, axiom2]
            expect =
                AttemptedAxiom.Applied AttemptedAxiomResults
                    { results = OrPattern.fromPatterns
                        [ Conditional
                            { term = final1
                            , predicate = requirement1
                            , substitution = mempty
                            }
                        , Conditional
                            { term = final2
                            , predicate = requirement2
                            , substitution = mempty
                            }
                        ]
                    , remainders = OrPattern.bottom
                    }
        actual <- evaluate evaluator initial
        assertEqual "" expect actual
    -}
    , testCase "Does not evaluate concrete axiom with symbolic input" $ do
        let expectConcrete =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromTermLike
                            (Mock.g Mock.c)
                        , remainders = OrPattern.fromPatterns []
                        }

            symbolicTerm = Mock.functionalConstr10 (mkElemVar Mock.y)
            expectSymbolic = AttemptedAxiom.NotApplicable

            evaluator = definitionEvaluation
                [ axiom_
                    (Mock.functionalConstr10 (mkElemVar Mock.x))
                    (Mock.g (mkElemVar Mock.x))
                    & concrete [mkElemVar Mock.x]
                    & Equation.mapVariables (pure mkRuleVariable)
                ]

        actualConcrete <- evaluate evaluator (Mock.functionalConstr10 Mock.c)
        assertEqual "" expectConcrete actualConcrete

        actualSymbolic <- evaluate evaluator symbolicTerm
        assertEqual "" expectSymbolic actualSymbolic
    ]

test_firstFullEvaluation :: [TestTree]
test_firstFullEvaluation =
    [ testCase "Simple evaluation" $ do
        let expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.g Mock.c
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        actual <-
            evaluate
                (firstFullEvaluation
                    [ axiomEvaluator
                        (Mock.functionalConstr10 (mkElemVar Mock.x))
                        (Mock.g (mkElemVar Mock.x))
                    ]
                )
                (Mock.functionalConstr10 Mock.c)
        assertEqual "" expect actual
    , testCase "Uses first matching" $ do
        let expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.f Mock.a
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        actual <-
            evaluate
                (firstFullEvaluation
                    [ axiomEvaluator
                        (Mock.functionalConstr10 Mock.b)
                        (Mock.g Mock.a)
                    , axiomEvaluator
                        (Mock.functionalConstr10 (mkElemVar Mock.x))
                        (Mock.f Mock.a)
                    , axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                    ]
                )
                (Mock.functionalConstr10 Mock.a)
        assertEqual "" expect actual
    , testCase "Skips partial matches" $ do
        let expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.f Mock.a
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        actual <-
            evaluate
                (firstFullEvaluation
                    [ axiomEvaluator
                        (Mock.functionalConstr10 Mock.b)
                        (Mock.g Mock.a)
                    , axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                    , axiomEvaluator
                        (Mock.functionalConstr10 (mkElemVar Mock.x))
                        (Mock.f Mock.a)
                    ]
                )
                (Mock.functionalConstr10 (mkElemVar Mock.x))
        assertEqual "" expect actual
    , testCase "None matching" $ do
        let
            expect = AttemptedAxiom.NotApplicable
        actual <-
            evaluate
                (firstFullEvaluation
                    [ axiomEvaluator
                        (Mock.functionalConstr10 Mock.b)
                        (Mock.g Mock.a)
                    , axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                    ]
                )
                (Mock.functionalConstr10 (mkElemVar Mock.x))
        assertEqual "" expect actual
    , testCase "Skip when remainder" $ do
        let expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.g Mock.b
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        let requirement = makeEqualsPredicate (Mock.f Mock.a) (Mock.g Mock.b)
        actual <-
            evaluate
                (firstFullEvaluation
                    [ definitionEvaluation
                        [ axiom'
                            (Mock.functionalConstr10 Mock.a)
                            (Mock.g Mock.a)
                            requirement
                        ]
                    , axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.b)
                    ]
                )
                (Mock.functionalConstr10 Mock.a)
        assertEqual "" expect actual
    , testCase "Apply with top configuration" $ do
        let requirement = makeEqualsPredicate
                (Mock.f Mock.a)
                (Mock.g Mock.b)
        let expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.g Mock.a
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        actual <-
            evaluateWithPredicate
                (firstFullEvaluation
                    [ axiomEvaluatorWithRequires
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                        requirement
                    , axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.b)
                    ]
                )
                (Mock.functionalConstr10 Mock.a)
                requirement
        assertEqual "" expect actual
    , testCase "Don't apply due to top configuration" $ do
        let requirement = makeEqualsPredicate (Mock.f Mock.a) (Mock.g Mock.b)
        let not_requirement = makeNotPredicate requirement
        let expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.g Mock.b
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        actual <-
            evaluateWithPredicate
                (firstFullEvaluation
                    [ axiomEvaluatorWithRequires
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                        requirement
                    , axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.b)
                    ]
                )
                (Mock.functionalConstr10 Mock.a)
                not_requirement
        assertEqual "" expect actual
    {-
    Uncomment this if we ever go back to allowing multiple results for equality
    simplification.

    , testCase "Error with multiple results" $ do
        let requirement = makeEqualsPredicate (Mock.f Mock.a) (Mock.g Mock.b)
        assertErrorIO
            (assertSubstring ""
                (  "Unexpected simplification result with more than one "
                ++ "configuration"
                )
            )
            (evaluate
                (firstFullEvaluation
                    [ definitionEvaluation
                        [ axiom
                            (Mock.functionalConstr10 Mock.a)
                            (Mock.g Mock.a)
                            requirement
                        , axiom
                            (Mock.functionalConstr10 Mock.a)
                            (Mock.g Mock.b)
                            (makeNotPredicate requirement)
                        ]
                    ]
                )
                (Mock.functionalConstr10 Mock.a)
            )
    -}
    ]

test_simplifierWithFallback :: [TestTree]
test_simplifierWithFallback =
    [ testCase "Uses first" $ do
        let expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.g Mock.a
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        actual <-
            evaluate
                (simplifierWithFallback
                    (axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                    )
                    (axiomEvaluator
                        (Mock.functionalConstr10 (mkElemVar Mock.x))
                        (Mock.f Mock.a)
                    )
                )
                (Mock.functionalConstr10 Mock.a)
        assertEqual "" expect actual
    {-
    Uncomment this if we ever go back to having remainders for equality axioms.

    , testCase "Uses first with remainder" $ do
        let requirement = makeEqualsPredicate
                (Mock.f Mock.a)
                (Mock.g Mock.b)
            expect =
                AttemptedAxiom.Applied AttemptedAxiomResults
                    { results = OrPattern.fromPatterns
                        [ Conditional
                            { term = Mock.g Mock.a
                            , predicate = requirement
                            , substitution = mempty
                            }
                        ]
                    , remainders =
                        OrPattern.fromPatterns $ (map . fmap) mkEvaluated
                            [ Conditional
                                { term = Mock.functionalConstr10 Mock.a
                                , predicate = makeNotPredicate requirement
                                , substitution = mempty
                                }
                            ]
                    }
        actual <-
            evaluate
                (simplifierWithFallback
                    (definitionEvaluation
                        [ axiom
                            (Mock.functionalConstr10 Mock.a)
                            (Mock.g Mock.a)
                            requirement
                        ]
                    )
                    (definitionEvaluation
                        [ axiom
                            (Mock.functionalConstr10 Mock.a)
                            (Mock.f Mock.a)
                            (makeNotPredicate requirement)
                        ]
                    )
                )
                (Mock.functionalConstr10 Mock.a)
        assertEqual "" expect actual
    -}
    , testCase "Falls back to second" $ do
        let expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.f Mock.a
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        actual <-
            evaluate
                (simplifierWithFallback
                    (axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                    )
                    (axiomEvaluator
                        (Mock.functionalConstr10 (mkElemVar Mock.x))
                        (Mock.f Mock.a)
                    )
                )
                (Mock.functionalConstr10 Mock.b)
        assertEqual "" expect actual
    , testCase "None works" $ do
        let
            expect = AttemptedAxiom.NotApplicable
        actual <-
            evaluate
                (simplifierWithFallback
                    (axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                    )
                    (axiomEvaluator
                        (Mock.functionalConstr10 Mock.b)
                        (Mock.f Mock.a)
                    )
                )
                (Mock.functionalConstr10 Mock.c)
        assertEqual "" expect actual
    ]

test_builtinEvaluation :: [TestTree]
test_builtinEvaluation =
    [ testCase "Simple evaluation" $ do
        let
            expect =
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = OrPattern.fromPatterns
                            [ Conditional
                                { term = Mock.g Mock.a
                                , predicate = makeTruePredicate
                                , substitution = mempty
                                }
                            ]
                        , remainders = OrPattern.fromPatterns []
                        }
        actual <-
            evaluate
                (builtinEvaluation
                    (axiomEvaluator
                        (Mock.functionalConstr10 Mock.a)
                        (Mock.g Mock.a)
                    )
                )
                (Mock.functionalConstr10 Mock.a)
        assertEqual "" expect actual
    , testCase "Failed evaluation"
        (assertErrorIO
            (assertSubstring ""
                "Expecting hook 'MAP.unit' to reduce concrete pattern"
            )
            (evaluate
                (builtinEvaluation failingEvaluator)
                Mock.unitMap
            )
        )
    ]

failingEvaluator :: BuiltinAndAxiomSimplifier
failingEvaluator =
    BuiltinAndAxiomSimplifier $ \_ _ ->
        return AttemptedAxiom.NotApplicable

axiomEvaluatorWithRequires
    :: TermLike VariableName
    -> TermLike VariableName
    -> Predicate VariableName
    -> BuiltinAndAxiomSimplifier
axiomEvaluatorWithRequires left right requires =
    simplificationEvaluation (axiom' left right requires)

axiomEvaluator
    :: TermLike VariableName
    -> TermLike VariableName
    -> BuiltinAndAxiomSimplifier
axiomEvaluator left right =
    simplificationEvaluation (axiom' left right makeTruePredicate)

evaluate
    :: BuiltinAndAxiomSimplifier
    -> TermLike VariableName
    -> IO CommonAttemptedAxiom
evaluate simplifier term =
    evaluateWithPredicate simplifier term makeTruePredicate

evaluateWithPredicate
    :: BuiltinAndAxiomSimplifier
    -> TermLike VariableName
    -> Predicate VariableName
    -> IO CommonAttemptedAxiom
evaluateWithPredicate (BuiltinAndAxiomSimplifier simplifier) term predicate =
    runSimplifierSMT Mock.env
    $ simplifier term (SideCondition.assumeTruePredicate predicate)
