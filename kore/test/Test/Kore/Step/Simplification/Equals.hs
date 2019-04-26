module Test.Kore.Step.Simplification.Equals
    ( test_equalsSimplification_ExpandedPatterns
    , test_equalsSimplification_OrOfExpandedPatterns
    , test_equalsSimplification_Patterns
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Foldable as Foldable
import qualified Data.Map as Map

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.Attribute.Symbol
                 ( StepperAttributes )
import qualified Kore.Domain.Builtin as Domain
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import           Kore.Predicate.Predicate
                 ( pattern PredicateFalse, makeAndPredicate, makeCeilPredicate,
                 makeEqualsPredicate, makeIffPredicate, makeImpliesPredicate,
                 makeMultipleAndPredicate, makeNotPredicate, makeOrPredicate,
                 makeTruePredicate )
import           Kore.Step.Pattern
                 ( CommonExpandedPattern )
import qualified Kore.Step.Pattern as Conditional
import qualified Kore.Step.Representation.MultiOr as MultiOr
import           Kore.Step.Representation.OrOfExpandedPattern
                 ( CommonOrOfExpandedPattern, CommonOrOfPredicateSubstitution )
import qualified Kore.Step.Representation.OrOfExpandedPattern as OrOfExpandedPattern
import           Kore.Step.Representation.PredicateSubstitution
                 ( CommonPredicateSubstitution, Conditional (..) )
import qualified Kore.Step.Representation.PredicateSubstitution as PredicateSubstitution
import           Kore.Step.Simplification.Data
                 ( evalSimplifier )
import           Kore.Step.Simplification.Equals
                 ( makeEvaluate, makeEvaluateTermsToPredicateSubstitution,
                 simplify )
import qualified Kore.Step.Simplification.Simplifier as Simplifier
                 ( create )
import           Kore.Step.TermLike
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unparser
import qualified SMT

import           Test.Kore
import           Test.Kore.Comparators ()
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock
                 ( makeMetadataTools )
import qualified Test.Kore.Step.MockSimplifiers as Mock
import qualified Test.Kore.Step.MockSymbols as Mock
import           Test.Tasty.HUnit.Extensions

test_equalsSimplification_OrOfExpandedPatterns :: [TestTree]
test_equalsSimplification_OrOfExpandedPatterns =
    [ testCase "bottom == bottom" $ do
        let expect = MultiOr.make [ Conditional.top ]
        actual <-
            evaluateOr
                mockMetadataTools
                Equals
                    { equalsOperandSort = testSort
                    , equalsResultSort = testSort
                    , equalsFirst = MultiOr.make []
                    , equalsSecond = MultiOr.make []
                    }
        assertEqualWithExplanation "" expect actual

    , testCase "a == a" $ do
        let expect = MultiOr.make [ Conditional.top ]
        actual <-
            evaluateOr
                mockMetadataTools
                Equals
                    { equalsOperandSort = testSort
                    , equalsResultSort = testSort
                    , equalsFirst = MultiOr.make
                        [ Conditional
                            { term = Mock.a
                            , predicate = makeTruePredicate
                            , substitution = mempty
                            }
                        ]
                    , equalsSecond = MultiOr.make
                        [ Conditional
                            { term = Mock.a
                            , predicate = makeTruePredicate
                            , substitution = mempty
                            }
                        ]
                    }
        assertEqualWithExplanation "" expect actual

    , testCase "a != bottom" $ do
        let expect = MultiOr.make []
        actual <-
            evaluateOr
                mockMetadataTools
                Equals
                    { equalsOperandSort = testSort
                    , equalsResultSort = testSort
                    , equalsFirst = MultiOr.make []
                    , equalsSecond = MultiOr.make
                        [ Conditional
                            { term = Mock.a
                            , predicate = makeTruePredicate
                            , substitution = mempty
                            }
                        ]
                    }
        assertEqualWithExplanation "" expect actual

    , testCase "f(a) vs g(a)" $ do
        let expect =
                MultiOr.make
                    [ Conditional
                        { term = mkTop_
                        , predicate = makeEqualsPredicate fOfA gOfA
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluateOr
                mockMetadataTools
                Equals
                    { equalsOperandSort = testSort
                    , equalsResultSort = testSort
                    , equalsFirst = MultiOr.make
                        [ Conditional
                            { term = fOfA
                            , predicate = makeTruePredicate
                            , substitution = mempty
                            }
                        ]
                    , equalsSecond = MultiOr.make
                        [ Conditional
                            { term = gOfA
                            , predicate = makeTruePredicate
                            , substitution = mempty
                            }
                        ]
                    }
        assertEqualWithExplanation "" expect actual

    , testCase "f vs g or h" $ do
        let expect =
                MultiOr.make
                    [ Conditional
                        { term = mkTop_
                        , predicate =
                            makeMultipleAndPredicate
                                [ makeCeilPredicate Mock.cf
                                , makeOrPredicate
                                    (makeCeilPredicate Mock.cg)
                                    (makeCeilPredicate Mock.ch)
                                , makeImpliesPredicate
                                    (makeCeilPredicate Mock.cg)
                                    (makeEqualsPredicate Mock.cf Mock.cg)
                                , makeImpliesPredicate
                                    (makeCeilPredicate Mock.ch)
                                    (makeEqualsPredicate Mock.cf Mock.ch)
                                ]
                        , substitution = mempty
                        }
                    ,  Conditional
                        { term = mkTop_
                        , predicate =
                            makeMultipleAndPredicate
                                [ makeNotPredicate $ makeCeilPredicate Mock.cf
                                , makeNotPredicate $ makeCeilPredicate Mock.cg
                                , makeNotPredicate $ makeCeilPredicate Mock.ch
                                ]
                        , substitution = mempty
                        }
                    ]
            first =
                MultiOr.make
                    [ Conditional
                        { term = Mock.cf
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
            second =
                MultiOr.make
                    [ Conditional
                        { term = Mock.cg
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    , Conditional
                        { term = Mock.ch
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
        actual1 <-
            evaluateOr
                mockMetadataTools
                Equals
                    { equalsOperandSort = testSort
                    , equalsResultSort = testSort
                    , equalsFirst = first
                    , equalsSecond = second
                    }
        assertEqualWithExplanation "f vs g or h" expect actual1
        actual2 <-
            evaluateOr
                mockMetadataTools
                Equals
                    { equalsOperandSort = testSort
                    , equalsResultSort = testSort
                    , equalsFirst = second
                    , equalsSecond = first
                    }
        assertEqualWithExplanation "g or h or f" expect actual2

    , testCase "f vs g[x = a] or h" $ do
        let expect =
                MultiOr.make
                    [ Conditional
                        { term = mkTop_
                        , predicate =
                            makeMultipleAndPredicate
                                [ definedF
                                , makeOrPredicate definedG definedH
                                , makeImpliesPredicate
                                    definedG
                                    (makeEqualsPredicate Mock.cf Mock.cg)
                                , makeImpliesPredicate
                                    definedH
                                    (makeEqualsPredicate Mock.cf Mock.ch)
                                ]
                        , substitution = mempty
                        }
                    ,  Conditional
                        { term = mkTop_
                        , predicate =
                            makeMultipleAndPredicate
                                [ makeNotPredicate definedF
                                , makeNotPredicate definedG
                                , makeNotPredicate definedH
                                ]
                        , substitution = mempty
                        }
                    ]
              where
                definedF = makeCeilPredicate Mock.cf
                definedG =
                    makeAndPredicate
                        (makeCeilPredicate Mock.cg)
                        (makeEqualsPredicate (mkVar Mock.x) Mock.a)
                definedH = makeCeilPredicate Mock.ch
            first =
                MultiOr.make
                    [ Conditional
                        { term = Mock.cf
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
            second =
                MultiOr.make
                    [ Conditional
                        { term = Mock.cg
                        , predicate = makeTruePredicate
                        , substitution = Substitution.wrap [(Mock.x, Mock.a)]
                        }
                    , Conditional
                        { term = Mock.ch
                        , predicate = makeTruePredicate
                        , substitution = mempty
                        }
                    ]
            test1 =
                Equals
                    { equalsOperandSort = testSort
                    , equalsResultSort = testSort
                    , equalsFirst = first
                    , equalsSecond = second
                    }
        actual1 <- evaluateOr mockMetadataTools test1
        let message1 =
                unlines
                    [ "Expected"
                    , unparseToString (OrOfExpandedPattern.toExpandedPattern <$> test1)
                    , "would simplify to:"
                    , unlines (unparseToString <$> Foldable.toList expect)
                    , "but instead found:"
                    , unlines (unparseToString <$> Foldable.toList actual1)
                    ]
        assertEqual message1 expect actual1
        actual2 <-
            evaluateOr
                mockMetadataTools
                Equals
                    { equalsOperandSort = testSort
                    , equalsResultSort = testSort
                    , equalsFirst = second
                    , equalsSecond = first
                    }
        assertEqualWithExplanation "g[x = a] or h or f" expect actual2
    ]

test_equalsSimplification_ExpandedPatterns :: [TestTree]
test_equalsSimplification_ExpandedPatterns =
    [ testCase "predicate-substitution vs predicate-substitution" $ do
        let expect =
                MultiOr.make
                    [ Conditional
                        { term = mkTop_
                        , predicate =
                            makeIffPredicate
                                (makeEqualsPredicate fOfA fOfB)
                                (makeEqualsPredicate gOfA gOfB)
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluate
                mockMetadataTools
                Conditional
                    { term = mkTop_
                    , predicate = makeEqualsPredicate fOfA fOfB
                    , substitution = mempty
                    }
                Conditional
                    { term = mkTop_
                    , predicate = makeEqualsPredicate gOfA gOfB
                    , substitution = mempty
                    }
        assertEqualWithExplanation "" expect actual

    , testCase "constructor-patt vs constructor-patt" $ do
        let expect =
                MultiOr.make
                    [ Conditional
                        { term = mkTop_
                        , predicate =
                            makeOrPredicate
                                ( makeAndPredicate
                                    (makeAndPredicate
                                        (makeAndPredicate
                                            (makeAndPredicate
                                                (makeEqualsPredicate hOfA hOfB)
                                                (makeCeilPredicate hOfA)
                                            )
                                            (makeEqualsPredicate fOfA fOfB)
                                        )
                                        (makeCeilPredicate hOfB)
                                    )
                                    (makeEqualsPredicate gOfA gOfB)
                                )
                                (makeAndPredicate
                                    (makeNotPredicate
                                        (makeAndPredicate
                                            (makeCeilPredicate hOfA)
                                            (makeEqualsPredicate fOfA fOfB)
                                        )
                                    )
                                    (makeNotPredicate
                                        (makeAndPredicate
                                            (makeCeilPredicate hOfB)
                                            (makeEqualsPredicate gOfA gOfB)
                                        )
                                    )
                                )
                        , substitution = mempty
                        }
                    ]
        actual <-
            evaluate
                mockMetadataTools
                Conditional
                    { term = Mock.functionalConstr10 hOfA
                    , predicate = makeEqualsPredicate fOfA fOfB
                    , substitution = mempty
                    }
                Conditional
                    { term = Mock.functionalConstr10 hOfB
                    , predicate = makeEqualsPredicate gOfA gOfB
                    , substitution = mempty
                    }
        assertEqualWithExplanation "" expect actual
    ]

test_equalsSimplification_Patterns :: [TestTree]
test_equalsSimplification_Patterns =
    [ testCase "bottom == bottom"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.topPredicate
            mkBottom_
            mkBottom_
        )
    , testCase "domain-value == domain-value"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.topPredicate
            (mkDomainValue
                (Domain.BuiltinExternal Domain.External
                    { domainValueSort = testSort
                    , domainValueChild = eraseAnnotations $ mkStringLiteral "a"
                    }
                )
            )
            (mkDomainValue
                (Domain.BuiltinExternal Domain.External
                    { domainValueSort = testSort
                    , domainValueChild = eraseAnnotations $ mkStringLiteral "a"
                    }
                )
            )
        )
    , testCase "domain-value != domain-value"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.bottomPredicate
            (mkDomainValue
                (Domain.BuiltinExternal Domain.External
                    { domainValueSort = testSort
                    , domainValueChild = eraseAnnotations $ mkStringLiteral "a"
                    }
                )
            )
            (mkDomainValue
                (Domain.BuiltinExternal Domain.External
                    { domainValueSort = testSort
                    , domainValueChild = eraseAnnotations $ mkStringLiteral "b"
                    }
                )
            )
        )
    , testCase "domain-value != domain-value because of sorts"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.bottomPredicate
            (mkDomainValue
                (Domain.BuiltinExternal Domain.External
                    { domainValueSort = testSort
                    , domainValueChild = eraseAnnotations $ mkStringLiteral "a"
                    }
                )
            )
            (mkDomainValue
                (Domain.BuiltinExternal Domain.External
                    { domainValueSort = testSort2
                    , domainValueChild = eraseAnnotations $ mkStringLiteral "a"
                    }
                )
            )
        )
    , testCase "\"a\" == \"a\""
        (assertTermEqualsGeneric
            mockMetaMetadataTools
            PredicateSubstitution.topPredicate
            (mkStringLiteral "a")
            (mkStringLiteral "a")
        )
    , testCase "\"a\" != \"b\""
        (assertTermEqualsGeneric
            mockMetaMetadataTools
            PredicateSubstitution.bottomPredicate
            (mkStringLiteral "a")
            (mkStringLiteral "b")
        )
    , testCase "'a' == 'a'"
        (assertTermEqualsGeneric
            mockMetaMetadataTools
            PredicateSubstitution.topPredicate
            (mkCharLiteral 'a')
            (mkCharLiteral 'a')
        )
    , testCase "'a' != 'b'"
        (assertTermEqualsGeneric
            mockMetaMetadataTools
            PredicateSubstitution.bottomPredicate
            (mkCharLiteral 'a')
            (mkCharLiteral 'b')
        )
    , testCase "a != bottom"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.bottomPredicate
            mkBottom_
            Mock.a
        )
    , testCase "a == a"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.topPredicate
            Mock.a
            Mock.a
        )
    , testCase "f(a) vs g(a)"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeEqualsPredicate fOfA gOfA
                , substitution = mempty
                }
            fOfA
            gOfA
        )
    , testCase "constructor1(a) vs constructor1(a)"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.topPredicate
            constructor1OfA
            constructor1OfA
        )
    , testCase
        "functionalconstructor1(a) vs functionalconstructor2(a)"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.bottomPredicate
            functionalConstructor1OfA
            functionalConstructor2OfA
        )
    , testCase "constructor1(a) vs constructor2(a)"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.bottomPredicate
            constructor1OfA
            constructor2OfA
        )
    , testCase "constructor1(f(a)) vs constructor1(f(a))"
        (assertTermEquals
            mockMetadataTools
            PredicateSubstitution.topPredicate
            (Mock.constr10 fOfA)
            (Mock.constr10 fOfA)
        )
    , testCase "sigma(f(a), f(b)) vs sigma(g(a), g(b))"
        (assertTermEqualsMulti
            mockMetadataTools
            [ Conditional
                { term = ()
                , predicate =
                    makeAndPredicate
                        (makeEqualsPredicate fOfA gOfA)
                        (makeEqualsPredicate fOfB gOfB)
                , substitution = mempty
                }
            , Conditional
                { term = ()
                , predicate =
                    makeAndPredicate
                        (makeNotPredicate
                            (makeAndPredicate
                                (makeCeilPredicate fOfA)
                                (makeCeilPredicate fOfB)
                            )
                        )
                        (makeNotPredicate
                            (makeAndPredicate
                                (makeCeilPredicate gOfA)
                                (makeCeilPredicate gOfB)
                            )
                        )
                , substitution = mempty
                }
            ]
            (Mock.functionalConstr20 fOfA fOfB)
            (Mock.functionalConstr20 gOfA gOfB)
        )
    , testCase "equals(x, functional) becomes a substitution"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeTruePredicate
                , substitution =
                    Substitution.unsafeWrap [(Mock.x, functionalOfA)]
                }
                (mkVar Mock.x)
                functionalOfA
        )
    , testCase "equals(functional, x) becomes a substitution"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeTruePredicate
                , substitution =
                    Substitution.unsafeWrap [(Mock.x, functionalOfA)]
                }
                functionalOfA
                (mkVar Mock.x)
        )
    , testCase "equals(x, function) becomes a substitution + ceil"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeCeilPredicate fOfA
                , substitution = Substitution.unsafeWrap [(Mock.x, fOfA)]
                }
            (mkVar Mock.x)
            fOfA
        )
    , testCase "equals(function, x) becomes a substitution + ceil"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeCeilPredicate fOfA
                , substitution = Substitution.unsafeWrap [(Mock.x, fOfA)]
                }
            fOfA
            (mkVar Mock.x)
        )
    , testCase "equals(x, constructor) becomes a predicate"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeEqualsPredicate (mkVar Mock.x) constructor1OfA
                , substitution = mempty
                }
            (mkVar Mock.x)
            constructor1OfA
        )
    , testCase "equals(constructor, x) becomes a predicate"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeEqualsPredicate constructor1OfA (mkVar Mock.x)
                , substitution = mempty
                }
            constructor1OfA
            (mkVar Mock.x)
        )
    , testCase "equals(x, something) becomes a predicate"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeEqualsPredicate (mkVar Mock.x) plain1OfA
                , substitution = mempty
                }
            (mkVar Mock.x)
            plain1OfA
        )
    , testCase "equals(something, x) becomes a predicate"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeEqualsPredicate plain1OfA (mkVar Mock.x)
                , substitution = mempty
                }
            plain1OfA
            (mkVar Mock.x)
        )
    , testCase "equals(function, constructor) is not simplifiable"
        (assertTermEquals
            mockMetadataTools
            Conditional
                { term = ()
                , predicate = makeEqualsPredicate (Mock.f Mock.a) Mock.a
                , substitution = mempty
                }
                (Mock.f Mock.a)
                Mock.a
        )
    , testGroup "builtin Map domain"
        [ testCase "concrete Map, same keys"
            (assertTermEquals
                mockMetadataTools
                Conditional
                    { term = ()
                    , predicate = makeTruePredicate
                    , substitution = Substitution.unsafeWrap [(Mock.x, Mock.b)]
                    }
                (Mock.builtinMap [(Mock.aConcrete, Mock.b)])
                (Mock.builtinMap [(Mock.aConcrete, mkVar Mock.x)])
            )
        , testCase "concrete Map, different keys"
            (assertTermEquals
                mockMetadataTools
                PredicateSubstitution.bottomPredicate
                (Mock.builtinMap [(Mock.aConcrete, Mock.b)])
                (Mock.builtinMap [(Mock.bConcrete, mkVar Mock.x)])
            )
        , testCase "concrete Map with framed Map"
            (assertTermEquals
                mockMetadataTools
                Conditional
                    { term = ()
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate fOfA)
                            (makeCeilPredicate fOfB)
                    , substitution = Substitution.wrap
                        [ (Mock.x, fOfA)
                        , (Mock.m, Mock.builtinMap [(Mock.bConcrete, fOfB)])
                        ]
                    }
                (Mock.builtinMap
                    [ (Mock.aConcrete, fOfA)
                    , (Mock.bConcrete, fOfB)
                    ]
                )
                (Mock.concatMap
                    (Mock.builtinMap [(Mock.aConcrete, mkVar Mock.x)])
                    (mkVar Mock.m)
                )
            )
        , testCase "concrete Map with framed Map"
            (assertTermEquals
                mockMetadataTools
                Conditional
                    { term = ()
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate fOfA)
                            (makeCeilPredicate fOfB)
                    , substitution = Substitution.wrap
                        [ (Mock.x, fOfA)
                        , (Mock.m, Mock.builtinMap [(Mock.bConcrete, fOfB)])
                        ]
                    }
                (Mock.builtinMap
                    [ (Mock.aConcrete, fOfA)
                    , (Mock.bConcrete, fOfB)
                    ]
                )
                (Mock.concatMap
                    (mkVar Mock.m)
                    (Mock.builtinMap [(Mock.aConcrete, mkVar Mock.x)])
                )
            )
        , testCase "framed Map with concrete Map"
            (assertTermEquals
                mockMetadataTools
                Conditional
                    { term = ()
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate fOfA)
                            (makeCeilPredicate fOfB)
                    , substitution = Substitution.wrap
                        [ (Mock.x, fOfA)
                        , (Mock.m, Mock.builtinMap [(Mock.bConcrete, fOfB)])
                        ]
                    }
                (Mock.concatMap
                    (Mock.builtinMap [(Mock.aConcrete, mkVar Mock.x)])
                    (mkVar Mock.m)
                )
                (Mock.builtinMap
                    [ (Mock.aConcrete, fOfA)
                    , (Mock.bConcrete, fOfB)
                    ]
                )
            )
        , testCase "framed Map with concrete Map"
            (assertTermEquals
                mockMetadataTools
                Conditional
                    { term = ()
                    , predicate =
                        makeAndPredicate
                            (makeCeilPredicate fOfA)
                            (makeCeilPredicate fOfB)
                    , substitution = Substitution.wrap
                        [ (Mock.x, fOfA)
                        , (Mock.m, Mock.builtinMap [(Mock.bConcrete, fOfB)])
                        ]
                    }
                (Mock.concatMap
                    (mkVar Mock.m)
                    (Mock.builtinMap [(Mock.aConcrete, mkVar Mock.x)])
                )
                (Mock.builtinMap
                    [ (Mock.aConcrete, fOfA)
                    , (Mock.bConcrete, fOfB)
                    ]
                )
            )
        -- TODO: Add tests with non-trivial predicates.
        ]
    , testGroup "builtin List domain"
        [
            let term1 =
                    Mock.builtinList
                        [ Mock.constr10 Mock.cf
                        , Mock.constr11 Mock.cf
                        ]
            in
                testCase "[same head, same head]"
                    (assertTermEquals
                        mockMetadataTools
                        Conditional
                            { term = ()
                            , predicate = makeTruePredicate
                            , substitution = mempty
                            }
                        term1
                        term1
                    )
        ,
            let term3 = Mock.builtinList [Mock.a, Mock.a]
                term4 = Mock.builtinList [Mock.a, Mock.b]
                unified34 = PredicateSubstitution.bottomPredicate
            in
                testCase "[same head, different head]"
                    (assertTermEquals
                        mockMetadataTools
                        unified34
                        term3
                        term4
                    )
        ,
            let term5 = Mock.concatList
                        (Mock.builtinList [Mock.a])
                        (mkVar Mock.x)
                term6 = Mock.builtinList $ [Mock.a, Mock.b]
            in
                testCase "[a] `concat` x /\\ [a, b] "
                    (assertTermEquals
                        mockMetadataTools
                        Conditional
                            { term = ()
                            , predicate = makeTruePredicate
                            , substitution = Substitution.unsafeWrap
                                [(Mock.x, Mock.builtinList [Mock.b])]
                            }
                        term5
                        term6
                    )
        ,
            let term7 = Mock.builtinList [Mock.a, Mock.a]
                term8 = Mock.builtinList [Mock.a]
            in
                testCase "different lengths"
                    (assertTermEquals
                        mockMetadataTools
                        PredicateSubstitution.bottomPredicate
                        term7
                        term8
                    )
        -- TODO: Add tests with non-trivial unifications and predicates.
        ]
    -- TODO: Add tests for set equality.
    ]

assertTermEquals
    :: HasCallStack
    => SmtMetadataTools StepperAttributes
    -> CommonPredicateSubstitution Object
    -> TermLike Variable
    -> TermLike Variable
    -> IO ()
assertTermEquals = assertTermEqualsGeneric

assertTermEqualsGeneric
    :: (MetaOrObject level, HasCallStack)
    => SmtMetadataTools StepperAttributes
    -> CommonPredicateSubstitution level
    -> TermLike Variable
    -> TermLike Variable
    -> Assertion
assertTermEqualsGeneric tools expectPure =
    assertTermEqualsMultiGeneric tools [expectPure]


assertTermEqualsMulti
    :: HasCallStack
    => SmtMetadataTools StepperAttributes
    -> [CommonPredicateSubstitution Object]
    -> TermLike Variable
    -> TermLike Variable
    -> IO ()
assertTermEqualsMulti = assertTermEqualsMultiGeneric

assertTermEqualsMultiGeneric
    :: (MetaOrObject level, HasCallStack)
    => SmtMetadataTools StepperAttributes
    -> [CommonPredicateSubstitution level]
    -> TermLike Variable
    -> TermLike Variable
    -> Assertion
assertTermEqualsMultiGeneric tools expectPure first second = do
    let expectExpanded =
            MultiOr.make
                (map predSubstToExpandedPattern expectPure)
    actualExpanded <-
        evaluateGeneric
            tools
            (termToExpandedPattern first)
            (termToExpandedPattern second)
    assertEqualWithExplanation
        "ExpandedPattern"
        expectExpanded
        actualExpanded
    actualPure <- evaluateTermsGeneric tools first second
    assertEqualWithExplanation
        "PureMLPattern"
        (MultiOr.make expectPure)
        actualPure
  where
    termToExpandedPattern
        :: MetaOrObject level
        => TermLike Variable
        -> CommonExpandedPattern level
    termToExpandedPattern (Bottom_ _) =
        Conditional.bottom
    termToExpandedPattern term =
        Conditional
            { term = term
            , predicate = makeTruePredicate
            , substitution = mempty
            }
    predSubstToExpandedPattern
        :: MetaOrObject level
        => CommonPredicateSubstitution level
        -> CommonExpandedPattern level
    predSubstToExpandedPattern
        Conditional {predicate = PredicateFalse}
      =
        Conditional.bottom
    predSubstToExpandedPattern
        Conditional {predicate, substitution}
      =
        Conditional
            { term = mkTop_
            , predicate = predicate
            , substitution = substitution
            }

fOfA :: TermLike Variable
fOfA = Mock.f Mock.a

fOfB :: TermLike Variable
fOfB = Mock.f Mock.b

gOfA :: TermLike Variable
gOfA = Mock.g Mock.a

gOfB :: TermLike Variable
gOfB = Mock.g Mock.b

hOfA :: TermLike Variable
hOfA = Mock.h Mock.a

hOfB :: TermLike Variable
hOfB = Mock.h Mock.b

functionalOfA :: TermLike Variable
functionalOfA = Mock.functional10 Mock.a

constructor1OfA :: TermLike Variable
constructor1OfA = Mock.constr10 Mock.a

constructor2OfA :: TermLike Variable
constructor2OfA = Mock.constr11 Mock.a

functionalConstructor1OfA :: TermLike Variable
functionalConstructor1OfA = Mock.functionalConstr10 Mock.a

functionalConstructor2OfA :: TermLike Variable
functionalConstructor2OfA = Mock.functionalConstr11 Mock.a

plain1OfA :: TermLike Variable
plain1OfA = Mock.plain10 Mock.a

mockMetadataTools :: SmtMetadataTools StepperAttributes
mockMetadataTools =
    Mock.makeMetadataTools
        Mock.attributesMapping
        Mock.headTypeMapping
        Mock.sortAttributesMapping
        Mock.subsorts
        Mock.headSortsMapping
        Mock.smtDeclarations

mockMetaMetadataTools :: SmtMetadataTools StepperAttributes
mockMetaMetadataTools =
    Mock.makeMetadataTools [] [] [] [] [] Mock.emptySmtDeclarations

testSort :: Sort Object
testSort = Mock.testSort

testSort2 :: Sort Object
testSort2 =
    SortActualSort SortActual
        { sortActualName  = Id "testSort2" AstLocationTest
        , sortActualSorts = []
        }

evaluateOr
    :: SmtMetadataTools StepperAttributes
    -> Equals Object (CommonOrOfExpandedPattern Object)
    -> IO (CommonOrOfExpandedPattern Object)
evaluateOr tools equals =
    (<$>) fst
    $ SMT.runSMT SMT.defaultConfig
    $ evalSimplifier emptyLogger
    $ simplify
        tools
        (Mock.substitutionSimplifier tools)
        (Simplifier.create tools Map.empty)
        Map.empty
        equals

evaluate
    :: SmtMetadataTools StepperAttributes
    -> CommonExpandedPattern Object
    -> CommonExpandedPattern Object
    -> IO (CommonOrOfExpandedPattern Object)
evaluate = evaluateGeneric

evaluateGeneric
    :: MetaOrObject level
    => SmtMetadataTools StepperAttributes
    -> CommonExpandedPattern level
    -> CommonExpandedPattern level
    -> IO (CommonOrOfExpandedPattern level)
evaluateGeneric tools first second =
    (<$>) fst
    $ SMT.runSMT SMT.defaultConfig
    $ evalSimplifier emptyLogger
    $ makeEvaluate
        tools
        (Mock.substitutionSimplifier tools)
        (Simplifier.create tools Map.empty)
        Map.empty
        first
        second

evaluateTermsGeneric
    :: MetaOrObject level
    => SmtMetadataTools StepperAttributes
    -> TermLike Variable
    -> TermLike Variable
    -> IO (CommonOrOfPredicateSubstitution level)
evaluateTermsGeneric tools first second =
    (<$>) fst
    $ SMT.runSMT SMT.defaultConfig
    $ evalSimplifier emptyLogger
    $ makeEvaluateTermsToPredicateSubstitution
        tools
        (Mock.substitutionSimplifier tools)
        (Simplifier.create tools Map.empty)
        Map.empty
        first
        second
