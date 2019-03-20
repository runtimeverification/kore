{-# LANGUAGE OverloadedLists #-}

module Test.Kore.Step.Step
    ( test_baseStepMultipleRemainder
    , test_applyRule
    , test_unifyRule
    , test_applyRewriteRule_
    , test_applyRewriteRule
    ) where

import Test.Tasty
import Test.Tasty.HUnit

import           Control.Monad.Except
                 ( ExceptT, runExceptT )
import           Data.Default as Default
                 ( def )
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import           Kore.Predicate.Predicate as Predicate
import           Kore.Step.AxiomPatterns
                 ( RewriteRule (RewriteRule), RulePattern (..) )
import qualified Kore.Step.AxiomPatterns as RulePattern
import           Kore.Step.Error
import           Kore.Step.Representation.ExpandedPattern
                 ( CommonExpandedPattern, ExpandedPattern,
                 PredicateSubstitution, Predicated (..) )
import           Kore.Step.Representation.MultiOr
                 ( MultiOr )
import qualified Kore.Step.Representation.MultiOr as MultiOr
                 ( make )
import qualified Kore.Step.Representation.PredicateSubstitution as PredicateSubstitution
import           Kore.Step.Simplification.Data
import qualified Kore.Step.Simplification.PredicateSubstitution as PredicateSubstitution
import qualified Kore.Step.Simplification.Simplifier as Simplifier
                 ( create )
import           Kore.Step.Step hiding
                 ( applyRewriteRule, applyRule, unifyRule )
import qualified Kore.Step.Step as Step
import           Kore.Step.StepperAttributes
import           Kore.Unification.Error
                 ( SubstitutionError (..) )
import qualified Kore.Unification.Procedure as Unification
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unification.Unifier
                 ( UnificationError (..) )
import           Kore.Variables.Fresh
                 ( nextVariable )
import qualified SMT

import           Test.Kore
import           Test.Kore.Comparators ()
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock
                 ( makeMetadataTools )
import qualified Test.Kore.Step.MockSimplifiers as Mock
import qualified Test.Kore.Step.MockSymbols as Mock
import           Test.Tasty.HUnit.Extensions

test_baseStepMultipleRemainder :: [TestTree]
test_baseStepMultipleRemainder =
    [ testCase "If-then" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: if x then cg
        --   axiom: if true y => y
        -- Actual:
        --   term: constr20(x, cg)
        --   axiom: constr20(a, y) => y
        -- Expected:
        --   rewritten: cg, with ⌈cg⌉ and [x=a]
        --   remainder: constr20(x, cg), with ¬(⌈cg⌉ and x=a)
        let
            expected =
                OrStepResult
                    { rewrittenPattern = MultiOr.make
                        [ Predicated
                            { term = Mock.cg
                            , predicate = makeCeilPredicate Mock.cg
                            , substitution =
                                Substitution.wrap [(Mock.x, Mock.a)]
                            }
                        ]
                    , remainder = MultiOr.make
                        [ Predicated
                            { term =
                                Mock.functionalConstr20 (mkVar Mock.x) Mock.cg
                            , predicate =
                                makeNotPredicate
                                    (makeAndPredicate
                                        (makeCeilPredicate Mock.cg)
                                        (makeEqualsPredicate (mkVar Mock.x) Mock.a)
                                    )
                            , substitution = mempty
                            }
                        ]
                    }
        actual <- runStepWithRemainders
            mockMetadataTools
            Predicated
                { term = Mock.functionalConstr20 (mkVar Mock.x) Mock.cg
                , predicate = makeTruePredicate
                , substitution = mempty
                }
            [RewriteRule RulePattern
                { left = Mock.functionalConstr20 Mock.a (mkVar Mock.y)
                , right = mkVar Mock.y
                , requires = makeTruePredicate
                , ensures = makeTruePredicate
                , attributes = def
                }
            ]
        assertEqualWithExplanation "" expected actual
    , testCase "case" $ do
        -- This uses `functionalConstr30(x, y, z)` to represent a case
        -- statement,
        -- i.e. `case x of 1 -> y; 2 -> z`
        -- and `a`, `b` as the case labels.
        --
        -- Intended:
        --   term: case x of 1 -> cf; 2 -> cg
        --   axiom: case 1 of 1 -> cf; 2 -> cg => cf
        --   axiom: case 2 of 1 -> cf; 2 -> cg => cg
        -- Actual:
        --   term: constr30(x, cg, cf)
        --   axiom: constr30(a, y, z) => y
        --   axiom: constr30(b, y, z) => z
        -- Expected:
        --   rewritten: cf, with ⌈cf⌉ and ⌈cg⌉ and [x=a]
        --   rewritten:
        --      cg, with ¬(⌈cf⌉ and ⌈cg⌉ and x=b) and (⌈cf⌉ and ⌈cg⌉ and b=a)
        --   remainder:
        --     constr20(x, cf, cg)
        --        with ¬(⌈cf⌉ and ⌈cg⌉ and x=a)
        --        and ¬(⌈cf⌉ and ⌈cg⌉ and x=b)
        let
            unificationNotBottom =
                makeAndPredicate
                    (makeCeilPredicate Mock.cf)
                    (makeCeilPredicate Mock.cg)
            expected =
                OrStepResult
                    { rewrittenPattern = MultiOr.make
                        [ Predicated
                            { term = Mock.cf
                            , predicate = unificationNotBottom
                            , substitution =
                                Substitution.wrap [(Mock.x, Mock.a)]
                            }
                        , Predicated
                            { term = Mock.cg
                            , predicate = makeAndPredicate
                                (makeNotPredicate
                                    (makeAndPredicate
                                        unificationNotBottom
                                        (makeEqualsPredicate Mock.b Mock.a)
                                    )
                                )
                                unificationNotBottom
                            , substitution =
                                Substitution.wrap [(Mock.x, Mock.b)]
                            }
                        ]
                    , remainder = MultiOr.make
                        [ Predicated
                            { term =
                                Mock.functionalConstr30
                                    (mkVar Mock.x) Mock.cf Mock.cg
                            , predicate =
                                makeAndPredicate
                                    (makeNotPredicate
                                        (makeAndPredicate
                                            unificationNotBottom
                                            (makeEqualsPredicate
                                                (mkVar Mock.x)
                                                Mock.a
                                            )
                                        )
                                    )
                                    (makeNotPredicate
                                        (makeAndPredicate
                                            unificationNotBottom
                                            (makeEqualsPredicate
                                                (mkVar Mock.x)
                                                Mock.b
                                            )
                                        )
                                    )
                            , substitution = mempty
                            }
                        ]
                    }
        actual <- runStepWithRemainders
            mockMetadataTools
            Predicated
                { term =
                    Mock.functionalConstr30 (mkVar Mock.x) Mock.cf Mock.cg
                , predicate = makeTruePredicate
                , substitution = mempty
                }
            [ RewriteRule RulePattern
                { left = Mock.functionalConstr30
                    Mock.a (mkVar Mock.y) (mkVar Mock.z)
                , right = mkVar Mock.y
                , requires = makeTruePredicate
                , ensures = makeTruePredicate
                , attributes = def
                }
            , RewriteRule RulePattern
                { left = Mock.functionalConstr30
                    Mock.b (mkVar Mock.y) (mkVar Mock.z)
                , right = mkVar Mock.z
                , requires = makeTruePredicate
                , ensures = makeTruePredicate
                , attributes = def
                }
            ]
        assertEqualWithExplanation "" expected actual
    ]

mockMetadataTools :: MetadataTools Object StepperAttributes
mockMetadataTools =
    Mock.makeMetadataTools
        Mock.attributesMapping
        Mock.headTypeMapping
        Mock.sortAttributesMapping
        Mock.subsorts

runStepWithRemainders
    :: MetaOrObject level
    => MetadataTools level StepperAttributes
    -- ^functions yielding metadata for pattern heads
    -> CommonExpandedPattern level
    -- ^left-hand-side of unification
    -> [RewriteRule level Variable]
    -> IO (OrStepResult level Variable)
runStepWithRemainders metadataTools configuration axioms =
    fst
    <$> SMT.runSMT SMT.defaultConfig
            ( evalSimplifier emptyLogger
            $ stepWithRemainders
                metadataTools
                (Mock.substitutionSimplifier metadataTools)
                (Simplifier.create metadataTools Map.empty)
                Map.empty
                configuration
                axioms
            )

evalUnifier
    :: BranchT (ExceptT e Simplifier) a
    -> IO (Either e (MultiOr a))
evalUnifier =
    SMT.runSMT SMT.defaultConfig
    . evalSimplifier emptyLogger
    . runExceptT
    . gather

applyRule
    :: PredicateSubstitution Object Variable
    -> UnifiedRule Variable
    -> IO
        (Either
            (StepError Object Variable)
            (MultiOr (ExpandedPattern Object Variable))
        )
applyRule initial unifiedRule =
    evalUnifier
    $ Step.applyRule
        metadataTools
        predicateSimplifier
        patternSimplifier
        axiomSimplifiers
        initial
        unifiedRule
  where
    metadataTools = mockMetadataTools
    predicateSimplifier =
        PredicateSubstitution.create
            metadataTools
            patternSimplifier
            axiomSimplifiers
    patternSimplifier =
        Simplifier.create
            metadataTools
            axiomSimplifiers
    axiomSimplifiers = Map.empty

test_applyRule :: [TestTree]
test_applyRule =
    [ testCase "\\bottom initial condition" $ do
        let unifiedRule =
                Predicated
                    { term = axiom
                    , predicate = Predicate.makeTruePredicate
                    , substitution = mempty
                    }
            axiom =
                RulePattern
                    { left = Mock.a
                    , right = Mock.b
                    , requires = Predicate.makeTruePredicate
                    , ensures = makeTruePredicate
                    , attributes = Default.def
                    }
            initial = PredicateSubstitution.bottom
            expect = Right []
        actual <- applyRule initial unifiedRule
        assertEqual "" expect actual

    , testCase "returns axiom right-hand side" $ do
        let unifiedRule =
                Predicated
                    { term = axiom
                    , predicate = Predicate.makeTruePredicate
                    , substitution = mempty
                    }
            axiom =
                RulePattern
                    { left = Mock.a
                    , right = Mock.b
                    , requires = Predicate.makeTruePredicate
                    , ensures = makeTruePredicate
                    , attributes = Default.def
                    }
            initial = PredicateSubstitution.top
            expect = Right [ right axiom <$ initial ]
        actual <- applyRule initial unifiedRule
        assertEqual "" expect actual

    , testCase "combine initial and rule conditions" $ do
        let unifiedRule =
                Predicated
                    { term = axiom
                    , predicate = expect2
                    , substitution = mempty
                    }
            axiom =
                RulePattern
                    { left = Mock.a
                    , right = Mock.b
                    , requires = Predicate.makeTruePredicate
                    , ensures = makeTruePredicate
                    , attributes = Default.def
                    }
            initial = PredicateSubstitution.fromPredicate expect1
            expect1 =
                Predicate.makeEqualsPredicate
                    (Mock.f $ mkVar Mock.x)
                    Mock.a
            expect2 =
                Predicate.makeEqualsPredicate
                    (Mock.f $ mkVar Mock.y)
                    Mock.b
            expect = Predicate.makeAndPredicate expect1 expect2
        Right [ applied ] <- applyRule initial unifiedRule
        let Predicated { predicate = actual } = applied
        assertEqual "" expect actual

    , testCase "conflicting initial and rule conditions" $ do
        let predicate = Predicate.makeEqualsPredicate (mkVar Mock.x) Mock.a
            unifiedRule =
                Predicated
                    { term = axiom
                    , predicate
                    , substitution = mempty
                    }
            axiom =
                RulePattern
                    { left = Mock.a
                    , right = Mock.b
                    , requires = Predicate.makeTruePredicate
                    , ensures = makeTruePredicate
                    , attributes = Default.def
                    }
            initial =
                PredicateSubstitution.fromPredicate
                $ Predicate.makeNotPredicate predicate
            expect = Right []
        actual <- applyRule initial unifiedRule
        assertEqual "" expect actual

    ]

unifyRule
    :: ExpandedPattern Object Variable
    -> RulePattern Object Variable
    -> IO
        (Either
            (StepError Object Variable)
            (MultiOr (Predicated Object Variable (RulePattern Object Variable)))
        )
unifyRule initial rule =
    evalUnifier
    $ Step.unifyRule
        metadataTools
        unificationProcedure
        predicateSimplifier
        patternSimplifier
        axiomSimplifiers
        initial
        rule
  where
    metadataTools = mockMetadataTools
    unificationProcedure = UnificationProcedure Unification.unificationProcedure
    predicateSimplifier =
        PredicateSubstitution.create
            metadataTools
            patternSimplifier
            axiomSimplifiers
    patternSimplifier =
        Simplifier.create
            metadataTools
            axiomSimplifiers
    axiomSimplifiers = Map.empty

test_unifyRule :: [TestTree]
test_unifyRule =
    [ testCase "renames axiom left variables" $ do
        let initial = pure (Mock.f (mkVar Mock.x))
            axiom =
                RulePattern
                    { left = Mock.f (mkVar Mock.x)
                    , right = Mock.g (mkVar Mock.x)
                    , requires =
                        Predicate.makeEqualsPredicate (mkVar Mock.x) Mock.a
                    , ensures = makeTruePredicate
                    , attributes = Default.def
                    }
        Right [unified] <- unifyRule initial axiom
        let Predicated { term = actual } = unified
        assertBool "" (Set.notMember Mock.x $ RulePattern.freeVariables actual)

    , testCase "performs unification with initial term" $ do
        let initial = pure (Mock.functionalConstr10 Mock.a)
            axiom =
                RulePattern
                    { left = Mock.functionalConstr10 (mkVar Mock.x)
                    , right = Mock.g Mock.b
                    , requires = Predicate.makeTruePredicate
                    , ensures = makeTruePredicate
                    , attributes = Default.def
                    }
            expect = Right [(pure axiom) { substitution }]
              where
                substitution = Substitution.unsafeWrap [(Mock.x, Mock.a)]
        actual <- unifyRule initial axiom
        assertEqualWithExplanation "" expect actual

    , testCase "returns unification failures" $ do
        let initial = pure (Mock.functionalConstr10 Mock.a)
            axiom =
                RulePattern
                    { left = Mock.functionalConstr11 (mkVar Mock.x)
                    , right = Mock.g Mock.b
                    , requires = Predicate.makeTruePredicate
                    , ensures = makeTruePredicate
                    , attributes = Default.def
                    }
            expect = Right []
        actual <- unifyRule initial axiom
        assertEqualWithExplanation "" expect actual
    ]

-- | Apply the 'RewriteRule' to the configuration, but discard remainders.
applyRewriteRule_
    :: ExpandedPattern Object Variable
    -- ^ Configuration
    -> RewriteRule Object Variable
    -- ^ Rewrite rule
    -> IO
        (Either
            (StepError Object Variable)
            (MultiOr (ExpandedPattern Object Variable))
        )
applyRewriteRule_ initial rule = do
    result <- applyRewriteRule initial rule
    return (discardRemainders <$> result)
  where
    discardRemainders OrStepResult { rewrittenPattern } = rewrittenPattern

test_applyRewriteRule_ :: [TestTree]
test_applyRewriteRule_ =
    [ testCase "apply identity axiom" $ do
        let expect = Right [ initial ]
            initial = pure (mkVar Mock.x)
        actual <- applyRewriteRule_ initial axiomId
        assertEqualWithExplanation "" expect actual

    , testCase "apply identity without renaming" $ do
        let expect = Right [ initial ]
            initial = pure (mkVar Mock.y)
        actual <- applyRewriteRule_ initial axiomId
        assertEqualWithExplanation "" expect actual

    , testCase "substitute variable with itself" $ do
        let expect = Right [ initial { term = mkVar Mock.x } ]
            initial = pure (Mock.sigma (mkVar Mock.x) (mkVar Mock.x))
        actual <- applyRewriteRule_ initial axiomSigmaId
        assertEqualWithExplanation "" expect actual

    , testCase "merge configuration patterns" $ do
        let term = Mock.functionalConstr10 (mkVar Mock.y)
            expect =
                Right [ initial { term, substitution } ]
              where
                substitution = Substitution.wrap [ (Mock.x, term) ]
            initial = pure (Mock.sigma (mkVar Mock.x) term)
        actual <- applyRewriteRule_ initial axiomSigmaId
        assertEqualWithExplanation "" expect actual

    , testCase "substitution with symbol matching" $ do
        let expect = Right [ initial { term = fz, substitution } ]
              where
                substitution = Substitution.wrap [ (Mock.y, mkVar Mock.z) ]
            fy = Mock.functionalConstr10 (mkVar Mock.y)
            fz = Mock.functionalConstr10 (mkVar Mock.z)
            initial = pure (Mock.sigma fy fz)
        actual <- applyRewriteRule_ initial axiomSigmaId
        assertEqualWithExplanation "" expect actual

    , testCase "merge multiple variables" $ do
        let expect = Right [ initial { term = yy, substitution } ]
              where
                substitution = Substitution.wrap [ (Mock.x, mkVar Mock.y) ]
            xy = Mock.sigma (mkVar Mock.x) (mkVar Mock.y)
            yx = Mock.sigma (mkVar Mock.y) (mkVar Mock.x)
            yy = Mock.sigma (mkVar Mock.y) (mkVar Mock.y)
            initial = pure (Mock.sigma xy yx)
        actual <- applyRewriteRule_ initial axiomSigmaXXYY
        assertEqualWithExplanation "" expect actual

    , testCase "rename quantified right variables" $ do
        let expect = Right [ pure final ]
            final = mkExists (nextVariable Mock.y) (mkVar Mock.y)
            initial = pure (mkVar Mock.y)
            axiom =
                RewriteRule RulePattern
                    { left = mkVar Mock.x
                    , right = mkExists Mock.y (mkVar Mock.x)
                    , requires = makeTruePredicate
                    , ensures = makeTruePredicate
                    , attributes = def
                    }
        actual <- applyRewriteRule_ initial axiom
        assertEqualWithExplanation "" expect actual

    , testCase "symbol clash" $ do
        let expect = Right []
            fx = Mock.functionalConstr10 (mkVar Mock.x)
            gy = Mock.functionalConstr11 (mkVar Mock.y)
            initial = pure (Mock.sigma fx gy)
        actual <- applyRewriteRule_ initial axiomSigmaId
        assertEqualWithExplanation "" expect actual

    , testCase "impossible substitution" $ do
        let expect = Right []
            xfy =
                Mock.sigma
                    (mkVar Mock.x)
                    (Mock.functionalConstr10 (mkVar Mock.y))
            xy = Mock.sigma (mkVar Mock.x) (mkVar Mock.y)
            initial = pure (Mock.sigma xfy xy)
        actual <- applyRewriteRule_ initial axiomSigmaXXYY
        assertEqualWithExplanation "" expect actual

    -- sigma(x, x) -> x
    -- vs
    -- sigma(a, f(b)) with substitution b=a
    , testCase "impossible substitution (ctor)" $ do
        let expect = Right []
            initial =
                Predicated
                    { term =
                        Mock.sigma
                            (mkVar Mock.x)
                            (Mock.functionalConstr10 (mkVar Mock.y))
                    , predicate = Predicate.makeTruePredicate
                    , substitution = Substitution.wrap [(Mock.y, mkVar Mock.x)]
                    }
        actual <- applyRewriteRule_ initial axiomSigmaId
        assertEqualWithExplanation "" expect actual

    -- sigma(x, x) -> x
    -- vs
    -- sigma(a, h(b)) with substitution b=a
    , testCase "circular dependency error" $ do
        let expect =
                -- TODO(virgil): This should probably be a normal result with
                -- b=h(b) in the predicate.
                Left
                $ StepErrorSubstitution
                $ NonCtorCircularVariableDependency [Mock.y]
            initial =
                Predicated
                    { term =
                        Mock.sigma
                            (mkVar Mock.x)
                            (Mock.functional10 (mkVar Mock.y))
                    , predicate = makeTruePredicate
                    , substitution = Substitution.wrap [(Mock.y, mkVar Mock.x)]
                    }
        actual <- applyRewriteRule_ initial axiomSigmaId
        assertEqualWithExplanation "" expect actual

    -- sigma(x, x) -> x
    -- vs
    -- sigma(a, i(b)) with substitution b=a
    , testCase "non-function substitution error" $ do
        let expect = Left $ StepErrorUnification UnsupportedPatterns
            initial =
                pure $ Mock.sigma (mkVar Mock.x) (Mock.plain10 (mkVar Mock.y))
        actual <- applyRewriteRule_ initial axiomSigmaId
        assertEqualWithExplanation "" expect actual

    -- sigma(x, x) -> x
    -- vs
    -- sigma(sigma(a, a), sigma(sigma(b, c), sigma(b, b)))
    , testCase "unify all children" $ do
        let expect = Right
                [ Predicated
                    { term = Mock.sigma zz zz
                    , predicate = makeTruePredicate
                    , substitution = Substitution.wrap
                        [ (Mock.x, zz)
                        , (Mock.y, mkVar Mock.z)
                        ]
                    }
                ]
            xx = Mock.sigma (mkVar Mock.x) (mkVar Mock.x)
            yy = Mock.sigma (mkVar Mock.y) (mkVar Mock.y)
            zz = Mock.sigma (mkVar Mock.z) (mkVar Mock.z)
            yz = Mock.sigma (mkVar Mock.y) (mkVar Mock.z)
            initial = pure $ Mock.sigma xx (Mock.sigma yz yy)
        actual <- applyRewriteRule_ initial axiomSigmaId
        assertEqualWithExplanation "" expect actual

    -- sigma(sigma(x, x), y) => sigma(x, y)
    -- vs
    -- sigma(sigma(a, f(b)), a)
    -- Expected: sigma(f(b), f(b)) and a=f(b)
    , testCase "normalize substitution" $ do
        let
            fb = Mock.functional10 (mkVar Mock.y)
            expect =
                Right
                    [ Predicated
                        { term = Mock.sigma fb fb
                        , predicate = makeTruePredicate
                        , substitution = Substitution.wrap [(Mock.x, fb)]
                        }
                    ]
            initial =
                pure $ Mock.sigma (Mock.sigma (mkVar Mock.x) fb) (mkVar Mock.x)
        actual <- applyRewriteRule_ initial axiomSigmaXXY
        assertEqualWithExplanation "" expect actual

    -- sigma(sigma(x, x), y) => sigma(x, y)
    -- vs
    -- sigma(sigma(a, f(b)), a) and a=f(c)
    -- Expected: sigma(f(b), f(b)) and a=f(b), b=c
    , testCase "merge substitution with initial" $ do
        let
            fy = Mock.functionalConstr10 (mkVar Mock.y)
            fz = Mock.functionalConstr10 (mkVar Mock.z)
            expect =
                Right
                    [ Predicated
                        { term = Mock.sigma fz fz
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.wrap
                                [ (Mock.x, fz)
                                , (Mock.y, mkVar Mock.z)
                                ]
                        }
                    ]
            initial =
                Predicated
                    { term =
                        Mock.sigma (Mock.sigma (mkVar Mock.x) fy) (mkVar Mock.x)
                    , predicate = makeTruePredicate
                    , substitution = Substitution.wrap [(Mock.x, fz)]
                    }
        actual <- applyRewriteRule_ initial axiomSigmaXXY
        assertEqualWithExplanation "" expect actual

    -- "sl1" => x
    -- vs
    -- "sl2"
    -- Expected: bottom
    , testCase "unmatched string literals" $ do
        let expect = Right []
            initial = pure (mkStringLiteral "sl2")
            axiom =
                RewriteRule RulePattern
                    { left = mkStringLiteral "sl1"
                    , right = mkVar Mock.x
                    , requires = makeTruePredicate
                    , ensures = makeTruePredicate
                    , attributes = def
                    }
        actual <- applyRewriteRule_ initial axiom
        assertEqualWithExplanation "" expect actual

    -- x => x
    -- vs
    -- a and g(a)=f(a)
    -- Expected: a and g(a)=f(a)
    , testCase "preserve initial condition" $ do
        let expect = Right [initial]
            predicate =
                makeEqualsPredicate
                    (Mock.functional11 Mock.a)
                    (Mock.functional10 Mock.a)
            initial =
                Predicated
                    { term = Mock.a
                    , predicate
                    , substitution = mempty
                    }
        actual <- applyRewriteRule_ initial axiomId
        assertEqualWithExplanation "" expect actual

    -- sigma(sigma(x, x), y) => sigma(x, y)
    -- vs
    -- sigma(sigma(a, f(b)), a) and g(a)=f(a)
    -- Expected: sigma(f(b), f(b)) and a=f(b) and and g(f(b))=f(f(b))
    , testCase "normalize substitution with initial condition" $ do
        let
            fb = Mock.functional10 (mkVar Mock.y)
            expect =
                Right
                    [ Predicated
                        { term = Mock.sigma fb fb
                        , predicate =
                            makeEqualsPredicate
                                (Mock.functional11 fb)
                                (Mock.functional10 fb)
                        , substitution = Substitution.wrap [(Mock.x, fb)]
                        }
                    ]
            initial =
                Predicated
                    { term =
                        Mock.sigma
                            (Mock.sigma (mkVar Mock.x) fb)
                            (mkVar Mock.x)
                    , predicate =
                        makeEqualsPredicate
                            (Mock.functional11 (mkVar Mock.x))
                            (Mock.functional10 (mkVar Mock.x))
                    , substitution = mempty
                    }
        actual <- applyRewriteRule_ initial axiomSigmaXXY
        assertEqualWithExplanation "" expect actual

    -- x => x ensures g(x)=f(x)
    -- vs
    -- a
    -- Expected: a and g(a)=f(a)
    , testCase "conjoin rule ensures" $ do
        let
            ensures =
                makeEqualsPredicate
                    (Mock.functional11 (mkVar Mock.x))
                    (Mock.functional10 (mkVar Mock.x))
            expect = Right [ initial { predicate = ensures } ]
            initial = pure (mkVar Mock.x)
            axiom = RewriteRule ruleId { ensures }
        actual <- applyRewriteRule_ initial axiom
        assertEqualWithExplanation "" expect actual

    -- x => x requires g(x)=f(x)
    -- vs
    -- a
    -- Expected: y1 and g(a)=f(a)
    , testCase "conjoin rule requirement" $ do
        let
            requires =
                makeEqualsPredicate
                    (Mock.functional11 (mkVar Mock.x))
                    (Mock.functional10 (mkVar Mock.x))
            expect = Right [ initial { predicate = requires } ]
            initial = pure (mkVar Mock.x)
            axiom = RewriteRule ruleId { requires }
        actual <- applyRewriteRule_ initial axiom
        assertEqualWithExplanation "" expect actual
    ]
  where
    ruleId =
        RulePattern
            { left = mkVar Mock.x
            , right = mkVar Mock.x
            , requires = makeTruePredicate
            , ensures = makeTruePredicate
            , attributes = def
            }
    axiomId = RewriteRule ruleId

    axiomSigmaId =
        RewriteRule RulePattern
            { left = Mock.sigma (mkVar Mock.x) (mkVar Mock.x)
            , right = mkVar Mock.x
            , requires = makeTruePredicate
            , ensures = makeTruePredicate
            , attributes = def
            }

    axiomSigmaXXYY =
        RewriteRule RulePattern
            { left =
                Mock.sigma
                    (Mock.sigma (mkVar Mock.x) (mkVar Mock.x))
                    (Mock.sigma (mkVar Mock.y) (mkVar Mock.y))
            , right = Mock.sigma (mkVar Mock.x) (mkVar Mock.y)
            , requires = makeTruePredicate
            , ensures = makeTruePredicate
            , attributes = def
            }

    axiomSigmaXXY =
        RewriteRule RulePattern
            { left =
                Mock.sigma
                    (Mock.sigma (mkVar Mock.x) (mkVar Mock.x))
                    (mkVar Mock.y)
            , right = Mock.sigma (mkVar Mock.x) (mkVar Mock.y)
            , requires = makeTruePredicate
            , ensures = makeTruePredicate
            , attributes = def
            }

-- | Apply the 'RewriteRule' to the configuration.
applyRewriteRule
    :: ExpandedPattern Object Variable
    -- ^ Configuration
    -> RewriteRule Object Variable
    -- ^ Rewrite rule
    -> IO
        (Either
            (StepError Object Variable)
            (OrStepResult Object Variable)
        )
applyRewriteRule initial rule =
    SMT.runSMT SMT.defaultConfig
    $ evalSimplifier emptyLogger
    $ runExceptT
    $ Step.applyRewriteRules
        metadataTools
        predicateSimplifier
        patternSimplifier
        axiomSimplifiers
        [rule]
        initial
  where
    metadataTools = mockMetadataTools
    predicateSimplifier =
        PredicateSubstitution.create
            metadataTools
            patternSimplifier
            axiomSimplifiers
    patternSimplifier =
        Simplifier.create
            metadataTools
            axiomSimplifiers
    axiomSimplifiers = Map.empty

test_applyRewriteRule :: [TestTree]
test_applyRewriteRule =
    [ testCase "if _ then _" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: if x then cg
        --   axiom: if true y => y
        -- Actual:
        --   term: constr20(x, cg)
        --   axiom: constr20(a, y) => y
        -- Expected:
        --   rewritten: cg, with ⌈cg⌉ and [x=a]
        --   remainder: constr20(x, cg), with ¬(⌈cg⌉ and x=a)
        let
            expect =
                Right OrStepResult
                    { rewrittenPattern =
                        [ Predicated
                            { term = Mock.cg
                            , predicate = makeCeilPredicate Mock.cg
                            , substitution =
                                Substitution.wrap [(Mock.x, Mock.a)]
                            }
                        ]
                    , remainder =
                        [ Predicated
                            { term = initialTerm
                            , predicate =
                                makeNotPredicate
                                $ makeCeilPredicate Mock.cg
                            , substitution = mempty
                            }
                        , Predicated
                            { term = initialTerm
                            , predicate =
                                makeNotPredicate
                                $ makeEqualsPredicate (mkVar Mock.x) Mock.a
                            , substitution = mempty
                            }
                        ]
                    }
            initialTerm = Mock.functionalConstr20 (mkVar Mock.x) Mock.cg
            initial = pure initialTerm
        actual <- applyRewriteRule initial axiomIfThen
        assertEqualWithExplanation "" expect actual

    , testCase "if _ then _ with initial condition" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: if x then cg
        --   axiom: if true y => y
        -- Actual:
        --   term: constr20(x, cg), with a ⌈cf⌉ predicate
        --   axiom: constr20(a, y) => y
        -- Expected:
        --   rewritten: cg, with ⌈cf⌉ and ⌈cg⌉ and [x=a]
        --   remainder: constr20(x, cg), with ⌈cf⌉ and ¬(⌈cg⌉ and x=a)
        let
            expect =
                Right OrStepResult
                    { rewrittenPattern =
                        [ Predicated
                            { term = Mock.cg
                            , predicate =
                                makeAndPredicate
                                    (makeCeilPredicate Mock.cf)
                                    (makeCeilPredicate Mock.cg)
                            , substitution =
                                Substitution.wrap
                                    [(Mock.x, Mock.a)]
                            }
                        ]
                    , remainder =
                        [ Predicated
                            { term =
                                Mock.functionalConstr20 (mkVar Mock.x) Mock.cg
                            , predicate =
                                makeAndPredicate (makeCeilPredicate Mock.cf)
                                $ makeNotPredicate
                                $ makeCeilPredicate Mock.cg
                            , substitution = mempty
                            }
                        , Predicated
                            { term =
                                Mock.functionalConstr20 (mkVar Mock.x) Mock.cg
                            , predicate =
                                makeAndPredicate (makeCeilPredicate Mock.cf)
                                $ makeNotPredicate
                                $ makeEqualsPredicate (mkVar Mock.x) Mock.a
                            , substitution = mempty
                            }
                        ]
                    }
            initialTerm = Mock.functionalConstr20 (mkVar Mock.x) Mock.cg
            initial =
                Predicated
                    { term = initialTerm
                    , predicate = makeCeilPredicate Mock.cf
                    , substitution = mempty
                    }
        actual <- applyRewriteRule initial axiomIfThen
        assertEqualWithExplanation "" expect actual

    , testCase "signum - side condition" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: signum(x)
        --   axiom: signum(y) => -1 if (y<0 == true)
        -- Actual:
        --   term: functionalConstr10(x)
        --   axiom: functionalConstr10(y) => a if f(y) == b
        -- Expected:
        --   rewritten: a, with f(x) == b
        --   remainder: functionalConstr10(x), with ¬(f(x) == b)
        let
            expect =
                Right OrStepResult
                    { rewrittenPattern =
                        [ Predicated
                            { term = Mock.a
                            , predicate = requirement
                            , substitution = mempty
                            }
                        ]
                    , remainder =
                        [ Predicated
                            { term = Mock.functionalConstr10 (mkVar Mock.x)
                            , predicate = makeNotPredicate requirement
                            , substitution = mempty
                            }
                        ]
                    }
            initial = pure (Mock.functionalConstr10 (mkVar Mock.x))
            requirement = makeEqualsPredicate (Mock.f (mkVar Mock.x)) Mock.b
        actual <- applyRewriteRule initial axiomSignum
        assertEqualWithExplanation "" expect actual

    , testCase "if _ then _ -- partial" $ do
        -- This uses `functionalConstr20(x, y)` instead of `if x then y`
        -- and `a` instead of `true`.
        --
        -- Intended:
        --   term: if x then cg
        --   axiom: if true y => y
        -- Actual:
        --   term: functionalConstr20(x, cg)
        --   axiom: functionalConstr20(a, y) => y
        -- Expected:
        --   rewritten: cg, with ⌈cg⌉ and [x=a]
        --   remainder: functionalConstr20(x, cg), with ¬(⌈cg⌉ and x=a)
        let
            expect =
                Right OrStepResult
                    { rewrittenPattern =
                        [ Predicated
                            { term = Mock.cg
                            , predicate = makeCeilPredicate Mock.cg
                            , substitution =
                                Substitution.wrap [(Mock.x, Mock.a)]
                            }
                        ]
                    , remainder =
                        [ initial
                            { predicate =
                                makeNotPredicate (makeCeilPredicate Mock.cg)
                            }
                        , initial
                            { predicate =
                                makeNotPredicate
                                $ makeEqualsPredicate (mkVar Mock.x) Mock.a
                            }
                        ]
                    }
            initialTerm = Mock.functionalConstr20 (mkVar Mock.x) Mock.cg
            initial = pure initialTerm
        actual <- applyRewriteRule initial axiomIfThen
        assertEqualWithExplanation "" expect actual
    ]
  where
    axiomIfThen =
        RewriteRule RulePattern
            { left = Mock.functionalConstr20 Mock.a (mkVar Mock.y)
            , right = mkVar Mock.y
            , requires = makeTruePredicate
            , ensures = makeTruePredicate
            , attributes = def
            }
    axiomSignum =
        RewriteRule RulePattern
            { left = Mock.functionalConstr10 (mkVar Mock.y)
            , right = Mock.a
            , requires = makeEqualsPredicate (Mock.f (mkVar Mock.y)) Mock.b
            , ensures = makeTruePredicate
            , attributes = def
            }
