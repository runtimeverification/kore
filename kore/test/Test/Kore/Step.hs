module Test.Kore.Step
where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Control.Exception as Exception
import           Data.Default
                 ( def )
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Data.Text
                 ( Text )
import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.Attribute.Symbol
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..), SmtMetadataTools )
import qualified Kore.IndexedModule.MetadataTools as HeadType
                 ( HeadType (..) )
import           Kore.Predicate.Predicate
                 ( makeTruePredicate )
import           Kore.Step
import           Kore.Step.Proof
                 ( StepProof )
import           Kore.Step.Representation.ExpandedPattern as ExpandedPattern
                 ( CommonExpandedPattern, Conditional (..), ExpandedPattern )
import           Kore.Step.Rule
                 ( RewriteRule (RewriteRule), RulePattern (RulePattern) )
import           Kore.Step.Rule as RulePattern
                 ( RulePattern (..) )
import           Kore.Step.Simplification.Data
                 ( Simplifier )
import           Kore.Step.Simplification.Data as Simplification
import           Kore.Step.TermLike

import qualified Kore.Step.Simplification.Simplifier as Simplifier
import qualified Kore.Step.Strategy as Strategy
import qualified SMT

import           Test.Kore
import           Test.Kore.Comparators ()
import qualified Test.Kore.Step.MockSimplifiers as Mock
import qualified Test.Kore.Step.MockSymbols as Mock
import           Test.Tasty.HUnit.Extensions

{-
    Tests of running a strategy by checking if the expected
    endpoint of the rewrites is achieved.

    These tests are concise examples of common situations.
    They are more integration tests than unit tests.
-}

-- DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER DANGER
-- The names of constructors ("c1", etc.) must match those in
-- `mockMetadataTools. `
test_constructorRewriting :: TestTree
test_constructorRewriting =
    applyStrategy                              "a constructor appied to a var"
        ( Start $ cons "c1" ["var"])
        [ Axiom $ cons "c1" ["x1"] `rewritesTo` cons "c2" ["x1"]
        , Axiom $ cons "c2" ["x2"] `rewritesTo` cons "c3" ["x2"]
        ]
        ( Expect $                              cons "c3" ["var"])
      where
        cons = applyConstructorToVariables


test_ruleThatDoesn'tApply :: TestTree
test_ruleThatDoesn'tApply =
    applyStrategy                              "unused rewrite rule"
        ( Start $ cons "c1"     ["var"])
        [ Axiom $ cons "c1"     ["x1"] `rewritesTo`  cons "c2" ["x1"]
        , Axiom $ cons "unused" ["x2"] `rewritesTo`  var "x2"
        ]
        ( Expect $                              cons "c2" ["var"])
      where
        cons = applyConstructorToVariables


        {- Test API -}

applyStrategy
    :: HasCallStack
    => TestName -> Start -> [Axiom] -> Expect -> TestTree
applyStrategy testName start axioms expected =
    testCase testName $
        takeSteps (start, axioms) >>= compareTo expected


        {- API Helpers -}

takeSteps :: (Start, [Axiom]) -> IO (Actual, Proof)
takeSteps (Start start, wrappedAxioms) =
    (<$>) pickLongest
    $ SMT.runSMT SMT.defaultConfig
    $ Simplification.evalSimplifier emptyLogger
    $ makeExecutionGraph start (unAxiom <$> wrappedAxioms)
  where
    makeExecutionGraph configuration axioms =
        Strategy.runStrategy
            mockTransitionRule
            (repeat $ allRewrites axioms)
            (pure configuration, mempty)

compareTo
    :: HasCallStack
    => Expect -> (Actual, Proof) -> IO ()
compareTo (Expect expected) (actual, _ignoredProof) =
    assertEqualWithExplanation "" (pure expected) actual


    {- Types used in this file -}

-- Isolate knowledge of `Object` in anticipation of its removal.
-- Should these go in some common location?
type RewriteRule' variable = RewriteRule Object variable
type TermLike' variable = TermLike variable
type CommonTermLike' = TermLike Variable
type ExpandedPattern' variable = ExpandedPattern Object variable
type CommonExpandedPattern' = CommonExpandedPattern Object
type Sort' = Sort Object
type StepProof' variable = StepProof Object variable

-- Test types
type TestPattern = CommonTermLike'
newtype Start = Start TestPattern
newtype Axiom = Axiom
    { unAxiom :: RewriteRule' Variable }
newtype Expect = Expect TestPattern

type Actual = ExpandedPattern' Variable
type Proof = StepProof' Variable


-- Useful constant values

anySort :: Sort'
anySort = sort "irrelevant"

mockTransitionRule
    :: Prim (RewriteRule' Variable)
    -> (CommonExpandedPattern', StepProof' Variable)
    -> Strategy.TransitionT
            (RewriteRule' Variable)
            Simplifier
            (CommonExpandedPattern', StepProof' Variable)
mockTransitionRule =
    transitionRule
        metadataTools
        substitutionSimplifier
        simplifier
        Map.empty
  where
    metadataTools = mockMetadataTools
    simplifier = Simplifier.create metadataTools Map.empty
    substitutionSimplifier =
        Mock.substitutionSimplifier metadataTools

-- Builders -- should these find a better home?

-- | Create a function pattern from a function name and list of argnames.
applyConstructorToVariables :: Text -> [Text] -> TestPattern
applyConstructorToVariables name arguments =
    mkApp anySort symbol $ fmap var arguments
  where
      symbol = SymbolOrAlias -- can this be more abstact?
        { symbolOrAliasConstructor = testId name
        , symbolOrAliasParams = []
        }

-- | Do the busywork of converting a name into a variable pattern.
var :: Text -> TestPattern
var name =
    mkVar $ (Variable (testId name) mempty) anySort
-- can the above be more abstract?

sort :: Text -> Sort'
sort name =
    SortActualSort $ SortActual
      { sortActualName = testId name
      , sortActualSorts = []
      }

rewritesTo :: TestPattern -> TestPattern -> RewriteRule' Variable
rewritesTo left right =
    RewriteRule $ RulePattern
        { left
        , right
        , requires = makeTruePredicate
        , ensures = makeTruePredicate
        , attributes = def
        }


{-

    The following tests are old and should eventually be rewritten.

    They should perhaps be rewritten to use individual Kore.Step functions
    like `rewriteStep`.
-}

v1, a1, b1, x1 :: Sort Meta -> Variable Meta
v1 = Variable (testId "#v1") mempty
a1 = Variable (testId "#a1") mempty
b1 = Variable (testId "#b1") mempty
x1 = Variable (testId "#x1") mempty

rewriteIdentity :: RewriteRule Meta Variable
rewriteIdentity =
    RewriteRule RulePattern
        { left = mkVar (x1 Mock.testSort)
        , right = mkVar (x1 Mock.testSort)
        , requires = makeTruePredicate
        , ensures = makeTruePredicate
        , attributes = def
        }

rewriteImplies :: RewriteRule Meta Variable
rewriteImplies =
    RewriteRule $ RulePattern
        { left = mkVar (x1 Mock.testSort)
        , right =
            mkImplies
                (mkVar $ x1 Mock.testSort)
                (mkVar $ x1 Mock.testSort)
        , requires = makeTruePredicate
        , ensures = makeTruePredicate
        , attributes = def
        }

expectTwoAxioms :: [(ExpandedPattern Meta Variable, StepProof Meta Variable)]
expectTwoAxioms =
    [   ( pure (mkVar $ v1 Mock.testSort), mempty )
    ,   ( Conditional
            { term =
                mkImplies
                    (mkVar $ v1 Mock.testSort)
                    (mkVar $ v1 Mock.testSort)
            , predicate = makeTruePredicate
            , substitution = mempty
            }
        , mempty
        )
    ]

actualTwoAxioms :: IO [(CommonExpandedPattern Meta, StepProof Meta Variable)]
actualTwoAxioms =
    runStep
        mockMetadataTools
        Conditional
            { term = mkVar (v1 Mock.testSort)
            , predicate = makeTruePredicate
            , substitution = mempty
            }
        [ rewriteIdentity
        , rewriteImplies
        ]

initialFailSimple :: ExpandedPattern Meta Variable
initialFailSimple =
    Conditional
        { term =
            metaSigma
                (metaG (mkVar $ a1 Mock.testSort))
                (metaF (mkVar $ b1 Mock.testSort))
        , predicate = makeTruePredicate
        , substitution = mempty
        }

expectFailSimple :: [(CommonExpandedPattern Meta, StepProof Meta Variable)]
expectFailSimple = [ (initialFailSimple, mempty) ]

actualFailSimple :: IO [(CommonExpandedPattern Meta, StepProof Meta Variable)]
actualFailSimple =
    runStep
        mockMetadataTools
        initialFailSimple
        [ RewriteRule $ RulePattern
            { left =
                metaSigma
                    (mkVar $ x1 Mock.testSort)
                    (mkVar $ x1 Mock.testSort)
            , right =
                mkVar (x1 Mock.testSort)
            , requires = makeTruePredicate
            , ensures = makeTruePredicate
            , attributes = def
            }
        ]

initialFailCycle :: ExpandedPattern Meta Variable
initialFailCycle =
    Conditional
        { term =
            metaSigma
                (mkVar $ a1 Mock.testSort)
                (mkVar $ a1 Mock.testSort)
        , predicate = makeTruePredicate
        , substitution = mempty
        }

expectFailCycle :: [(CommonExpandedPattern Meta, StepProof Meta Variable)]
expectFailCycle = [ (initialFailCycle, mempty) ]

actualFailCycle :: IO [(CommonExpandedPattern Meta, StepProof Meta Variable)]
actualFailCycle =
    runStep
        mockMetadataTools
        initialFailCycle
        [ RewriteRule $ RulePattern
            { left =
                metaSigma
                    (metaF (mkVar $ x1 Mock.testSort))
                    (mkVar $ x1 Mock.testSort)
            , right =
                mkVar (x1 Mock.testSort)
            , ensures = makeTruePredicate
            , requires = makeTruePredicate
            , attributes = def
            }
        ]

initialIdentity :: ExpandedPattern Meta Variable
initialIdentity =
    Conditional
        { term = mkVar (v1 Mock.testSort)
        , predicate = makeTruePredicate
        , substitution = mempty
        }

expectIdentity :: [(CommonExpandedPattern Meta, StepProof Meta Variable)]
expectIdentity = [ (initialIdentity, mempty) ]

actualIdentity :: IO [(CommonExpandedPattern Meta, StepProof Meta Variable)]
actualIdentity =
    runStep
        mockMetadataTools
        initialIdentity
        [ rewriteIdentity ]

test_stepStrategy :: [TestTree]
test_stepStrategy =
    [ testCase "Applies a simple axiom"
        -- Axiom: X1 => X1
        -- Start pattern: V1
        -- Expected: V1
        (assertEqualWithExplanation "" expectIdentity =<< actualIdentity)
    , testCase "Applies two simple axioms"
        -- Axiom: X1 => X1
        -- Axiom: X1 => implies(X1, X1)
        -- Start pattern: V1
        -- Expected: V1
        -- Expected: implies(V1, V1)
        (assertEqualWithExplanation "" expectTwoAxioms =<< actualTwoAxioms)
    , testCase "Fails to apply a simple axiom"      --- unification failure
        -- Axiom: sigma(X1, X1) => X1
        -- Start pattern: sigma(f(A1), g(B1))
        -- Expected: empty result list
        (assertEqualWithExplanation "" expectFailSimple =<< actualFailSimple)
    , testCase "Fails to apply a simple axiom due to cycle."  -- unification error constructor based vs
        -- Axiom: sigma(f(X1), X1) => X1
        -- Start pattern: sigma(A1, A1)
        -- Expected: empty result list
        (assertEqualWithExplanation "" expectFailCycle =<< actualFailCycle)
    ]


test_unificationError :: TestTree
test_unificationError =
    testCase "Throws unification error" $ do
        result <- Exception.try actualUnificationError
        case result of
            Left (Exception.ErrorCall _) -> return ()
            Right _ -> assertFailure "Expected unification error"

actualUnificationError
    :: IO [(CommonExpandedPattern Meta, StepProof Meta Variable)]
actualUnificationError =
    runStep
        mockMetadataTools
        Conditional
            { term =
                metaSigma
                    (mkVar $ a1 Mock.testSort)
                    (metaI (mkVar $ b1 Mock.testSort))
            , predicate = makeTruePredicate
            , substitution = mempty
            }
        [axiomMetaSigmaId]

mockSymbolAttributes :: SymbolOrAlias Meta -> StepperAttributes
mockSymbolAttributes patternHead =
    defaultSymbolAttributes
        { constructor = Constructor { isConstructor }
        , functional = Functional { isDeclaredFunctional }
        , function = Function { isDeclaredFunction }
        , injective = Injective { isDeclaredInjective }
        }
  where
    isConstructor = patternHead /= iSymbol
    isDeclaredFunctional = patternHead /= iSymbol
    isDeclaredFunction = patternHead /= iSymbol
    isDeclaredInjective = patternHead /= iSymbol

mockMetadataTools :: SmtMetadataTools StepperAttributes
mockMetadataTools = MetadataTools
    { symAttributes = mockSymbolAttributes
    , symbolOrAliasType = const HeadType.Symbol
    , sortAttributes = const def
    , isSubsortOf = const $ const False
    , subsorts = Set.singleton
    , applicationSorts = undefined
    , smtData = undefined
    }

sigmaSymbol :: SymbolOrAlias Meta
sigmaSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = testId "#sigma"
    , symbolOrAliasParams = []
    }

metaSigma
    :: TermLike Variable
    -> TermLike Variable
    -> TermLike Variable
metaSigma p1 p2 = mkApp Mock.testSort sigmaSymbol [p1, p2]

axiomMetaSigmaId :: RewriteRule Meta Variable
axiomMetaSigmaId =
    RewriteRule RulePattern
        { left =
            metaSigma
                (mkVar $ x1 Mock.testSort)
                (mkVar $ x1 Mock.testSort)
        , right =
            mkVar $ x1 Mock.testSort
        , requires = makeTruePredicate
        , ensures = makeTruePredicate
        , attributes = def
        }


fSymbol :: SymbolOrAlias Meta
fSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#f" AstLocationTest
    , symbolOrAliasParams = []
    }

metaF
    :: TermLike Variable
    -> TermLike Variable
metaF p = mkApp Mock.testSort fSymbol [p]


gSymbol :: SymbolOrAlias Meta
gSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#g" AstLocationTest
    , symbolOrAliasParams = []
    }

metaG
    :: TermLike Variable
    -> TermLike Variable
metaG p = mkApp Mock.testSort gSymbol [p]


hSymbol :: SymbolOrAlias Meta
hSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#h" AstLocationTest
    , symbolOrAliasParams = []
    }

metaH
    :: TermLike Variable
    -> TermLike Variable
metaH p = mkApp Mock.testSort hSymbol [p]

iSymbol :: SymbolOrAlias Meta
iSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#i" AstLocationTest
    , symbolOrAliasParams = []
    }

metaI
    :: TermLike Variable
    -> TermLike Variable
metaI p = mkApp Mock.testSort iSymbol [p]

runStep
    :: MetaOrObject level
    => SmtMetadataTools StepperAttributes
    -- ^functions yielding metadata for pattern heads
    -> CommonExpandedPattern level
    -- ^left-hand-side of unification
    -> [RewriteRule level Variable]
    -> IO [(CommonExpandedPattern level, StepProof level Variable)]
runStep metadataTools configuration axioms =
    (<$>) pickFinal
    $ SMT.runSMT SMT.defaultConfig
    $ Simplification.evalSimplifier emptyLogger
    $ runStrategy
        (transitionRule
            metadataTools
            (Mock.substitutionSimplifier metadataTools)
            simplifier
            Map.empty
        )
        [allRewrites axioms]
        (configuration, mempty)
  where
    simplifier = Simplifier.create metadataTools Map.empty

runSteps
    :: MetaOrObject level
    => SmtMetadataTools StepperAttributes
    -- ^functions yielding metadata for pattern heads
    -> CommonExpandedPattern level
    -- ^left-hand-side of unification
    -> [RewriteRule level Variable]
    -> IO (CommonExpandedPattern level, StepProof level Variable)
runSteps metadataTools configuration axioms =
    (<$>) pickLongest
    $ SMT.runSMT SMT.defaultConfig
    $ Simplification.evalSimplifier emptyLogger
    $ runStrategy
        (transitionRule
            metadataTools
            (Mock.substitutionSimplifier metadataTools)
            simplifier
            Map.empty
        )
        (repeat $ allRewrites axioms)
        (configuration, mempty)
  where
    simplifier = Simplifier.create metadataTools Map.empty
