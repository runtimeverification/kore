module Test.Kore.Unification.Unifier (
    test_unification,
    test_unsupportedConstructs,
) where

import Control.Exception (
    ErrorCall (ErrorCall),
    catch,
    evaluate,
 )
import Data.Bifunctor qualified as Bifunctor
import Data.Text (
    Text,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Internal.SideCondition qualified as SideCondition (
    top,
    topTODO,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Pattern qualified as Pattern
import Kore.Simplify.Simplify (
    Env (..),
    runSimplifier,
 )
import Kore.Simplify.SubstitutionSimplifier qualified as SubstitutionSimplifier
import Kore.Unification.Procedure
import Kore.Unification.SubstitutionSimplifier qualified as Unification
import Kore.Unification.UnifierT qualified as Monad.Unify
import Kore.Unparser
import Prelude.Kore
import Pretty qualified
import Test.Kore
import Test.Kore.Internal.Pattern as Pattern
import Test.Kore.Internal.Predicate qualified as Predicate
import Test.Kore.Internal.Substitution qualified as Substitution
import Test.Kore.Internal.TermLike qualified as TermLike
import Test.Kore.Rewrite.MockSymbols qualified as Mock
import Test.Kore.Variables.V
import Test.Kore.Variables.W
import Test.SMT (
    runNoSMT,
 )
import Test.Tasty
import Test.Tasty.HUnit.Ext

var :: Text -> Sort -> Mock.MockRewritingElementVariable
var name variableSort =
    Mock.mkConfigElementVariable (testId name) mempty variableSort

a1, a2, a3, a4, a5 :: TermLike RewritingVariableName
a1 = Mock.c
a2 = TermLike.markSimplified Mock.functional00
a3 = Mock.constr00
a4 = TermLike.markSimplified Mock.functionalInjective00
a5 = TermLike.markSimplified Mock.cf

a :: TermLike RewritingVariableName
a = Mock.a

b :: TermLike RewritingVariableName
b = Mock.b

f :: TermLike RewritingVariableName -> TermLike RewritingVariableName
f = Mock.constr10

ef ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
ef = Mock.functionalConstr30

eg, eh :: TermLike RewritingVariableName -> TermLike RewritingVariableName
eg = Mock.functionalConstr10
eh = Mock.functionalConstr11

nonLinF ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
nonLinF = Mock.functionalConstr20

nonLinG :: TermLike RewritingVariableName -> TermLike RewritingVariableName
nonLinG = Mock.functionalConstr12

nonLinA, nonLinX, nonLinY :: TermLike RewritingVariableName
nonLinA = Mock.d
nonLinX = mkElemVar Mock.xConfig
nonLinY = mkElemVar Mock.yConfig

expBin ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
expBin = Mock.functionalConstr21

expA, expX, expY :: TermLike RewritingVariableName
expA = mkElemVar $ var "a" Mock.testSort
expX = mkElemVar $ var "x" Mock.testSort
expY = mkElemVar $ var "y" Mock.testSort

ex1, ex2, ex3, ex4 :: TermLike RewritingVariableName
ex1 = mkElemVar $ var "ex1" Mock.testSort
ex2 = mkElemVar $ var "ex2" Mock.testSort
ex3 = mkElemVar $ var "ex3" Mock.testSort
ex4 = mkElemVar $ var "ex4" Mock.testSort

dv1, dv2 :: TermLike RewritingVariableName
dv1 =
    mkDomainValue
        DomainValue
            { domainValueSort = Mock.testSort
            , domainValueChild = mkStringLiteral "dv1"
            }
dv2 =
    mkDomainValue
        DomainValue
            { domainValueSort = Mock.testSort
            , domainValueChild = mkStringLiteral "dv2"
            }

testEnv :: Env
testEnv = Mock.env

unificationProblem ::
    UnificationTerm ->
    UnificationTerm ->
    TermLike RewritingVariableName
unificationProblem (UnificationTerm term1) (UnificationTerm term2) =
    mkAnd term1 term2

type Substitution = [(Text, TermLike RewritingVariableName)]

unificationSubstitution ::
    Substitution -> [Substitution.Assignment RewritingVariableName]
unificationSubstitution = map trans
  where
    trans (v, p) =
        Substitution.assign (Mock.makeSomeConfigVariable v (termLikeSort p)) p

unificationResult :: UnificationResult -> Pattern RewritingVariableName
unificationResult
    UnificationResult{term, substitution, predicate} =
        unificationSubstitution substitution
            & Substitution.unsafeWrapFromAssignments
            & Condition.fromSubstitution
            & (<>) (Condition.fromPredicate predicate)
            & Pattern.withCondition term

newtype UnificationTerm = UnificationTerm (TermLike RewritingVariableName)

instance Unparse UnificationTerm where
    unparse (UnificationTerm term) = unparse term
    unparse2 (UnificationTerm term) = unparse2 term

data UnificationResult = UnificationResult
    { term :: TermLike RewritingVariableName
    , substitution :: Substitution
    , predicate :: Predicate RewritingVariableName
    }

simplifyAnds ::
    Monad.Unify.MonadUnify unifier =>
    NonEmpty (TermLike RewritingVariableName) ->
    unifier (Pattern RewritingVariableName)
simplifyAnds =
    SubstitutionSimplifier.simplifyAnds
        Unification.unificationMakeAnd
        SideCondition.top

andSimplify ::
    HasCallStack =>
    UnificationTerm ->
    UnificationTerm ->
    [UnificationResult] ->
    Assertion
andSimplify term1 term2 results = do
    let expect = OrPattern.fromPatterns $ map unificationResult results
    subst' <-
        simplifyAnds (unificationProblem term1 term2 :| [])
            & Monad.Unify.runUnifierT
            & runSimplifier testEnv
            & runNoSMT
            & fmap OrPattern.fromPatterns
    assertEqual (message expect subst') expect subst'
  where
    message expected actual =
        (show . Pretty.vsep)
            [ "Unifying term:"
            , Pretty.indent 4 (unparse term1)
            , "with term:"
            , Pretty.indent 4 (unparse term2)
            , "expected:"
            , Pretty.indent 4 (unparseOrPattern expected)
            , "actual:"
            , Pretty.indent 4 (unparseOrPattern actual)
            ]
    unparseOrPattern = Pretty.vsep . map unparse . OrPattern.toPatterns

andSimplifyException ::
    HasCallStack =>
    String ->
    UnificationTerm ->
    UnificationTerm ->
    String ->
    TestTree
andSimplifyException message term1 term2 exceptionMessage =
    testCase message (catch test handler)
  where
    test = do
        assignment <-
            runNoSMT $
                runSimplifier testEnv $
                    Monad.Unify.runUnifierT $
                        simplifyAnds (unificationProblem term1 term2 :| [])
        _ <- evaluate assignment
        assertFailure "This evaluation should fail"
    handler (ErrorCall s) = assertEqual "" exceptionMessage s

unificationProcedureSuccess ::
    HasCallStack =>
    TestName ->
    UnificationTerm ->
    UnificationTerm ->
    [(Substitution, Predicate RewritingVariableName)] ->
    TestTree
unificationProcedureSuccess
    message
    (UnificationTerm term1)
    (UnificationTerm term2)
    substPredicate =
        testCase message $ do
            let mockEnv = testEnv
            results <-
                runNoSMT
                    . runSimplifier mockEnv
                    . runUnifier
                    $ unificationProcedure
                        SideCondition.topTODO
                        term1
                        term2
            let normalize ::
                    Condition RewritingVariableName ->
                    ( [Substitution.Assignment RewritingVariableName]
                    , Predicate RewritingVariableName
                    )
                normalize Conditional{substitution, predicate} =
                    (Substitution.unwrap substitution, predicate)
            assertEqual
                ""
                expect
                (map normalize results)
      where
        expect ::
            [ ( [Substitution.Assignment RewritingVariableName]
              , Predicate RewritingVariableName
              )
            ]
        expect =
            map (Bifunctor.first unificationSubstitution) substPredicate

test_unification :: [TestTree]
test_unification =
    [ testCase "Constant" $
        andSimplify
            (UnificationTerm a)
            (UnificationTerm a)
            [ UnificationResult
                { term = a
                , substitution = []
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "Variable" $
        andSimplify
            (UnificationTerm (mkElemVar Mock.xConfig))
            (UnificationTerm a)
            [ UnificationResult
                { term = a
                , substitution = [("x", a)]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "one level" $
        andSimplify
            (UnificationTerm (f (mkElemVar Mock.xConfig)))
            (UnificationTerm (f a))
            [ UnificationResult
                { term = f a
                , substitution = [("x", a)]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "equal non-constructor patterns" $
        andSimplify
            (UnificationTerm a2)
            (UnificationTerm a2)
            [ UnificationResult
                { term = a2
                , substitution = []
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "variable + non-constructor pattern" $
        andSimplify
            (UnificationTerm a2)
            (UnificationTerm (mkElemVar Mock.xConfig))
            [ UnificationResult
                { term = a2
                , substitution = [("x", a2)]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "https://basics.sjtu.edu.cn/seminars/c_chu/Algorithm.pdf slide 3" $
        andSimplify
            (UnificationTerm (ef ex1 (eh ex1) ex2))
            (UnificationTerm (ef (eg ex3) ex4 ex3))
            [ UnificationResult
                { term = ef (eg ex3) (eh ex1) ex3
                , substitution =
                    [ ("ex1", eg ex3)
                    , ("ex2", ex3)
                    , ("ex4", eh (eg ex3))
                    ]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase
        "f(g(X),X) = f(Y,a) https://en.wikipedia.org/wiki/Unification_(computer_science)#Examples_of_syntactic_unification_of_first-order_terms"
        $ andSimplify
            (UnificationTerm (nonLinF (nonLinG nonLinX) nonLinX))
            (UnificationTerm (nonLinF nonLinY nonLinA))
            [ UnificationResult
                { term = nonLinF (nonLinG nonLinX) nonLinA
                , substitution =
                    [ ("x", nonLinA)
                    , ("y", nonLinG nonLinA)
                    ]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "times(times(a, y), (mkElemVar Mock.x)) = times(x, times(y, a))" $
        andSimplify
            (UnificationTerm (expBin (expBin expA expY) expX))
            (UnificationTerm (expBin expX (expBin expY expA)))
            [ UnificationResult
                { term = expBin (expBin expA expY) (expBin expY expA)
                , substitution =
                    [ ("a", expY)
                    , ("x", expBin expY expY)
                    ]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , unificationProcedureSuccess
        "times(x, g(x)) = times(a, a) -- cycle bottom"
        (UnificationTerm (expBin expX (eg expX)))
        (UnificationTerm (expBin expA expA))
        []
    , unificationProcedureSuccess
        "times(times(a, y), (mkElemVar Mock.x)) = times(x, times(y, a))"
        (UnificationTerm (expBin (expBin expA expY) expX))
        (UnificationTerm (expBin expX (expBin expY expA)))
        [
            (
                [ ("a", expY)
                , ("x", expBin expY expY)
                ]
            , Predicate.makeTruePredicate
            )
        ]
    , unificationProcedureSuccess
        "Unifying two non-ctors results in equals predicate"
        (UnificationTerm a2)
        (UnificationTerm a4)
        [([], makeEqualsPredicate a2 a4)]
    , unificationProcedureSuccess
        "Unifying function and variable results in ceil predicate"
        (UnificationTerm (mkElemVar Mock.xConfig))
        (UnificationTerm a5)
        [
            ( [("x", a5)]
            , Predicate.makeCeilPredicate a5
            )
        ]
    , testGroup "inj unification tests" injUnificationTests
    , testCase "Unmatching constants is bottom" $
        andSimplify
            (UnificationTerm a)
            (UnificationTerm a1)
            []
    , testCase "Unmatching domain values is bottom" $
        andSimplify
            (UnificationTerm dv1)
            (UnificationTerm dv2)
            []
    , andSimplifyException
        "Unmatching constructor constant + domain value"
        (UnificationTerm a)
        (UnificationTerm dv2)
        "Cannot handle Constructor and DomainValue:\n\
        \a{}()\n\
        \\\dv{testSort{}}(\"dv2\")\n"
    , andSimplifyException
        "Unmatching domain value + constructor constant"
        (UnificationTerm dv1)
        (UnificationTerm a)
        "Cannot handle DomainValue and Constructor:\n\
        \\\dv{testSort{}}(\"dv1\")\n\
        \a{}()\n"
    , testCase "Unmatching domain value + nonconstructor constant" $
        andSimplify
            (UnificationTerm dv1)
            (UnificationTerm a2)
            [ UnificationResult
                { term = dv1
                , substitution = []
                , predicate = Predicate.makeEqualsPredicate dv1 a2
                }
            ]
    , testCase "Unmatching nonconstructor constant + domain value" $
        andSimplify
            (UnificationTerm a2)
            (UnificationTerm dv1)
            [ UnificationResult
                { term = dv1
                , substitution = []
                , predicate = Predicate.makeEqualsPredicate dv1 a2
                }
            ]
    , testCase "non-functional pattern" $
        andSimplify
            (UnificationTerm (mkElemVar Mock.xConfig))
            (UnificationTerm a3)
            [ UnificationResult
                { term = mkAnd (mkElemVar Mock.xConfig) a3
                , substitution = []
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "SetVariable w. constructor" $
        andSimplify
            (UnificationTerm (f (Mock.mkTestSomeConfigVariable "@x")))
            (UnificationTerm (f a))
            [ UnificationResult
                { term =
                    f (mkAnd (Mock.mkTestSomeConfigVariable "@x") a)
                , substitution = []
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "SetVariable" $
        andSimplify
            (UnificationTerm (Mock.mkTestSomeConfigVariable "@x"))
            (UnificationTerm a)
            [ UnificationResult
                { term =
                    mkAnd (Mock.mkTestSomeConfigVariable "@x") a
                , substitution = []
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "non-constructor symbolHead right" $
        andSimplify
            (UnificationTerm a)
            (UnificationTerm a2)
            [ UnificationResult
                { term = a
                , substitution = []
                , predicate = Predicate.makeEqualsPredicate a a2
                }
            ]
    , testCase "non-constructor symbolHead left" $
        andSimplify
            (UnificationTerm a2)
            (UnificationTerm a)
            [ UnificationResult
                { term = a
                , substitution = []
                , predicate = Predicate.makeEqualsPredicate a a2
                }
            ]
    , testCase "nested a=a1 is bottom" $
        andSimplify
            (UnificationTerm (f a))
            (UnificationTerm (f a1))
            []
    , {- currently this cannot even be built because of builder checks
      , andSimplifyFailure "Unmatching sorts"
          (UnificationTerm aA)
          (UnificationTerm bA)
          UnificationError
          -}
      testCase
        "Maps substitution variables"
        ( assertEqual
            ""
            ( Substitution.mkUnwrappedSubstitution
                [(inject . ElementVariableName <$> mkW "1", war' "2")]
            )
            ( Substitution.unwrap
                . Substitution.mapVariables showUnifiedVar
                . Substitution.wrap
                . Substitution.mkUnwrappedSubstitution
                $ [(inject . ElementVariableName <$> mkV 1, var' 2)]
            )
        )
    , testCase "framed Map with concrete Map" $
        andSimplify
            ( UnificationTerm
                ( Mock.concatMap
                    (Mock.builtinMap [(a, mkElemVar Mock.xConfig)])
                    (mkElemVar Mock.mConfig)
                )
            )
            ( UnificationTerm
                (Mock.builtinMap [(a, constr a), (b, constr b)])
            )
            [ UnificationResult
                { term =
                    Mock.builtinMap [(a, constr a), (b, constr b)]
                , predicate = Predicate.makeTruePredicate
                , substitution =
                    [ ("x", constr a)
                    , ("m", Mock.builtinMap [(Mock.b, constr b)])
                    ]
                }
            ]
    , testCase "key outside of map" $
        andSimplify
            ( UnificationTerm
                ( constr20
                    y
                    ( Mock.concatMap
                        (Mock.builtinMap [(y, x)])
                        (mkElemVar Mock.mConfig)
                    )
                )
            )
            ( UnificationTerm
                ( constr20
                    Mock.a
                    (Mock.builtinMap [(a, constr a), (b, constr b)])
                )
            )
            [ UnificationResult
                { term =
                    constr20
                        Mock.a
                        (Mock.builtinMap [(a, constr a), (b, constr b)])
                , predicate = Predicate.makeTruePredicate
                , substitution =
                    [ ("x", constr a)
                    , ("y", a)
                    , ("m", Mock.builtinMap [(Mock.b, constr b)])
                    ]
                }
            ]
    , testCase "key outside of map, symbolic opaque terms" $
        andSimplify
            ( UnificationTerm
                ( constr20
                    y
                    ( Mock.concatMap
                        (Mock.builtinMap [(y, x)])
                        (mkElemVar Mock.mConfig)
                    )
                )
            )
            ( UnificationTerm
                ( constr20
                    Mock.a
                    ( Mock.concatMap
                        (Mock.builtinMap [(a, constr a)])
                        (mkElemVar Mock.xMapConfig)
                    )
                )
            )
            [ UnificationResult
                { term =
                    constr20
                        Mock.a
                        ( Mock.concatMap
                            (Mock.builtinMap [(a, constr a)])
                            (mkElemVar Mock.xMapConfig)
                        )
                , predicate =
                    Predicate.makeAndPredicate
                        ( Predicate.makeNotPredicate
                            ( Predicate.makeCeilPredicate
                                ( Mock.concatMap
                                    (Mock.builtinMap [(a, constr a)])
                                    (mkElemVar Mock.xMapConfig)
                                )
                            )
                        )
                        ( Predicate.makeNotPredicate
                            ( Predicate.makeCeilPredicate
                                ( Mock.concatMap
                                    (Mock.builtinMap [(a, x)])
                                    (mkElemVar Mock.mConfig)
                                )
                            )
                        )
                , substitution = [("y", a)]
                }
            , UnificationResult
                { term =
                    constr20
                        Mock.a
                        ( Mock.concatMap
                            (Mock.builtinMap [(a, constr a)])
                            (mkElemVar Mock.xMapConfig)
                        )
                , predicate = Predicate.makeTruePredicate
                , substitution =
                    [ ("x", constr a)
                    , ("y", a)
                    , ("m", mkElemVar Mock.xMapConfig)
                    ]
                }
            ]
    ]
  where
    constr = Mock.functionalConstr10
    constr20 = Mock.constrFunct20TestMap
    x = mkElemVar Mock.xConfig
    y = mkElemVar Mock.yConfig

test_unsupportedConstructs :: TestTree
test_unsupportedConstructs =
    testCase "Unsupported constructs" $
        andSimplify
            (UnificationTerm (f a))
            (UnificationTerm (f (mkImplies a (mkNext a1))))
            [ UnificationResult
                { term =
                    f (mkAnd a (mkImplies a (mkNext a1)))
                , substitution = []
                , predicate = Predicate.makeTruePredicate
                }
            ]

injUnificationTests :: [TestTree]
injUnificationTests =
    [ testCase "Injected Variable" $
        andSimplify
            ( UnificationTerm
                (Mock.sortInjectionSubToTop (mkElemVar Mock.xConfigSubSort))
            )
            (UnificationTerm (Mock.sortInjectionSubToTop Mock.aSubsort))
            [ UnificationResult
                { term = Mock.sortInjectionSubToTop Mock.aSubsort
                , substitution = [("xSubSort", Mock.aSubsort)]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "Variable" $
        andSimplify
            (UnificationTerm (mkElemVar Mock.xConfigTopSort))
            (UnificationTerm (Mock.sortInjectionSubToTop Mock.aSubsort))
            [ UnificationResult
                { term = Mock.sortInjectionSubToTop Mock.aSubsort
                , substitution =
                    [("xTopSort", Mock.sortInjectionSubToTop Mock.aSubsort)]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "Injected Variable vs doubly injected term" $ do
        term2 <-
            simplifyPattern $
                UnificationTerm
                    ( Mock.sortInjectionSubToTop
                        (Mock.sortInjectionSubSubToSub Mock.aSubSubsort)
                    )
        andSimplify
            ( UnificationTerm
                (Mock.sortInjectionSubSubToTop (mkElemVar Mock.xConfigSubSubSort))
            )
            term2
            [ UnificationResult
                { term = Mock.sortInjectionSubSubToTop Mock.aSubSubsort
                , substitution = [("xSubSubSort", Mock.aSubSubsort)]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "doubly injected variable vs injected term" $ do
        term1 <-
            simplifyPattern $
                UnificationTerm
                    ( Mock.sortInjectionSubToTop
                        ( Mock.sortInjectionSubSubToSub
                            (mkElemVar Mock.xConfigSubSubSort)
                        )
                    )
        andSimplify
            term1
            (UnificationTerm (Mock.sortInjectionSubSubToTop Mock.aSubSubsort))
            [ UnificationResult
                { term = Mock.sortInjectionSubSubToTop Mock.aSubSubsort
                , substitution = [("xSubSubSort", Mock.aSubSubsort)]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "doubly injected variable vs doubly injected term" $ do
        term1 <-
            simplifyPattern $
                UnificationTerm
                    ( Mock.sortInjectionSubToTop
                        (Mock.sortInjectionSubSubToSub Mock.aSubSubsort)
                    )
        term2 <-
            simplifyPattern $
                UnificationTerm
                    ( Mock.sortInjectionOtherToTop
                        ( Mock.sortInjectionSubSubToOther
                            (mkElemVar Mock.xConfigSubSubSort)
                        )
                    )
        andSimplify
            term1
            term2
            [ UnificationResult
                { term = Mock.sortInjectionSubSubToTop Mock.aSubSubsort
                , substitution = [("xSubSubSort", Mock.aSubSubsort)]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "constant vs injection is bottom" $
        andSimplify
            (UnificationTerm Mock.aTopSort)
            (UnificationTerm (Mock.sortInjectionSubSubToTop Mock.aSubSubsort))
            []
    , testCase "unmatching nested injections" $ do
        term1 <-
            simplifyPattern $
                UnificationTerm
                    ( Mock.sortInjectionSubToTop
                        (Mock.sortInjectionSubSubToSub Mock.aSubSubsort)
                    )
        term2 <-
            simplifyPattern $
                UnificationTerm
                    ( Mock.sortInjectionOtherToTop
                        (Mock.sortInjectionSubOtherToOther Mock.aSubOthersort)
                    )
        andSimplify
            term1
            term2
            []
    , testCase "matching injections" $
        andSimplify
            (UnificationTerm (Mock.sortInjectionSubSubToTop Mock.aSubSubsort))
            ( UnificationTerm
                (Mock.sortInjectionSubToTop (mkElemVar Mock.xConfigSubSort))
            )
            [ UnificationResult
                { term = Mock.sortInjectionSubSubToTop Mock.aSubSubsort
                , substitution =
                    [
                        ( "xSubSort"
                        , Mock.sortInjectionSubSubToSub Mock.aSubSubsort
                        )
                    ]
                , predicate = Predicate.makeTruePredicate
                }
            ]
    , testCase "unmatching injections" $
        andSimplify
            (UnificationTerm (Mock.sortInjectionOtherToTop Mock.aOtherSort))
            ( UnificationTerm
                (Mock.sortInjectionSubToTop (mkElemVar Mock.xConfigSubSort))
            )
            []
    ]

simplifyPattern :: UnificationTerm -> IO UnificationTerm
simplifyPattern (UnificationTerm term) = do
    Conditional{term = term'} <- runNoSMT $ runSimplifier testEnv simplifier
    return $ UnificationTerm term'
  where
    simplifier = do
        simplifiedPatterns <-
            Pattern.simplify expandedPattern
        case toList simplifiedPatterns of
            [] -> return (Pattern.bottomOf Mock.testSort)
            (config : _) -> return config
    expandedPattern = Pattern.fromTermLike term

makeEqualsPredicate ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Predicate RewritingVariableName
makeEqualsPredicate = Predicate.makeEqualsPredicate
