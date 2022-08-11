module Test.Kore.Equation.Application (
    test_attemptEquation,
    test_applySubstitutionAndSimplify,
) where

import Control.Monad (
    (>=>),
 )
import Control.Monad.Trans.Except (
    runExceptT,
 )
import Data.Map.Strict qualified as Map
import Data.Text (
    Text,
 )
import Kore.Equation qualified as Equation
import Kore.Equation.Application hiding (
    attemptEquation,
 )
import Kore.Equation.DebugEquation
import Kore.Equation.Equation
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.Pattern as Pattern
import Kore.Rewrite.Axiom.Identifier qualified as AxiomIdentifier
import Kore.Rewrite.Axiom.Registry (
    mkEvaluatorRegistry,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Unparser (
    unparse,
 )
import Prelude.Kore
import Pretty qualified
import Test.Expect
import Test.Kore.Equation.Common
import Test.Kore.Internal.Pattern as Pattern
import Test.Kore.Internal.Predicate as Predicate
import Test.Kore.Internal.SideCondition as SideCondition
import Test.Kore.Rewrite.MockSymbols qualified as Mock
import Test.Kore.Simplify
import Test.Tasty
import Test.Tasty.HUnit.Ext

type AttemptEquationError' = AttemptEquationError RewritingVariableName
type AttemptEquationResult' = AttemptEquationResult RewritingVariableName

attemptEquation ::
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    Equation' ->
    IO AttemptEquationResult'
attemptEquation sideCondition termLike equation =
    Equation.attemptEquation sideCondition termLike equation
        & runSimplifierSMT Mock.env

assertNotMatched :: AttemptEquationError' -> Assertion
assertNotMatched (WhileMatch _) = return ()
assertNotMatched result =
    (assertFailure . show . Pretty.vsep)
        [ "Expected (WhileMatch _), but found:"
        , Pretty.indent 4 (debug result)
        ]

assertApplyMatchResultErrors :: AttemptEquationError' -> Assertion
assertApplyMatchResultErrors (WhileApplyMatchResult _) = return ()
assertApplyMatchResultErrors result =
    (assertFailure . show . Pretty.vsep)
        [ "Expected (WhileApplyMatch _), but found:"
        , Pretty.indent 4 (debug result)
        ]

assertRequiresNotMet :: AttemptEquationError' -> Assertion
assertRequiresNotMet (WhileCheckRequires _) = return ()
assertRequiresNotMet result =
    (assertFailure . show . Pretty.vsep)
        [ "Expected (RequiresNotMet _ _), but found:"
        , Pretty.indent 4 (debug result)
        ]

test_attemptEquation :: [TestTree]
test_attemptEquation =
    [ applies
        "applies identity axiom"
        (axiom_ x x)
        SideCondition.top
        x
        (Pattern.fromTermLike x)
    , applies
        "applies identity without renaming"
        (axiom_ x x)
        SideCondition.top
        y
        (Pattern.fromTermLike y)
    , applies
        "Σ(X, X) => X applies to Σ(f(X), f(X))"
        (axiom_ (sigma x x) x)
        SideCondition.top
        (sigma (f x) (f x))
        (Pattern.fromTermLike $ f x)
    , notMatched
        "merge configuration patterns"
        (axiom_ (sigma x x) x)
        SideCondition.top
        (sigma x (f x))
    , notMatched
        "substitution with symbol matching"
        (axiom_ (sigma x x) x)
        SideCondition.top
        (sigma (f y) (f z))
    , notMatched
        "merge multiple variables"
        (axiom_ (sigma (sigma x x) (sigma y y)) (sigma x y))
        SideCondition.top
        (sigma (sigma x y) (sigma y x))
    , notMatched
        "symbol clash"
        (axiom_ (sigma x x) x)
        SideCondition.top
        (sigma (f x) (g x))
    , notMatched
        "impossible substitution"
        (axiom_ (sigma (sigma x x) (sigma y y)) (sigma x y))
        SideCondition.top
        (sigma (sigma x (f y)) (sigma x y))
    , notMatched
        "circular dependency error"
        (axiom_ (sigma x x) x)
        SideCondition.top
        (sigma x (f x))
    , notMatched
        "non-function substitution error"
        (axiom_ (sigma x x) x)
        SideCondition.top
        (sigma x (f y))
    , notMatched
        "unify all children"
        (axiom_ (sigma x x) x)
        SideCondition.top
        (sigma (sigma x x) (sigma (sigma y z) (sigma y y)))
    , notMatched
        "normalize substitution"
        (axiom_ (sigma (sigma x x) y) (sigma x y))
        SideCondition.top
        (sigma (sigma x (f b)) x)
    , notMatched
        "merge substitution with initial"
        (axiom_ (sigma (sigma x x) y) (sigma x y))
        SideCondition.top
        (sigma (sigma (f z) (f y)) (f z))
    , notMatched
        "unmatched strings"
        (axiom_ (string "Good-bye, world!") xString)
        SideCondition.top
        (string "Hello, world!")
    , testCase "conjoin rule ensures" $ do
        let ensures =
                makeEqualsPredicate
                    (Mock.functional11 (mkElemVar Mock.xConfig))
                    (Mock.functional10 (mkElemVar Mock.xConfig))
            expect =
                Pattern.withCondition initial $
                    Condition.fromPredicate $
                        makeEqualsPredicate
                            (Mock.functional11 (mkElemVar Mock.yConfig))
                            (Mock.functional10 (mkElemVar Mock.yConfig))
            initial = mkElemVar Mock.yConfig
            equation = equationId{ensures}
        actual <-
            attemptEquation SideCondition.top initial equation
                >>= expectRight
        let message =
                (show . Pretty.vsep)
                    [ "Expected:"
                    , Pretty.indent 4 (unparse expect)
                    , "but found:"
                    , Pretty.indent 4 (unparse actual)
                    ]
        assertEqual message expect actual
    , testCase "equation requirement" $ do
        let requires =
                makeEqualsPredicate
                    (Mock.functional10 (mkElemVar Mock.xConfig))
                    (Mock.functional11 (mkElemVar Mock.xConfig))
            equation = equationId{requires}
            initial = Mock.a
        let requires1 =
                makeEqualsPredicate
                    (Mock.functional10 Mock.a)
                    (Mock.functional11 Mock.a)
        checkRequiresError <-
            attemptEquation SideCondition.top initial equation
                >>= expectLeft
                >>= expectCheckRequiresError
        assertEqual "" requires1 (equationRequires checkRequiresError)
        let requires2 =
                makeEqualsPredicate
                    (Mock.functional10 Mock.a)
                    (Mock.functional11 Mock.a)
            sideCondition2 =
                SideCondition.fromPredicateWithReplacements requires2
            expect2 = Pattern.fromTermLike initial
        attemptEquation sideCondition2 initial equation
            >>= expectRight
            >>= assertEqual "" expect2
    , testCase "rule a => \\bottom" $ do
        let expect =
                Pattern.withCondition
                    (mkBottom Mock.testSort)
                    Condition.top
            initial = Mock.a
        attemptEquation SideCondition.top initial equationBottom
            >>= expectRight
            >>= assertEqual "" expect
    , testCase "rule a => b ensures \\bottom" $ do
        let expect =
                Pattern.withCondition
                    Mock.b
                    Condition.bottom
            initial = Mock.a
        attemptEquation SideCondition.top initial equationEnsuresBottom
            >>= expectRight
            >>= assertEqual "" expect
    , testCase "rule a => b requires \\bottom" $ do
        let initial = Mock.a
        checkRequiresError <-
            attemptEquation SideCondition.top initial equationRequiresBottom
                >>= expectLeft
                >>= expectCheckRequiresError
        assertEqual "" makeFalsePredicate (equationRequires checkRequiresError)
    , testCase "rule a => \\bottom does not apply to c" $ do
        let initial = Mock.c
        attemptEquation SideCondition.top initial equationRequiresBottom
            >>= expectLeft
            >>= assertNotMatched
    , applies
        "F(x) => G(x) applies to F(x)"
        (axiom_ (f x) (g x))
        SideCondition.top
        (f x)
        (Pattern.fromTermLike $ g x)
    , applies
        "F(x) => G(x) [symbolic(x)] applies to F(x)"
        (axiom_ (f x) (g x) & symbolic [x])
        SideCondition.top
        (f x)
        (Pattern.fromTermLike $ g x)
    , notInstantiated
        "F(x) => G(x) [concrete(x)] doesn't apply to F(x)"
        (axiom_ (f x) (g x) & concrete [x])
        SideCondition.top
        (f x)
    , notInstantiated
        "F(x) => G(x) [concrete] doesn't apply to f(cf)"
        (axiom_ (f x) (g x) & concrete [x])
        SideCondition.top
        (f cf)
    , notMatched
        "F(x) => G(x) doesn't apply to F(top)"
        (axiom_ (f x) (g x))
        SideCondition.top
        (f (mkTop Mock.testSort))
    , applies
        "F(x) => G(x) [concrete] applies to F(a)"
        (axiom_ (f x) (g x) & concrete [x])
        SideCondition.top
        (f a)
        (Pattern.fromTermLike $ g a)
    , applies
        "Σ(X, Y) => A [symbolic(x), concrete(Y)]"
        (axiom_ (sigma x y) a & symbolic [x] & concrete [y])
        SideCondition.top
        (sigma x a)
        (Pattern.fromTermLike a)
    , notInstantiated
        "Σ(X, Y) => A [symbolic(x), concrete(Y)]"
        (axiom_ (sigma x y) a & symbolic [x] & concrete [y])
        SideCondition.top
        (sigma a a)
    , notInstantiated
        "Σ(X, Y) => A [symbolic(x), concrete(Y)]"
        (axiom_ (sigma x y) a & symbolic [x] & concrete [y])
        SideCondition.top
        (sigma x x)
    , requiresNotMet
        "F(x) => G(x) requires \\bottom doesn't apply to F(x)"
        (axiom (f x) (g x) makeFalsePredicate)
        SideCondition.top
        (f x)
    , notMatched
        "Σ(X, X) => G(X) doesn't apply to Σ(Y, Z) -- no narrowing"
        (axiom_ (sigma x x) (g x))
        SideCondition.top
        (sigma y z)
    , requiresNotMet
        -- using SMT
        "Σ(X, Y) => A requires (X > 0 and not Y > 0) doesn't apply to Σ(Z, Z)"
        (axiom (sigma x y) a (positive x `andNot` positive y))
        SideCondition.top
        (sigma z z)
    , applies
        -- using SMT
        "Σ(X, Y) => A requires (X > 0 or not Y > 0) applies to Σ(Z, Z)"
        (axiom (sigma x y) a (positive x `orNot` positive y))
        ( SideCondition.fromPredicateWithReplacements $
            positive a
        )
        (sigma a a)
        -- SMT not used to simplify trivial constraints
        (Pattern.fromTermLike a)
    , requiresNotMet
        -- using SMT
        "f(X) => A requires (X > 0) doesn't apply to f(Z) and (not (Z > 0))"
        (axiom (f x) a (positive x))
        ( SideCondition.fromPredicateWithReplacements $
            makeNotPredicate (positive z)
        )
        (f z)
    , applies
        -- using SMT
        "f(X) => A requires (X > 0) applies to f(Z) and (Z > 0)"
        (axiom (f x) a (positive x))
        ( SideCondition.fromPredicateWithReplacements $
            positive z
        )
        (f z)
        (Pattern.fromTermLike a)
    , testCase "X => X does not apply to X / X" $ do
        let initial = tdivInt xInt xInt
        attemptEquation SideCondition.top initial equationId
            >>= expectLeft
            >>= assertRequiresNotMet
    , testCase "X => X does apply to X / X if \\ceil(X / X)" $ do
        let initial = tdivInt xInt xInt
            sideCondition =
                makeCeilPredicate initial
                    & SideCondition.fromPredicateWithReplacements
            expect = Pattern.fromTermLike initial
        attemptEquation sideCondition initial equationId
            >>= expectRight
            >>= assertEqual "" expect
    , notInstantiated
        "does not introduce variables"
        (axiom_ (f a) (g x))
        SideCondition.top
        (f a)
    , applies
        -- using SMT
        "equation applies on Equation variable"
        (axiom (f x) a Predicate.makeTruePredicate)
        SideCondition.top
        (f zEq)
        (Pattern.fromTermLike a)
    ]

test_applySubstitutionAndSimplify :: [TestTree]
test_applySubstitutionAndSimplify =
    [ testCase "Function application in argument doesn't get evaluated" $ do
        let mockArgument :: Predicate RewritingVariableName
            mockArgument =
                var1Term `makeInPredicate` Mock.f var2Term
            expected =
                ( makeCeilPredicate (Mock.f var2Term)
                , variableName someVar1 `subst` Mock.f var2Term
                )
        (Right [actual]) <-
            applySubstitutionAndSimplify
                (Just mockArgument)
                Nothing
                mempty
                & runExceptT
                & testRunSimplifier env
        assertEqual "" expected actual
    ]
  where
    subst var term =
        Map.fromList [(var, term)]
    env = Mock.env{simplifierAxioms}
    simplifierAxioms =
        mkEvaluatorRegistry $
            Map.fromList
                [
                    ( AxiomIdentifier.Application Mock.fId
                    ,
                        [ functionAxiomUnification_
                            Mock.fSymbol
                            [mkElemVar Mock.zConfig]
                            Mock.a
                        ]
                    )
                ]
    someVar1 = Mock.xConfig & inject
    var1Term = mkElemVar Mock.xConfig
    var2Term = mkElemVar Mock.yConfig

-- * Test data

equationId :: Equation'
equationId = mkEquation (mkElemVar Mock.xConfig) (mkElemVar Mock.xConfig)

equationRequiresBottom :: Equation'
equationRequiresBottom =
    (mkEquation Mock.a Mock.b)
        { requires = makeFalsePredicate
        }

equationEnsuresBottom :: Equation'
equationEnsuresBottom =
    (mkEquation Mock.a Mock.b)
        { ensures = makeFalsePredicate
        }

equationBottom :: Equation'
equationBottom =
    mkEquation Mock.a (mkBottom Mock.testSort)

f, g :: TermLike RewritingVariableName -> TermLike RewritingVariableName
f = Mock.functionalConstr10
g = Mock.functionalConstr11

fSymbol :: Symbol
fSymbol = Mock.functionalConstr10Symbol

cf :: TermLike RewritingVariableName
cf = Mock.cf

sigma ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
sigma = Mock.functionalConstr20

sigmaSymbol :: Symbol
sigmaSymbol = Mock.functionalConstr20Symbol

string :: Text -> TermLike RewritingVariableName
string = Mock.builtinString

x, xString, xInt, y, z, zEq :: TermLike RewritingVariableName
x = mkElemVar Mock.xConfig
xInt = mkElemVar Mock.xConfigInt
xString = mkElemVar Mock.xConfigString
y = mkElemVar Mock.yConfig
z = mkElemVar Mock.zConfig
zEq = mkElemVar Mock.zEquation

a, b :: TermLike RewritingVariableName
a = Mock.a
b = Mock.b

tdivInt ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
tdivInt = Mock.tdivInt

positive :: TermLike RewritingVariableName -> Predicate RewritingVariableName
positive u' =
    makeEqualsPredicate
        ( Mock.lessInt
            (Mock.fTestFunctionalInt u') -- wrap the given term for sort agreement
            (Mock.builtinInt 0)
        )
        (Mock.builtinBool False)

andNot
    , orNot ::
        Predicate RewritingVariableName ->
        Predicate RewritingVariableName ->
        Predicate RewritingVariableName
andNot p1 p2 = makeAndPredicate p1 (makeNotPredicate p2)
orNot p1 p2 = makeOrPredicate p1 (makeNotPredicate p2)
-- * Test cases

withAttemptEquationResult ::
    (AttemptEquationResult' -> Assertion) ->
    TestName ->
    Equation' ->
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    TestTree
withAttemptEquationResult check testName equation sideCondition initial =
    testCase testName (attemptEquation sideCondition initial equation >>= check)

applies ::
    TestName ->
    Equation' ->
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    Pattern RewritingVariableName ->
    TestTree
applies testName equation sideCondition initial expect =
    withAttemptEquationResult
        (expectRight >=> assertEqual "" expect)
        testName
        equation
        sideCondition
        initial

notMatched ::
    TestName ->
    Equation' ->
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    TestTree
notMatched = withAttemptEquationResult (expectLeft >=> assertNotMatched)

notInstantiated ::
    TestName ->
    Equation' ->
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    TestTree
notInstantiated =
    withAttemptEquationResult (expectLeft >=> assertApplyMatchResultErrors)

requiresNotMet ::
    TestName ->
    Equation' ->
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    TestTree
requiresNotMet =
    withAttemptEquationResult (expectLeft >=> assertRequiresNotMet)

expectCheckRequiresError ::
    AttemptEquationError variable ->
    IO (CheckRequiresError variable)
expectCheckRequiresError (WhileCheckRequires checkRequiresError) =
    pure checkRequiresError
expectCheckRequiresError (WhileMatch _) =
    assertFailure "Expected WhileCheckRequires, but found WhileMatch"
expectCheckRequiresError (WhileApplyMatchResult _) =
    assertFailure "Expected WhileCheckRequires, but found WhileApplyMatchResult"
