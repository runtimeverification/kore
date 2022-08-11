{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Kore.Rewrite.Function.Integration (
    ) where

import Control.Lens qualified as Lens
import Data.Generics.Product
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Sup
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Builtin qualified as Builtin
import Kore.Builtin.AssociativeCommutative qualified as Ac
import Kore.Builtin.Int qualified as Int (
    builtinFunctions,
 )
import Kore.Builtin.Map qualified as Map (
    builtinFunctions,
 )
import Kore.Equation
import Kore.Error qualified
import Kore.IndexedModule.IndexedModule as IndexedModule
import Kore.IndexedModule.MetadataTools (
    SmtMetadataTools,
 )
import Kore.IndexedModule.MetadataToolsBuilder qualified as MetadataTools
import Kore.IndexedModule.SortGraph qualified as SortGraph
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate (
    makeAndPredicate,
    makeCeilPredicate,
    makeEqualsPredicate,
    makeTruePredicate,
 )
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition (
    top,
 )
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.Symbol
import Kore.Internal.TermLike
import Kore.Rewrite.Axiom.EvaluationStrategy (
    builtinEvaluation,
    definitionEvaluation,
    simplificationEvaluation,
    simplifierWithFallback,
 )
import Kore.Rewrite.Axiom.Identifier (
    AxiomIdentifier,
 )
import Kore.Rewrite.Axiom.Identifier qualified as AxiomIdentifier
import Kore.Rewrite.Function.Memo qualified as Memo
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
    configElementVariableFromId,
    mkConfigVariable,
 )
import Kore.Simplify.Condition qualified as Simplifier.Condition
import Kore.Simplify.InjSimplifier (
    InjSimplifier,
    mkInjSimplifier,
 )
import Kore.Simplify.Pattern qualified as Pattern
import Kore.Simplify.Simplify
import Kore.Simplify.Simplify as AttemptedAxiom (
    AttemptedAxiom (..),
 )
import Kore.Simplify.SubstitutionSimplifier qualified as SubstitutionSimplifier
import Kore.Simplify.TermLike qualified as TermLike
import Kore.Syntax.Definition hiding (
    Symbol (..),
 )
import Kore.Unparser
import Kore.Validate.DefinitionVerifier
import Kore.Validate.Error (
    VerifyError,
 )
import Prelude.Kore hiding (
    succ,
 )
import Pretty qualified
import Test.Kore
import Test.Kore.Builtin.Bool qualified as Bool
import Test.Kore.Builtin.Builtin qualified as Builtin
import Test.Kore.Builtin.Definition qualified as Builtin
import Test.Kore.Builtin.Int qualified as Int
import Test.Kore.Builtin.List qualified as List
import Test.Kore.Builtin.Map qualified as Map
import Test.Kore.Equation.Common (
    axiom,
    axiom_,
    functionAxiomUnification,
    functionAxiomUnification_,
 )
import Test.Kore.Rewrite.Axiom.Matcher (
    doesn'tMatch,
    matches,
 )
import Test.Kore.Rewrite.MockSymbols qualified as Mock
import Test.Kore.Simplify
import Test.Tasty
import Test.Tasty.HUnit.Ext

-- Evaluation tests: check the result of evaluating the term
equals ::
    HasCallStack =>
    TestName ->
    TermLike RewritingVariableName ->
    [TermLike RewritingVariableName] ->
    TestTree
equals comment term results =
    testCase comment $ do
        actual <- simplify term
        let expect = OrPattern.fromPatterns $ Pattern.fromTermLike <$> results
        assertEqual "" expect actual

equalsUnification ::
    HasCallStack =>
    TestName ->
    TermLike RewritingVariableName ->
    [TermLike RewritingVariableName] ->
    TestTree
equalsUnification comment term results =
    testCase comment $ do
        actual <- simplifyUnification term
        let expect = OrPattern.fromPatterns $ Pattern.fromTermLike <$> results
        assertEqual "" expect actual

simplify ::
    TermLike RewritingVariableName -> IO (OrPattern RewritingVariableName)
simplify = testRunSimplifier testEnv . TermLike.simplify SideCondition.top

simplifyUnification ::
    TermLike RewritingVariableName -> IO (OrPattern RewritingVariableName)
simplifyUnification = testRunSimplifier testEnvUnification . TermLike.simplify SideCondition.top

evaluate ::
    BuiltinAndAxiomSimplifierMap ->
    TermLike RewritingVariableName ->
    IO (OrPattern RewritingVariableName)
evaluate functionIdToEvaluator termLike =
    testRunSimplifier Mock.env{simplifierAxioms = functionIdToEvaluator} $
        TermLike.simplify SideCondition.top termLike

evaluateWith ::
    BuiltinAndAxiomSimplifier ->
    TermLike RewritingVariableName ->
    IO CommonAttemptedAxiom
evaluateWith simplifier patt =
    runSimplifierSMT testEnv $
        runBuiltinAndAxiomSimplifier simplifier patt SideCondition.top

evaluateWithUnification ::
    BuiltinAndAxiomSimplifier ->
    TermLike RewritingVariableName ->
    IO CommonAttemptedAxiom
evaluateWithUnification simplifier patt =
    testRunSimplifier testEnvUnification $
        runBuiltinAndAxiomSimplifier simplifier patt SideCondition.top

-- Applied tests: check that one or more rules applies or not
withApplied ::
    (CommonAttemptedAxiom -> Assertion) ->
    TestName ->
    [Equation RewritingVariableName] ->
    TermLike RewritingVariableName ->
    TestTree
withApplied check comment rules term =
    testCase comment $ do
        actual <- evaluateWith (definitionEvaluation rules) term
        check actual

withAppliedUnification ::
    (CommonAttemptedAxiom -> Assertion) ->
    TestName ->
    [Equation RewritingVariableName] ->
    TermLike RewritingVariableName ->
    TestTree
withAppliedUnification check comment rules term =
    testCase comment $ do
        actual <- evaluateWithUnification (definitionEvaluation rules) term
        check actual

applies
    , notApplies
    , appliesUnification
    , notAppliesUnification ::
        TestName ->
        [Equation RewritingVariableName] ->
        TermLike RewritingVariableName ->
        TestTree
applies =
    withApplied $ \attempted -> do
        results <- expectApplied attempted
        expectNoRemainders results
  where
    expectApplied NotApplicable = assertFailure "Expected Applied"
    expectApplied (NotApplicableUntilConditionChanges _) =
        assertFailure "Expected Applied"
    expectApplied (Applied results) = return results
    expectNoRemainders =
        assertBool "Expected no remainders"
            . isBottom
            . Lens.view (field @"remainders")
notApplies =
    withApplied $ \r ->
        assertBool "Expected NotApplicable" $
            isNotApplicable r || isNotApplicableUntilConditionChanges r
appliesUnification =
    withAppliedUnification $ \attempted -> do
        results <- expectApplied attempted
        expectNoRemainders results
  where
    expectApplied NotApplicable = assertFailure "Expected Applied"
    expectApplied (NotApplicableUntilConditionChanges _) =
        assertFailure "Expected Applied"
    expectApplied (Applied results) = return results
    expectNoRemainders =
        assertBool "Expected no remainders"
            . isBottom
            . Lens.view (field @"remainders")
notAppliesUnification =
    withAppliedUnification $ \r ->
        assertBool "Expected NotApplicable" $
            isNotApplicable r || isNotApplicableUntilConditionChanges r

natSort :: Sort
natSort =
    SortActualSort
        SortActual
            { sortActualName = testId "Nat"
            , sortActualSorts = []
            }

natMConfig
    , natNConfig ::
        ElementVariable RewritingVariableName
natMConfig = configElementVariableFromId "M" natSort
natNConfig = configElementVariableFromId "N" natSort

varMConfig, varNConfig :: TermLike RewritingVariableName
varMConfig = mkElemVar natMConfig
varNConfig = mkElemVar natNConfig

zeroSymbol, succSymbol :: Symbol
zeroSymbol = Mock.symbol "Zero" [] natSort & constructor & functional
succSymbol = Mock.symbol "Succ" [natSort] natSort & constructor & functional

plusSymbol, timesSymbol :: Symbol
plusSymbol = Mock.symbol "plus" [natSort, natSort] natSort & function
timesSymbol = Mock.symbol "times" [natSort, natSort] natSort & function

fibonacciSymbol, factorialSymbol :: Symbol
fibonacciSymbol = Mock.symbol "fibonacci" [natSort] natSort & function
factorialSymbol = Mock.symbol "factorial" [natSort] natSort & function

zero :: TermLike RewritingVariableName
zero = mkApplySymbol zeroSymbol []

one, two :: TermLike RewritingVariableName
one = succ zero
two = succ one

succ
    , fibonacci
    , factorial ::
        TermLike RewritingVariableName -> TermLike RewritingVariableName
succ n = mkApplySymbol succSymbol [n]
fibonacci n = mkApplySymbol fibonacciSymbol [n]
factorial n = mkApplySymbol factorialSymbol [n]

plus
    , times ::
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName ->
        TermLike RewritingVariableName
plus n1 n2 = mkApplySymbol plusSymbol [n1, n2]
times n1 n2 = mkApplySymbol timesSymbol [n1, n2]

functionEvaluator ::
    Symbol ->
    -- | Function definition rules
    [Equation RewritingVariableName] ->
    (AxiomIdentifier, BuiltinAndAxiomSimplifier)
functionEvaluator symb rules =
    (AxiomIdentifier.Application ident, definitionEvaluation rules)
  where
    ident = symbolConstructor symb

functionSimplifier ::
    Symbol ->
    -- | Function simplification rule
    [Equation RewritingVariableName] ->
    (AxiomIdentifier, BuiltinAndAxiomSimplifier)
functionSimplifier symb rules =
    ( AxiomIdentifier.Application ident
    , firstFullEvaluation (simplificationEvaluation <$> rules)
    )
  where
    ident = symbolConstructor symb

plusZeroRule, plusSuccRule :: Equation RewritingVariableName
plusZeroRule = axiom_ (plus zero varNConfig) varNConfig
plusSuccRule =
    axiom_ (plus (succ varMConfig) varNConfig) (succ (plus varMConfig varNConfig))

plusRules :: [Equation RewritingVariableName]
plusRules = [plusZeroRule, plusSuccRule]

plusEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
plusEvaluator = functionEvaluator plusSymbol plusRules

timesEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
timesEvaluator =
    functionEvaluator
        timesSymbol
        [ axiom_ (times zero varNConfig) zero
        , axiom_
            (times (succ varMConfig) varNConfig)
            (plus varNConfig (times varMConfig varNConfig))
        ]

fibonacciEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
fibonacciEvaluator =
    functionEvaluator
        fibonacciSymbol
        [ axiom_ (fibonacci zero) one
        , axiom_ (fibonacci one) one
        , axiom_
            (fibonacci (succ (succ varNConfig)))
            (plus (fibonacci (succ varNConfig)) (fibonacci varNConfig))
        ]

factorialEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
factorialEvaluator =
    functionEvaluator
        factorialSymbol
        [ axiom_ (factorial zero) (succ zero)
        , axiom_
            (factorial (succ varNConfig))
            (times (succ varNConfig) (factorial varNConfig))
        ]

natSimplifiers :: BuiltinAndAxiomSimplifierMap
natSimplifiers =
    Map.fromList
        [ plusEvaluator
        , timesEvaluator
        , fibonacciEvaluator
        , factorialEvaluator
        ]

plusZeroRuleUnification
    , plusSuccRuleUnification ::
        Equation RewritingVariableName
plusZeroRuleUnification =
    functionAxiomUnification_ plusSymbol [zero, varNConfig] varNConfig
plusSuccRuleUnification =
    functionAxiomUnification_
        plusSymbol
        [succ varMConfig, varNConfig]
        (succ (plus varMConfig varNConfig))

plusRulesUnification :: [Equation RewritingVariableName]
plusRulesUnification = [plusZeroRuleUnification, plusSuccRuleUnification]

plusEvaluatorUnification :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
plusEvaluatorUnification = functionEvaluator plusSymbol plusRulesUnification

timesEvaluatorUnification :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
timesEvaluatorUnification =
    functionEvaluator
        timesSymbol
        [ functionAxiomUnification_ timesSymbol [zero, varNConfig] zero
        , functionAxiomUnification_
            timesSymbol
            [succ varMConfig, varNConfig]
            (plus varNConfig (times varMConfig varNConfig))
        ]

fibonacciEvaluatorUnification :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
fibonacciEvaluatorUnification =
    functionEvaluator
        fibonacciSymbol
        [ functionAxiomUnification_ fibonacciSymbol [zero] one
        , functionAxiomUnification_ fibonacciSymbol [one] one
        , functionAxiomUnification_
            fibonacciSymbol
            [succ (succ varNConfig)]
            (plus (fibonacci (succ varNConfig)) (fibonacci varNConfig))
        ]

factorialEvaluatorUnification :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
factorialEvaluatorUnification =
    functionEvaluator
        factorialSymbol
        [ functionAxiomUnification_
            factorialSymbol
            [zero]
            (succ zero)
        , functionAxiomUnification_
            factorialSymbol
            [succ varNConfig]
            (times (succ varNConfig) (factorial varNConfig))
        ]

natSimplifiersUnification :: BuiltinAndAxiomSimplifierMap
natSimplifiersUnification =
    Map.fromList
        [ plusEvaluatorUnification
        , timesEvaluatorUnification
        , fibonacciEvaluatorUnification
        , factorialEvaluatorUnification
        ]

-- | Add an unsatisfiable requirement to the 'Equation'.
requiresBottom ::
    Equation RewritingVariableName -> Equation RewritingVariableName
requiresBottom equation = equation{requires = makeEqualsPredicate zero one}

{- | Add an unsatisfiable @\\equals@ requirement to the 'Equation'.

In contrast to 'requiresBottom', @requiresFatalEquals@ also includes a
requirement which results in a fatal error when evaluated.
-}
requiresFatalEquals ::
    Equation RewritingVariableName -> Equation RewritingVariableName
requiresFatalEquals equation =
    equation
        { requires =
            makeAndPredicate
                (makeEqualsPredicate (fatal zero) one)
                (makeEqualsPredicate zero one)
        }

{- | Add an unsatisfiable @\\in@ requirement to the 'Equation'.

In contrast to 'requiresBottom', @requiresFatalEquals@ also includes a
requirement which results in a fatal error when evaluated.
-}
requiresFatalIn ::
    Equation RewritingVariableName -> Equation RewritingVariableName
requiresFatalIn equation =
    equation
        { requires =
            makeAndPredicate
                (makeEqualsPredicate (fatal zero) one)
                (makeCeilPredicate (mkAnd zero one))
        }

{- | Test short-circuiting evaluation of function requirements.

We want to check that functions are not evaluated in an 'Equation'
requirement if the pre-condition is known to be unsatisfiable without function
evaluation. We check this by including a 'requires' clause with one
unsatisfiable condition and one "fatal" condition (a condition producing a fatal
error if evaluated). If we do function evaluation on the unsatisfiable
requirement, a fatal error will be produced.
-}

{- | A symbol which throws a fatal error when evaluated.

@fatalSymbol@ is useful for checking that symbols in certain positions are never
evaluated.
-}
fatalSymbol :: Symbol
fatalSymbol = Mock.symbol "fatal" [natSort] natSort & function

fatal :: TermLike RewritingVariableName -> TermLike RewritingVariableName
fatal x = mkApplySymbol fatalSymbol [x]

fatalEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
fatalEvaluator =
    ( AxiomIdentifier.Application ident
    , BuiltinAndAxiomSimplifier $ \_ _ -> error "fatal error"
    )
  where
    ident = symbolConstructor fatalSymbol

fatalSimplifiers :: BuiltinAndAxiomSimplifierMap
fatalSimplifiers = uncurry Map.singleton fatalEvaluator

listSort, intSort, mapSort :: Sort
listSort = Builtin.listSort
intSort = Builtin.intSort
mapSort = Builtin.mapSort

mkList :: [TermLike RewritingVariableName] -> TermLike RewritingVariableName
mkList = List.asInternal

mkInt :: Integer -> TermLike RewritingVariableName
mkInt = Int.asInternal

mkMap ::
    [(TermLike RewritingVariableName, TermLike RewritingVariableName)] ->
    [TermLike RewritingVariableName] ->
    TermLike RewritingVariableName
mkMap elements opaques =
    Ac.asInternal Builtin.testMetadataTools Builtin.mapSort $
        Map.normalizedMap elements opaques

removeMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
removeMap = Builtin.removeMap

addInt ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
addInt = Builtin.addInt

unitList :: TermLike RewritingVariableName
unitList = mkList []

varXConfig, varYConfig, varLConfig, mMapConfigTerm :: TermLike RewritingVariableName
varXConfig = mkElemVar xConfigInt
varYConfig = mkElemVar yConfigInt
varLConfig = mkElemVar (configElementVariableFromId (testId "lList") listSort)
mMapConfigTerm = mkElemVar mMapConfig

mMapConfig :: ElementVariable RewritingVariableName
mMapConfig = configElementVariableFromId (testId "mMap") mapSort

lengthListSymbol :: Symbol
lengthListSymbol = Mock.symbol "lengthList" [listSort] intSort & function

lengthList :: TermLike RewritingVariableName -> TermLike RewritingVariableName
lengthList l = mkApplySymbol lengthListSymbol [l]

concatList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
concatList = Builtin.concatList

consList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
consList x xs = concatList (mkList [x]) xs

lengthListUnitRule, lengthListConsRule :: Equation RewritingVariableName
lengthListUnitRule = axiom_ (lengthList unitList) (mkInt 0)
lengthListConsRule =
    axiom_
        (lengthList (consList varXConfig varLConfig))
        (addInt (mkInt 1) (lengthList varLConfig))

lengthListRules :: [Equation RewritingVariableName]
lengthListRules = [lengthListUnitRule, lengthListConsRule]

lengthListEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
lengthListEvaluator = functionEvaluator lengthListSymbol lengthListRules

removeListSymbol :: Symbol
removeListSymbol =
    Mock.symbol "removeList" [listSort, mapSort] mapSort & function

removeList ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
removeList l m = mkApplySymbol removeListSymbol [l, m]

removeListUnitRule, removeListConsRule :: Equation RewritingVariableName
removeListUnitRule = axiom_ (removeList unitList mMapConfigTerm) mMapConfigTerm
removeListConsRule =
    axiom_
        (removeList (consList varXConfig varLConfig) mMapConfigTerm)
        (removeMap mMapConfigTerm varXConfig)

removeListRules :: [Equation RewritingVariableName]
removeListRules = [removeListUnitRule, removeListConsRule]

removeListEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
removeListEvaluator = functionEvaluator removeListSymbol removeListRules

listSimplifiers :: BuiltinAndAxiomSimplifierMap
listSimplifiers =
    Map.fromList
        [ lengthListEvaluator
        , removeListEvaluator
        , (addIntId, builtinEvaluation addIntEvaluator)
        , (removeMapId, builtinEvaluation removeMapEvaluator)
        ]
  where
    Just addIntEvaluator = Map.lookup "INT.add" Int.builtinFunctions
    addIntId =
        AxiomIdentifier.Application $
            symbolConstructor Builtin.addIntSymbol
    Just removeMapEvaluator = Map.lookup "MAP.remove" Map.builtinFunctions
    removeMapId =
        AxiomIdentifier.Application $
            symbolConstructor Builtin.removeMapSymbol

singletonList :: TermLike RewritingVariableName
singletonList = Builtin.elementList (mkInt 0)

twoElementList :: TermLike RewritingVariableName
twoElementList = Builtin.concatList singletonList singletonList

updateList ::
    -- | List
    TermLike RewritingVariableName ->
    -- | Index
    TermLike RewritingVariableName ->
    -- | Value
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
updateList = Builtin.updateList

updateListSimplifier :: Equation RewritingVariableName
updateListSimplifier =
    axiom
        (updateList (updateList varLConfig u v) x y)
        (updateList varLConfig u y)
        (makeEqualsPredicate (Builtin.keqBool (injK u) (injK x)) (mkBool True))
  where
    [u, v, x, y] = mkElemVar <$> [uConfigInt, vConfigInt, xConfigInt, yConfigInt]
    injK = Builtin.inj Builtin.kSort

lookupMapSymbol :: Symbol
lookupMapSymbol = Builtin.lookupMapSymbol

lookupMap ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
lookupMap = Builtin.lookupMap

lookupMapRule :: Equation RewritingVariableName
lookupMapRule =
    axiom_
        ( lookupMap
            (mkMap [(varXConfig, varYConfig)] [mMapConfigTerm])
            varXConfig
        )
        varYConfig

lookupMapRules :: [Equation RewritingVariableName]
lookupMapRules = [lookupMapRule]

lookupMapEvaluator :: (AxiomIdentifier, BuiltinAndAxiomSimplifier)
lookupMapEvaluator = functionEvaluator lookupMapSymbol lookupMapRules

updateMap ::
    -- | Map
    TermLike RewritingVariableName ->
    -- | Key
    TermLike RewritingVariableName ->
    -- | Value
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName
updateMap = Builtin.updateMap

updateMapSimplifier :: Equation RewritingVariableName
updateMapSimplifier =
    axiom
        (updateMap (updateMap mMapConfigTerm u v) x y)
        (updateMap mMapConfigTerm u y)
        (makeEqualsPredicate (Builtin.keqBool (injK u) (injK x)) (mkBool True))
  where
    [u, v, x, y] = mkElemVar <$> [uConfigInt, vConfigInt, xConfigInt, yConfigInt]
    injK = Builtin.inj Builtin.kSort

dummyIntSimplifier :: Equation RewritingVariableName
dummyIntSimplifier =
    axiom_ (Builtin.dummyInt (mkElemVar xConfigInt)) (mkElemVar xConfigInt)

mkBool :: Bool -> TermLike RewritingVariableName
mkBool = Bool.asInternal

mapSimplifiers :: BuiltinAndAxiomSimplifierMap
mapSimplifiers =
    Map.fromList
        [ lookupMapEvaluator
        , functionSimplifier Builtin.updateMapSymbol [updateMapSimplifier]
        , functionEvaluator
            Builtin.dummyIntSymbol
            [dummyIntSimplifier]
        ]

uConfigInt, vConfigInt, xConfigInt, yConfigInt :: ElementVariable RewritingVariableName
uConfigInt = configElementVariableFromId (testId "uInt") intSort
vConfigInt = configElementVariableFromId (testId "vInt") intSort
xConfigInt = configElementVariableFromId (testId "xInt") intSort
yConfigInt = configElementVariableFromId (testId "yInt") intSort

xsConfigInt :: SetVariable RewritingVariableName
xsConfigInt =
    mkSetVariable (testId "xsInt") intSort
        & mapSetVariable (pure mkConfigVariable)

ceilDummyRule :: Equation RewritingVariableName
ceilDummyRule =
    axiom_
        (mkCeil Builtin.kSort $ Builtin.dummyInt $ mkElemVar xConfigInt)
        (mkEquals Builtin.kSort (Builtin.eqInt (mkElemVar xConfigInt) (mkInt 0)) (mkBool False))

ceilDummySetRule :: Equation RewritingVariableName
ceilDummySetRule =
    axiom_
        (mkCeil Builtin.kSort $ Builtin.dummyInt $ mkSetVar xsConfigInt)
        (mkEquals Builtin.kSort (Builtin.eqInt (mkSetVar xsConfigInt) (mkInt 0)) (mkBool False))

-- Simplification tests: check that one or more rules applies or not
withSimplified ::
    (CommonAttemptedAxiom -> Assertion) ->
    TestName ->
    Equation RewritingVariableName ->
    TermLike RewritingVariableName ->
    TestTree
withSimplified check comment rule term =
    testCase comment $ do
        actual <- evaluateWith (simplificationEvaluation rule) term
        check actual

simplifies
    , notSimplifies ::
        TestName ->
        Equation RewritingVariableName ->
        TermLike RewritingVariableName ->
        TestTree
simplifies =
    withSimplified $ \attempted -> do
        results <- expectApplied attempted
        expectNoRemainders results
  where
    expectApplied NotApplicable = assertFailure "Expected Applied"
    expectApplied (NotApplicableUntilConditionChanges _) =
        assertFailure "Expected Applied"
    expectApplied (Applied results) = return results
    expectNoRemainders =
        assertBool "Expected no remainders"
            . isBottom
            . Lens.view (field @"remainders")
notSimplifies =
    withSimplified (assertBool "Expected NotApplicable" . isNotApplicable)

axiomEvaluator ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    BuiltinAndAxiomSimplifier
axiomEvaluator left right =
    simplificationEvaluation (axiom left right makeTruePredicate)

axiomEvaluatorUnification ::
    Symbol ->
    [TermLike RewritingVariableName] ->
    TermLike RewritingVariableName ->
    BuiltinAndAxiomSimplifier
axiomEvaluatorUnification symbol args right =
    simplificationEvaluation
        (functionAxiomUnification symbol args right makeTruePredicate)

appliedMockEvaluator ::
    Pattern VariableName -> BuiltinAndAxiomSimplifier
appliedMockEvaluator result =
    BuiltinAndAxiomSimplifier $
        mockEvaluator $
            AttemptedAxiom.Applied
                AttemptedAxiomResults
                    { results =
                        OrPattern.fromPatterns
                            [Test.Kore.Rewrite.Function.Integration.mapVariablesConfig result]
                    , remainders = OrPattern.fromPatterns []
                    }

mapVariablesConfig ::
    Pattern VariableName ->
    Pattern RewritingVariableName
mapVariablesConfig =
    Pattern.mapVariables (pure worker)
  where
    worker :: VariableName -> RewritingVariableName
    worker v = mkConfigVariable v{counter = Just (Element 1)}

mockEvaluator ::
    Monad simplifier =>
    AttemptedAxiom variable ->
    TermLike variable ->
    SideCondition variable ->
    simplifier (AttemptedAxiom variable)
mockEvaluator evaluation _ _ = return evaluation
-- ---------------------------------------------------------------------

-- * Definition

natModuleName :: ModuleName
natModuleName = ModuleName "NAT"

natSortDecl :: Sentence pattern'
natSortDecl =
    asSentence
        SentenceSort
            { sentenceSortName =
                let SortActualSort SortActual{sortActualName} = natSort
                 in sortActualName
            , sentenceSortParameters = []
            , sentenceSortAttributes = Attributes []
            }

-- | Declare the @BOOL@ builtins.
natModule :: ParsedModule
natModule =
    Module
        { moduleName = natModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ natSortDecl
            , Builtin.symbolDecl zeroSymbol
            , Builtin.symbolDecl succSymbol
            , Builtin.symbolDecl plusSymbol
            , Builtin.symbolDecl timesSymbol
            , Builtin.symbolDecl fibonacciSymbol
            , Builtin.symbolDecl factorialSymbol
            ]
        }

testModuleName :: ModuleName
testModuleName = ModuleName "INTEGRATION-TEST"

testModule :: ParsedModule
testModule =
    Module
        { moduleName = testModuleName
        , moduleAttributes = Attributes []
        , moduleSentences =
            [ Builtin.importParsedModule Builtin.testModuleName
            , Builtin.importParsedModule natModuleName
            , Builtin.subsortDecl Builtin.intSort Builtin.kSort
            ]
        }

testDefinition :: ParsedDefinition
testDefinition =
    Builtin.testDefinition
        & field @"definitionModules" Lens.<>~ [natModule, testModule]

verify ::
    ParsedDefinition ->
    Either
        (Kore.Error.Error VerifyError)
        ( Map ModuleName (VerifiedModule Attribute.Symbol)
        )
verify = verifyAndIndexDefinition Builtin.koreVerifiers

verifiedModules ::
    Map ModuleName (VerifiedModule Attribute.Symbol)
verifiedModules = Kore.Error.assertRight (verify testDefinition)

verifiedModule :: VerifiedModule Attribute.Symbol
Just verifiedModule = Map.lookup testModuleName verifiedModules

testMetadataTools :: SmtMetadataTools Attribute.Symbol
testMetadataTools = MetadataTools.build verifiedModule

testConditionSimplifier ::
    MonadSimplify simplifier => ConditionSimplifier simplifier
testConditionSimplifier =
    Simplifier.Condition.create SubstitutionSimplifier.substitutionSimplifier

testEvaluators :: BuiltinAndAxiomSimplifierMap
testEvaluators = Builtin.koreEvaluators $ indexedModuleSyntax verifiedModule

testInjSimplifier :: InjSimplifier
testInjSimplifier =
    mkInjSimplifier $ SortGraph.fromIndexedModule verifiedModule

testEnv :: Env
testEnv =
    Env
        { metadataTools = testMetadataTools
        , simplifierCondition = testConditionSimplifier
        , simplifierPattern = Pattern.makeEvaluate
        , simplifierTerm = TermLike.simplify
        , simplifierAxioms =
            mconcat
                [ testEvaluators
                , natSimplifiers
                , listSimplifiers
                , mapSimplifiers
                , fatalSimplifiers
                ]
        , memo = Memo.forgetful
        , injSimplifier = testInjSimplifier
        , overloadSimplifier = Mock.overloadSimplifier
        }

-- TODO: make this Simplifier Env and build the evaluators here
testEnvUnification :: Env
testEnvUnification = do
    testEnv
        { simplifierAxioms =
            mconcat
                [ testEvaluators
                , natSimplifiersUnification
                , listSimplifiers
                , mapSimplifiers
                , fatalSimplifiers
                ]
        }
