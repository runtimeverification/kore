module Test.Kore.Builtin.Map where

import           Hedgehog hiding
                 ( Concrete )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Control.Monad as Monad
import qualified Data.Default as Default
import qualified Data.List as List
import           Data.Map
                 ( Map )
import qualified Data.Map as Map
import qualified Data.Reflection as Reflection
import qualified Data.Set as Set
import           Prelude hiding
                 ( concatMap )

import           Kore.Attribute.Hook
                 ( Hook )
import qualified Kore.Attribute.Symbol as StepperAttributes
import qualified Kore.Builtin.Map as Map
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import           Kore.Internal.MultiOr
                 ( MultiOr (..) )
import           Kore.Internal.Pattern
import qualified Kore.Internal.Pattern as Pattern
import           Kore.Internal.TermLike
import           Kore.Predicate.Predicate
                 ( makeTruePredicate )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.Rule
import           Kore.Step.Simplification.Data
import qualified Kore.Unification.Substitution as Substitution
import qualified SMT

import           Test.Kore
import qualified Test.Kore.Builtin.Bool as Test.Bool
import           Test.Kore.Builtin.Builtin
import           Test.Kore.Builtin.Definition
import           Test.Kore.Builtin.Int
                 ( genConcreteIntegerPattern, genInteger, genIntegerPattern )
import qualified Test.Kore.Builtin.Int as Test.Int
import qualified Test.Kore.Builtin.Set as Test.Set
import           Test.Kore.Comparators ()
import qualified Test.Kore.Step.MockSymbols as Mock
import           Test.SMT
import           Test.Tasty.HUnit.Extensions

genMapInteger :: Gen a -> Gen (Map Integer a)
genMapInteger genElement =
    Gen.map (Range.linear 0 32) ((,) <$> genInteger <*> genElement)

genConcreteMap :: Gen a -> Gen (Map (TermLike Concrete) a)
genConcreteMap genElement =
    Map.mapKeys Test.Int.asInternal <$> genMapInteger genElement

genMapPattern :: Gen (TermLike Variable)
genMapPattern = asTermLike <$> genConcreteMap genIntegerPattern

genMapSortedVariable :: Sort -> Gen a -> Gen (Map Variable a)
genMapSortedVariable sort genElement =
    Gen.map
        (Range.linear 0 32)
        ((,) <$> standaloneGen (variableGen sort) <*> genElement)

test_lookupUnit :: TestTree
test_lookupUnit =
    testPropertyWithSolver
        "lookup{}(unit{}(), key) === \\bottom{}()"
        (do
            key <- forAll genIntegerPattern
            let patLookup = lookupMap unitMap key
                predicate = mkEquals_ mkBottom_ patLookup
            (===) Pattern.bottom =<< evaluate patLookup
            (===) Pattern.top =<< evaluate predicate
        )

test_lookupUpdate :: TestTree
test_lookupUpdate =
    testPropertyWithSolver
        "lookup{}(update{}(map, key, val), key) === val"
        (do
            patKey <- forAll genIntegerPattern
            patVal <- forAll genIntegerPattern
            patMap <- forAll genMapPattern
            let patLookup = lookupMap (updateMap patMap patKey patVal) patKey
                predicate = mkEquals_ patLookup patVal
                expect = Pattern.fromTermLike patVal
            (===) expect =<< evaluate patLookup
            (===) Pattern.top =<< evaluate predicate
        )

test_removeUnit :: TestTree
test_removeUnit =
    testPropertyWithSolver
        "remove{}(unit{}(), key) === unit{}()"
        (do
            key <- forAll genIntegerPattern
            let patRemove = removeMap unitMap key
                predicate = mkEquals_ unitMap patRemove
            expect <- evaluate unitMap
            (===) expect =<< evaluate patRemove
            (===) Pattern.top =<< evaluate predicate
        )

test_removeKeyIn :: TestTree
test_removeKeyIn =
    testPropertyWithSolver
        "MAP.remove key with key not in map"
        (do
            key <- forAll genIntegerPattern
            map <- forAll genMapPattern
            isInMap <- evaluate $ lookupMap map key
            Monad.when (not $ Pattern.bottom == isInMap) discard
            let patRemove = removeMap map key
                predicate = mkEquals_ map patRemove
            expect <- evaluate map
            (===) expect =<< evaluate patRemove
            (===) Pattern.top =<< evaluate predicate
        )

test_removeAllUnit :: TestTree
test_removeAllUnit =
    testPropertyWithSolver
        "removeAll{}(unit{}(), set) === unit{}()"
        (do
            set <- forAll Test.Set.genSetPattern
            let patRemoveAll = removeAllMap unitMap set
                predicate = mkEquals_ unitMap patRemoveAll
            expect <- evaluate unitMap
            (===) expect =<< evaluate patRemoveAll
            (===) Pattern.top =<< evaluate predicate
        )

test_concatUnit :: TestTree
test_concatUnit =
    testPropertyWithSolver
        "concat{}(unit{}(), map) === concat{}(map, unit{}()) === map"
        (do
            patMap <- forAll genMapPattern
            let patConcat2 = concatMap patUnit patMap
                patConcat1 = concatMap patMap patUnit
                patUnit = unitMap
                predicate1 = mkEquals_ patMap patConcat1
                predicate2 = mkEquals_ patMap patConcat2
            expect <- evaluate patMap
            (===) expect =<< evaluate patConcat1
            (===) expect =<< evaluate patConcat2
            (===) Pattern.top =<< evaluate predicate1
            (===) Pattern.top =<< evaluate predicate2
        )

test_lookupConcatUniqueKeys :: TestTree
test_lookupConcatUniqueKeys =
    testPropertyWithSolver
        "MAP.lookup with two unique keys"
        (do
            patKey1 <- forAll genIntegerPattern
            patKey2 <- forAll genIntegerPattern
            Monad.when (patKey1 == patKey2) discard
            patVal1 <- forAll genIntegerPattern
            patVal2 <- forAll genIntegerPattern
            let patConcat = concatMap patMap1 patMap2
                patMap1 = elementMap patKey1 patVal1
                patMap2 = elementMap patKey2 patVal2
                patLookup1 = lookupMap patConcat patKey1
                patLookup2 = lookupMap patConcat patKey2
                predicate =
                    mkImplies
                        (mkNot (mkEquals_ patKey1 patKey2))
                        (mkAnd
                            (mkEquals_ patLookup1 patVal1)
                            (mkEquals_ patLookup2 patVal2)
                        )
                expect1 = Pattern.fromTermLike patVal1
                expect2 = Pattern.fromTermLike patVal2
            (===) expect1 =<< evaluate patLookup1
            (===) expect2 =<< evaluate patLookup2
            (===) Pattern.top =<< evaluate predicate
        )

test_concatDuplicateKeys :: TestTree
test_concatDuplicateKeys =
    testPropertyWithSolver
        "concat{}(element{}(key, val1), element{}(key, val2)) === \\bottom{}()"
        (do
            patKey <- forAll genIntegerPattern
            patVal1 <- forAll genIntegerPattern
            patVal2 <- forAll genIntegerPattern
            let patMap1 = elementMap patKey patVal1
                patMap2 = elementMap patKey patVal2
                patConcat = concatMap patMap1 patMap2
                predicate = mkEquals_ mkBottom_ patConcat
            (===) Pattern.bottom =<< evaluate patConcat
            (===) Pattern.top =<< evaluate predicate
        )

test_concatCommutes :: TestTree
test_concatCommutes =
    testPropertyWithSolver
        "concat{}(as, bs) === concat{}(bs, as)"
        (do
            patMap1 <- forAll genMapPattern
            patMap2 <- forAll genMapPattern
            let patConcat1 = concatMap patMap1 patMap2
                patConcat2 = concatMap patMap2 patMap1
                predicate = mkEquals_ patConcat1 patConcat2
            actual1 <- evaluate patConcat1
            actual2 <- evaluate patConcat2
            (===) actual1 actual2
            (===) Pattern.top =<< evaluate predicate
        )

test_concatAssociates :: TestTree
test_concatAssociates =
    testPropertyWithSolver
        "concat{}(concat{}(as, bs), cs) === concat{}(as, concat{}(bs, cs))"
        (do
            patMap1 <- forAll genMapPattern
            patMap2 <- forAll genMapPattern
            patMap3 <- forAll genMapPattern
            let patConcat12 = concatMap patMap1 patMap2
                patConcat23 = concatMap patMap2 patMap3
                patConcat12_3 = concatMap patConcat12 patMap3
                patConcat1_23 = concatMap patMap1 patConcat23
                predicate = mkEquals_ patConcat12_3 patConcat1_23
            actual12_3 <- evaluate patConcat12_3
            actual1_23 <- evaluate patConcat1_23
            (===) actual12_3 actual1_23
            (===) Pattern.top =<< evaluate predicate
        )

test_inKeysUnit :: TestTree
test_inKeysUnit =
    testPropertyWithSolver
        "inKeys{}(unit{}(), key) === \\dv{Bool{}}(\"false\")"
        (do
            patKey <- forAll genIntegerPattern
            let patUnit = unitMap
                patInKeys = inKeysMap patKey patUnit
                predicate = mkEquals_ (Test.Bool.asInternal False) patInKeys
            (===) (Test.Bool.asPattern False) =<< evaluate patInKeys
            (===) Pattern.top =<< evaluate predicate
        )

test_keysUnit :: TestTree
test_keysUnit =
    testCaseWithSolver
        "keys{}(unit{}() : Map{}) === unit{}() : Set{}"
        (\solver -> do
            let
                patUnit = unitMap
                patKeys = keysMap patUnit
                patExpect = Test.Set.asTermLike Set.empty
                predicate = mkEquals_ patExpect patKeys
            expect <- evaluateWith solver patExpect
            assertEqualWithExplanation "" expect =<< evaluateWith solver patKeys
            assertEqualWithExplanation "" Pattern.top =<< evaluateWith solver predicate
        )

test_keysElement :: TestTree
test_keysElement =
    testPropertyWithSolver
        "keys{}(element{}(key, _) : Map{}) === element{}(key) : Set{}"
        (do
            key <- forAll genConcreteIntegerPattern
            val <- forAll genIntegerPattern
            let patMap = asTermLike $ Map.singleton key val
                patKeys = Test.Set.asTermLike $ Set.singleton key
                patSymbolic = keysMap patMap
                predicate = mkEquals_ patKeys patSymbolic
            expect <- evaluate patKeys
            (===) expect =<< evaluate patSymbolic
            (===) Pattern.top =<< evaluate predicate
        )

test_keys :: TestTree
test_keys =
    testPropertyWithSolver
        "MAP.keys"
        (do
            map1 <- forAll (genConcreteMap genIntegerPattern)
            let keys1 = Map.keysSet map1
                patConcreteKeys = Test.Set.asTermLike keys1
                patMap = asTermLike map1
                patSymbolicKeys = keysMap patMap
                predicate = mkEquals_ patConcreteKeys patSymbolicKeys
            expect <- evaluate patConcreteKeys
            (===) expect =<< evaluate patSymbolicKeys
            (===) Pattern.top =<< evaluate predicate
        )

test_inKeysElement :: TestTree
test_inKeysElement =
    testPropertyWithSolver
        "inKeys{}(element{}(key, _), key) === \\dv{Bool{}}(\"true\")"
        (do
            patKey <- forAll genIntegerPattern
            patVal <- forAll genIntegerPattern
            let patMap = elementMap patKey patVal
                patInKeys = inKeysMap patKey patMap
                predicate = mkEquals_ (Test.Bool.asInternal True) patInKeys
            (===) (Test.Bool.asPattern True) =<< evaluate patInKeys
            (===) Pattern.top =<< evaluate predicate
        )

-- | Check that simplification is carried out on map elements.
test_simplify :: TestTree
test_simplify =
    testCaseWithSolver
        "simplify builtin Map elements"
        (\solver -> do
            let
                x =
                    mkVar Variable
                        { variableName = testId "x"
                        , variableCounter = mempty
                        , variableSort = intSort
                        }
                key = Test.Int.asInternal 1
                original = asTermLike $ Map.fromList [(key, mkAnd x mkTop_)]
                expected = asPattern $ Map.fromList [(key, x)]
            actual <- evaluateWith solver original
            assertEqualWithExplanation "expected simplified Map" expected actual
        )

-- | Maps with symbolic keys are not simplified.
test_symbolic :: TestTree
test_symbolic =
    testPropertyWithSolver
        "builtin functions are not evaluated on symbolic keys"
        (do
            elements <- forAll $ genMapSortedVariable intSort genIntegerPattern
            let patMap = asSymbolicPattern (Map.mapKeys mkVar elements)
                expect = Pattern.fromTermLike patMap
            if Map.null elements
                then discard
                else (===) expect =<< evaluate patMap
        )

test_isBuiltin :: [TestTree]
test_isBuiltin =
    [ testCase "isSymbolConcat" $ do
        assertBool ""
            (Map.isSymbolConcat mockHookTools Mock.concatMapSymbol)
        assertBool ""
            (not (Map.isSymbolConcat mockHookTools Mock.aSymbol))
        assertBool ""
            (not (Map.isSymbolConcat mockHookTools Mock.elementMapSymbol))
    , testCase "isSymbolElement" $ do
        assertBool ""
            (Map.isSymbolElement mockHookTools Mock.elementMapSymbol)
        assertBool ""
            (not (Map.isSymbolElement mockHookTools Mock.aSymbol))
        assertBool ""
            (not (Map.isSymbolElement mockHookTools Mock.concatMapSymbol))
    , testCase "isSymbolUnit" $ do
        assertBool ""
            (Map.isSymbolUnit mockHookTools Mock.unitMapSymbol)
        assertBool ""
            (not (Map.isSymbolUnit mockHookTools Mock.aSymbol))
        assertBool ""
            (not (Map.isSymbolUnit mockHookTools Mock.concatMapSymbol))
    ]

mockHookTools :: SmtMetadataTools Hook
mockHookTools = StepperAttributes.hook <$> Mock.metadataTools

-- | Construct a pattern for a map which may have symbolic keys.
asSymbolicPattern
    :: Map (TermLike Variable) (TermLike Variable)
    -> TermLike Variable
asSymbolicPattern result
    | Map.null result =
        applyUnit
    | otherwise =
        foldr1 applyConcat (applyElement <$> Map.toAscList result)
  where
    applyUnit = mkApp mapSort unitMapSymbol []
    applyElement (key, value) = elementMap key value
    applyConcat map1 map2 = concatMap map1 map2

{- | Unify two maps with concrete keys and variable values.
 -}
test_unifyConcrete :: TestTree
test_unifyConcrete =
    testPropertyWithSolver
        "unify maps with concrete keys and symbolic values"
        (do
            let genVariablePair =
                    (,) <$> genIntVariable <*> genIntVariable
                  where
                    genIntVariable =
                        standaloneGen $ mkVar <$> variableGen intSort
            map12 <- forAll (genConcreteMap genVariablePair)
            let map1 = fst <$> map12
                map2 = snd <$> map12
                patExpect = asTermLike $ uncurry mkAnd <$> map12
                patActual = mkAnd (asTermLike map1) (asTermLike map2)
                predicate = mkEquals_ patExpect patActual
            expect <- evaluate patExpect
            actual <- evaluate patActual
            (===) expect actual
            (===) Pattern.top =<< evaluate predicate
        )


-- Given a function to scramble the arguments to concat, i.e.,
-- @id@ or @reverse@, produces a pattern of the form
-- `MapItem(absInt(K:Int), absInt(V:Int)) Rest:Map`, or
-- `Rest:Map MapItem(absInt(K:Int), absInt(V:Int))`, respectively.
selectFunctionPattern
    :: Variable          -- ^key variable
    -> Variable          -- ^value variable
    -> Variable          -- ^map variable
    -> (forall a . [a] -> [a])  -- ^scrambling function
    -> TermLike Variable
selectFunctionPattern keyVar valueVar mapVar permutation  =
    mkApp mapSort concatMapSymbol $ permutation [singleton, mkVar mapVar]
  where
    key = mkApp intSort absIntSymbol  [mkVar keyVar]
    value = mkApp intSort absIntSymbol  [mkVar valueVar]
    singleton = mkApp mapSort elementMapSymbol [ key, value ]

makeElementSelect :: Variable -> Variable -> TermLike Variable
makeElementSelect keyVar valueVar =
    mkApp mapSort elementMapSymbol [mkVar keyVar, mkVar valueVar]

-- Given a function to scramble the arguments to concat, i.e.,
-- @id@ or @reverse@, produces a pattern of the form
-- `MapItem(K:Int, V:Int) Rest:Map`, or `Rest:Map MapItem(K:Int, V:Int)`,
-- respectively.
selectPattern
    :: Variable          -- ^key variable
    -> Variable          -- ^value variable
    -> Variable          -- ^map variable
    -> (forall a . [a] -> [a])  -- ^scrambling function
    -> TermLike Variable
selectPattern keyVar valueVar mapVar permutation  =
    mkApp mapSort concatMapSymbol $ permutation [element, mkVar mapVar]
  where
    element = makeElementSelect keyVar valueVar

addSelectElement
    :: Variable          -- ^key variable
    -> Variable          -- ^value variable
    -> TermLike Variable
    -> TermLike Variable
addSelectElement keyVar valueVar mapPattern  =
    mkApp mapSort concatMapSymbol [element, mapPattern]
  where
    element = makeElementSelect keyVar valueVar

test_unifyEmptyWithEmpty :: TestTree
test_unifyEmptyWithEmpty =
    testPropertyWithSolver "Unify empty map pattern with empty map DV" $ do
        -- Map.empty /\ mapUnit
        (emptyMapDV `unifiesWithMulti` emptyMapPattern) [expect]
        -- mapUnit /\ Map.empty
        (emptyMapPattern `unifiesWithMulti` emptyMapDV) [expect]
  where
    emptyMapDV = asInternal Map.empty
    emptyMapPattern = asTermLike Map.empty
    expect =
        Conditional
            { term = emptyMapDV
            , predicate = makeTruePredicate
            , substitution = Substitution.unsafeWrap []
            }

test_unifySelectFromEmpty :: TestTree
test_unifySelectFromEmpty =
    testPropertyWithSolver "unify an empty map with a selection pattern" $ do
        keyVar <- forAll (standaloneGen $ variableGen intSort)
        valueVar <- forAll (standaloneGen $ variableGen intSort)
        mapVar <- forAll (standaloneGen $ variableGen mapSort)
        let varNames =
                [ variableName keyVar
                , variableName valueVar
                , variableName mapVar
                ]
        Monad.when (varNames /= List.nub varNames) discard
        let selectPat       = selectPattern keyVar valueVar mapVar id
            selectPatRev    = selectPattern keyVar valueVar mapVar reverse
            fnSelectPat     = selectFunctionPattern keyVar valueVar mapVar id
            fnSelectPatRev  =
                selectFunctionPattern keyVar valueVar mapVar reverse
        -- Map.empty /\ MapItem(K:Int, V:Int) Rest:Map
        emptyMap `doesNotUnifyWith` selectPat
        selectPat `doesNotUnifyWith` emptyMap
        -- Map.empty /\ Rest:Map MapItem(K:Int, V:int)
        emptyMap `doesNotUnifyWith` selectPatRev
        selectPatRev `doesNotUnifyWith` emptyMap
        -- Map.empty /\ MapItem(absInt(K:Int), absInt(V:Int)) Rest:Map
        emptyMap `doesNotUnifyWith` fnSelectPat
        fnSelectPat `doesNotUnifyWith` emptyMap
        -- Map.empty /\ Rest:Map MapItem(absInt(K:Int), absInt(V:Int))
        emptyMap `doesNotUnifyWith` fnSelectPatRev
        fnSelectPatRev `doesNotUnifyWith` emptyMap
  where
    emptyMap = asTermLike Map.empty
    doesNotUnifyWith pat1 pat2 =
        (===) Pattern.bottom =<< evaluate (mkAnd pat1 pat2)

test_unifySelectFromSingleton :: TestTree
test_unifySelectFromSingleton =
    testPropertyWithSolver
        "unify a singleton map with a variable selection pattern"
        (do
            concreteKey <- forAll genConcreteIntegerPattern
            value <- forAll genIntegerPattern
            keyVar <- forAll (standaloneGen $ variableGen intSort)
            valueVar <- forAll (standaloneGen $ variableGen intSort)
            mapVar <- forAll (standaloneGen $ variableGen mapSort)
            Monad.when (variableName keyVar == variableName valueVar) discard
            Monad.when (variableName keyVar == variableName mapVar) discard
            Monad.when (variableName valueVar == variableName mapVar) discard
            let selectPat      = selectPattern keyVar valueVar mapVar id
                selectPatRev   = selectPattern keyVar valueVar mapVar reverse
                singleton      = asInternal (Map.singleton concreteKey value)
                keyStepPattern = fromConcrete concreteKey
                expect =
                    Conditional
                        { term = singleton
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [ (mapVar, asInternal Map.empty)
                                , (keyVar, keyStepPattern)
                                , (valueVar, value)
                                ]
                        }
            -- { 5 -> 7 } /\ Item(K:Int, V:Int) Rest:Map
            (singleton `unifiesWith` selectPat) expect
            (selectPat `unifiesWith` singleton) expect
            -- { 5 -> 7 } /\ Rest:Map MapItem(K:Int, V:Int)
            (singleton `unifiesWith` selectPatRev) expect
            (selectPatRev `unifiesWith` singleton) expect
        )

test_unifySelectSingletonFromSingleton :: TestTree
test_unifySelectSingletonFromSingleton =
    testPropertyWithSolver
        "unify a singleton map with a singleton variable selection pattern"
        (do
            concreteKey <- forAll genConcreteIntegerPattern
            value <- forAll genIntegerPattern
            keyVar <- forAll (standaloneGen $ variableGen intSort)
            valueVar <- forAll (standaloneGen $ variableGen intSort)
            Monad.when (variableName keyVar == variableName valueVar) discard
            let
                emptyMapPat    = asTermLike Map.empty
                selectPat      = addSelectElement keyVar valueVar emptyMapPat
                singleton      = asInternal (Map.singleton concreteKey value)
                keyStepPattern = fromConcrete concreteKey
                expect =
                    Conditional
                        { term = singleton
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [ (keyVar, keyStepPattern)
                                , (valueVar, value)
                                ]
                        }
            -- { 5 -> 7 } /\ Item(K:Int, V:Int) Map.unit
            (singleton `unifiesWith` selectPat) expect
            (selectPat `unifiesWith` singleton) expect
        )

test_unifySelectFromSingletonWithoutLeftovers :: TestTree
test_unifySelectFromSingletonWithoutLeftovers =
    testPropertyWithSolver
        "unify a singleton map with an element selection pattern"
        (do
            concreteKey <- forAll genConcreteIntegerPattern
            value <- forAll genIntegerPattern
            keyVar <- forAll (standaloneGen $ variableGen intSort)
            valueVar <- forAll (standaloneGen $ variableGen intSort)
            Monad.when (variableName keyVar == variableName valueVar) discard
            let selectPat = makeElementSelect keyVar valueVar
                singleton =
                    asInternal (Map.singleton concreteKey value)
                keyStepPattern = fromConcrete concreteKey
                expect =
                    Conditional
                        { term = singleton
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [ (keyVar, keyStepPattern)
                                , (valueVar, value)
                                ]
                        }
            -- { 5 -> 7 } /\ Item(K:Int, V:Int)
            (singleton `unifiesWith` selectPat) expect
            (selectPat `unifiesWith` singleton) expect
        )

test_unifySelectFromTwoElementMap :: TestTree
test_unifySelectFromTwoElementMap =
    testPropertyWithSolver
        "unify a two element map with a variable selection pattern"
        (do
            concreteKey1 <- forAll genConcreteIntegerPattern
            value1 <- forAll genIntegerPattern
            concreteKey2 <- forAll genConcreteIntegerPattern
            value2 <- forAll genIntegerPattern
            Monad.when (concreteKey1 == concreteKey2) discard

            keyVar <- forAll (standaloneGen $ variableGen intSort)
            valueVar <- forAll (standaloneGen $ variableGen intSort)
            mapVar <- forAll (standaloneGen $ variableGen mapSort)
            let variables = [keyVar, valueVar, mapVar]
            Monad.when (variables /= List.nub variables) discard

            let selectPat       = selectPattern keyVar valueVar mapVar id
                selectPatRev    = selectPattern keyVar valueVar mapVar reverse
                mapDV =
                    asInternal
                        (Map.fromList
                            [(concreteKey1, value1), (concreteKey2, value2)]
                        )
                keyStepPattern1 = fromConcrete concreteKey1
                keyStepPattern2 = fromConcrete concreteKey2
                expect1 =
                    Conditional
                        { term = mapDV
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [   ( mapVar
                                    , asInternal
                                        (Map.singleton concreteKey2 value2)
                                    )
                                , (keyVar, keyStepPattern1)
                                , (valueVar, value1)
                                ]
                        }
                expect2 =
                    Conditional
                        { term = mapDV
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [   ( mapVar
                                    , asInternal
                                        (Map.singleton concreteKey1 value1)
                                    )
                                , (keyVar, keyStepPattern2)
                                , (valueVar, value2)
                                ]
                        }
            -- { 5 -> 6, 7 -> 8 } /\ Item(K:Int, V:Int) Rest:Map
            (mapDV `unifiesWithMulti` selectPat) [expect1, expect2]
            (selectPat `unifiesWithMulti` mapDV) [expect1, expect2]
            -- { 5 -> 6, 7 -> 8 } /\ Rest:Map Item(K:Int, V:Int)
            (mapDV `unifiesWithMulti` selectPatRev) [expect1, expect2]
            (selectPatRev `unifiesWithMulti` mapDV) [expect1, expect2]
        )

test_unifySelectTwoFromTwoElementMap :: TestTree
test_unifySelectTwoFromTwoElementMap =
    testPropertyWithSolver
        "unify a two element map with a binary variable selection pattern"
        (do
            concreteKey1 <- forAll genConcreteIntegerPattern
            value1 <- forAll genIntegerPattern
            concreteKey2 <- forAll genConcreteIntegerPattern
            value2 <- forAll genIntegerPattern
            Monad.when (concreteKey1 == concreteKey2) discard

            keyVar1 <- forAll (standaloneGen $ variableGen intSort)
            valueVar1 <- forAll (standaloneGen $ variableGen intSort)
            keyVar2 <- forAll (standaloneGen $ variableGen intSort)
            valueVar2 <- forAll (standaloneGen $ variableGen intSort)
            mapVar <- forAll (standaloneGen $ variableGen mapSort)
            let variables = [keyVar1, keyVar2, valueVar1, valueVar2, mapVar]
            Monad.when (variables /= List.nub variables) discard

            let selectPat =
                    addSelectElement keyVar1 valueVar1
                    $ addSelectElement keyVar2 valueVar2
                    $ mkVar mapVar
                mapDV =
                    asInternal
                        (Map.fromList
                            [(concreteKey1, value1), (concreteKey2, value2)]
                        )
                keyStepPattern1 = fromConcrete concreteKey1
                keyStepPattern2 = fromConcrete concreteKey2
                expect1 =
                    Conditional
                        { term = mapDV
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [   ( mapVar
                                    , asInternal Map.empty
                                    )
                                , (keyVar1, keyStepPattern1)
                                , (keyVar2, keyStepPattern2)
                                , (valueVar1, value1)
                                , (valueVar2, value2)
                                ]
                        }
                expect2 =
                    Conditional
                        { term = mapDV
                        , predicate = makeTruePredicate
                        , substitution =
                            Substitution.unsafeWrap
                                [   ( mapVar
                                    , asInternal Map.empty
                                    )
                                , (keyVar1, keyStepPattern2)
                                , (keyVar2, keyStepPattern1)
                                , (valueVar1, value2)
                                , (valueVar2, value1)
                                ]
                        }

            -- { 5 } /\ MapItem(X:Int) Rest:Map
            (mapDV `unifiesWithMulti` selectPat) [expect1, expect2]
            (selectPat `unifiesWithMulti` mapDV) [expect1, expect2]
        )


-- use as (pat1 `unifiesWith` pat2) expect
unifiesWith
    :: HasCallStack
    => TermLike Variable
    -> TermLike Variable
    -> Pattern Variable
    -> PropertyT SMT.SMT ()
unifiesWith pat1 pat2 expected =
    unifiesWithMulti pat1 pat2 [expected]

-- use as (pat1 `unifiesWithMulti` pat2) expect
unifiesWithMulti
    :: HasCallStack
    => TermLike Variable
    -> TermLike Variable
    -> [Pattern Variable]
    -> PropertyT SMT.SMT ()
unifiesWithMulti pat1 pat2 expectedResults = do
    actualResults <- evaluateToList (mkAnd pat1 pat2)
    compareElements (List.sort expectedResults) actualResults
  where
    compareElements [] actuals = [] === actuals
    compareElements expecteds [] =  expecteds === []
    compareElements (expected : expecteds) (actual : actuals) = do
        compareElement expected actual
        compareElements expecteds actuals
    compareElement
        Conditional
            { term = expectedTerm
            , predicate = expectedPredicate
            , substitution = expectedSubstitution
            }
        Conditional
            { term = actualTerm
            , predicate = actualPredicate
            , substitution = actualSubstitution
            }
      = do
        Substitution.toMap expectedSubstitution
            === Substitution.toMap actualSubstitution
        expectedPredicate === actualPredicate
        expectedTerm === actualTerm

{- | Unify a concrete map with symbolic-keyed map.

@
(1, 1 |-> 2) ∧ (x, x |-> v)
@

Iterated unification must turn the symbolic key @x@ into a concrete key by
unifying the first element of the pair. This also requires that Map unification
return a partial result for unifying the second element of the pair.

 -}
test_concretizeKeys :: TestTree
test_concretizeKeys =
    testCaseWithSolver
        "unify a concrete Map with a symbolic Map"
        (\solver -> do
            actual <- evaluateWith solver original
            assertEqualWithExplanation "expected simplified Map" expected actual
        )
  where
    x =
        Variable
            { variableName = testId "x"
            , variableCounter = mempty
            , variableSort = intSort
            }
    v =
        Variable
            { variableName = testId "v"
            , variableCounter = mempty
            , variableSort = intSort
            }
    key = Test.Int.asInternal 1
    symbolicKey = fromConcrete key
    val = Test.Int.asInternal 2
    concreteMap = asTermLike $ Map.fromList [(key, val)]
    symbolic = asSymbolicPattern $ Map.fromList [(mkVar x, mkVar v)]
    original =
        mkAnd
            (mkPair intSort mapSort (Test.Int.asInternal 1) concreteMap)
            (mkPair intSort mapSort (mkVar x) symbolic)
    expected =
        Conditional
            { term =
                mkPair intSort mapSort
                    symbolicKey
                    (asInternal $ Map.fromList [(key, val)])
            , predicate = Predicate.makeTruePredicate
            , substitution =
                Substitution.unsafeWrap
                [ (v, val)
                , (x, symbolicKey)
                ]
            }

{- | Unify a concrete map with symbolic-keyed map in an axiom

Apply the axiom
@
(x, x |-> v) => v
@
to the configuration
@
(1, 1 |-> 2)
@
yielding @2@.

Iterated unification must turn the symbolic key @x@ into a concrete key by
unifying the first element of the pair. This also requires that Map unification
return a partial result for unifying the second element of the pair.

 -}
test_concretizeKeysAxiom :: TestTree
test_concretizeKeysAxiom =
    testCaseWithSolver
        "unify a concrete Map with a symbolic Map in an axiom"
        (\solver -> do
            let pair =
                    mkPair intSort mapSort
                        symbolicKey
                        (asTermLike $ Map.fromList [(key, val)])
            config <- evaluateWith solver pair
            actual <- runStepWith solver config axiom
            assertEqualWithExplanation "expected MAP.lookup" expected actual
        )
  where
    x = mkIntVar (testId "x")
    v = mkIntVar (testId "v")
    key = Test.Int.asInternal 1
    symbolicKey = fromConcrete key
    val = Test.Int.asInternal 2
    symbolicMap = asSymbolicPattern $ Map.fromList [(x, v)]
    axiom =
        RewriteRule RulePattern
            { left = mkPair intSort mapSort x symbolicMap
            , right = v
            , requires = Predicate.makeTruePredicate
            , ensures = Predicate.makeTruePredicate
            , attributes = Default.def
            }
    expected = Right (MultiOr [ pure val ])

hprop_unparse :: Property
hprop_unparse =
    hpropUnparse (asInternal <$> genConcreteMap genValue)
  where
    genValue = Test.Int.asInternal <$> genInteger

-- | Specialize 'Map.asTermLike' to the builtin sort 'mapSort'.
asTermLike :: Map (TermLike Concrete) (TermLike Variable) -> TermLike Variable
asTermLike =
    Reflection.give testMetadataTools Map.asTermLike
    . builtinMap
    . Map.toAscList

-- | Specialize 'Map.asPattern' to the builtin sort 'mapSort'.
asPattern :: Map (TermLike Concrete) (TermLike Variable) -> Pattern Variable
asPattern =
    Reflection.give testMetadataTools Map.asPattern mapSort

-- | Specialize 'Map.asInternal' to the builtin sort 'mapSort'.
asInternal
    :: Map (TermLike Concrete) (TermLike Variable)
    -> TermLike Variable
asInternal = Map.asInternal testMetadataTools mapSort

-- * Constructors

mkIntVar :: Id -> TermLike Variable
mkIntVar variableName =
    mkVar
        Variable
            { variableName, variableCounter = mempty, variableSort = intSort }

mockSubstitutionSimplifier :: PredicateSimplifier
mockSubstitutionSimplifier = PredicateSimplifier return
