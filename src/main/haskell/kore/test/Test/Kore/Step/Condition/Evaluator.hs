module Test.Kore.Step.Condition.Evaluator (test_conditionEvaluator) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( testCase )

import Data.Reflection
       ( give )

import           Kore.AST.Common
                 ( Application (..), AstLocation (..), Id (..), Pattern (..),
                 SymbolOrAlias (..) )
import           Kore.AST.MetaOrObject
import           Kore.AST.PureToKore
                 ( patternKoreToPure )
import           Kore.ASTHelpers
                 ( ApplicationSorts (..) )
import           Kore.Building.AsAst
import           Kore.Building.Patterns
import           Kore.Building.Sorts
import           Kore.Error
import           Kore.IndexedModule.MetadataTools
                 ( SortTools )
import           Kore.MetaML.AST
                 ( CommonMetaPattern )
import           Kore.Predicate.Predicate
                 ( CommonPredicate, Predicate, makeEqualsPredicate,
                 makeFalsePredicate, makeTruePredicate, wrapPredicate )
import qualified Kore.Step.Condition.Evaluator as Predicate
                 ( evaluate )
import           Kore.Step.ExpandedPattern
                 ( CommonPredicateSubstitution,
                 ExpandedPattern (ExpandedPattern),
                 PredicateSubstitution (PredicateSubstitution) )
import           Kore.Step.ExpandedPattern as PredicateSubstitution
                 ( PredicateSubstitution (..) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
                 ( ExpandedPattern (..) )
import           Kore.Step.Simplification.Data
                 ( CommonPureMLPatternSimplifier, SimplificationProof (..) )
import           Kore.Variables.Fresh.IntCounter

import Test.Kore.Comparators ()
import Test.Kore.Step.Simplifier
       ( mockSimplifier )
import Test.Tasty.HUnit.Extensions

test_conditionEvaluator :: [TestTree]
test_conditionEvaluator =
    [ testCase "Or truth table"
        (do
            assertEqualWithExplanation "false or false = false"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaOr SortSort sortSortFalse sortSortFalse)
                    )
                )
            assertEqualWithExplanation "false or true = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaOr SortSort sortSortFalse sortSortTrue)
                    )
                )
            assertEqualWithExplanation "true or false = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaOr SortSort sortSortTrue sortSortFalse)
                    )
                )
            assertEqualWithExplanation "true or true = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaOr SortSort sortSortTrue sortSortTrue)
                    )
                )
        )
    , testCase "Implies truth table"
        (do
            assertEqualWithExplanation "false implies false = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaImplies SortSort sortSortFalse sortSortFalse)
                    )
                )
            assertEqualWithExplanation "false implies true = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaImplies SortSort sortSortFalse sortSortTrue)
                    )
                )
            assertEqualWithExplanation "true implies false = false"
                (predicateSubstitution makeFalsePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaImplies SortSort sortSortTrue sortSortFalse)
                    )
                )
            assertEqualWithExplanation "true implies true = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaImplies SortSort sortSortTrue sortSortTrue)
                    )
                )
        )
    , testCase "Iff truth table"
        (do
            assertEqualWithExplanation "false iff false = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaIff SortSort sortSortFalse sortSortFalse)
                    )
                )
            assertEqualWithExplanation "false iff true = false"
                (predicateSubstitution makeFalsePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaIff SortSort sortSortFalse sortSortTrue)
                    )
                )
            assertEqualWithExplanation "true iff false = false"
                (predicateSubstitution makeFalsePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaIff SortSort sortSortTrue sortSortFalse)
                    )
                )
            assertEqualWithExplanation "true iff true = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaIff SortSort sortSortTrue sortSortTrue)
                    )
                )
        )
    , testCase "Not truth table"
        (do
            assertEqualWithExplanation "not false = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaNot SortSort sortSortFalse)
                    )
                )
            assertEqualWithExplanation "not true = false"
                (predicateSubstitution makeFalsePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaNot SortSort sortSortTrue)
                    )
                )
        )
    , let
        falseChild = metaNot SortSort sortSortTrue
        trueChild = metaNot SortSort sortSortFalse
      in
        testCase "Evaluates children"
        (do
            assertEqualWithExplanation "true and <true-child> = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaAnd SortSort sortSortTrue trueChild)
                    )
                )
            assertEqualWithExplanation "<true-child> and true = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaAnd SortSort trueChild sortSortTrue)
                    )
                )
            assertEqualWithExplanation "true and <false-child> = false"
                (predicateSubstitution makeFalsePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaAnd SortSort sortSortTrue falseChild)
                    )
                )
            assertEqualWithExplanation "false or <true-child> = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaOr SortSort sortSortFalse trueChild)
                    )
                )
            assertEqualWithExplanation "<true-child> or false = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaOr SortSort trueChild sortSortFalse)
                    )
                )
            assertEqualWithExplanation "false or <false-child> = false"
                (predicateSubstitution makeFalsePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaOr SortSort sortSortFalse falseChild)
                    )
                )
            assertEqualWithExplanation "true implies <true-child> = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaImplies SortSort sortSortTrue trueChild)
                    )
                )
            assertEqualWithExplanation "<true-child> implies true = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaImplies SortSort trueChild sortSortTrue)
                    )
                )
            assertEqualWithExplanation "true implies <false-child> = false"
                (predicateSubstitution makeFalsePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaImplies SortSort sortSortTrue falseChild)
                    )
                )
            assertEqualWithExplanation "true iff <true-child> = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaIff SortSort sortSortTrue trueChild)
                    )
                )
            assertEqualWithExplanation "<true-child> iff true = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaIff SortSort trueChild sortSortTrue)
                    )
                )
            assertEqualWithExplanation "true iff <false-child> = false"
                (predicateSubstitution makeFalsePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaIff SortSort sortSortTrue falseChild)
                    )
                )
            assertEqualWithExplanation "not <true-child> = false"
                (predicateSubstitution makeFalsePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaNot SortSort trueChild)
                    )
                )
            assertEqualWithExplanation "not <false-child> = true"
                (predicateSubstitution makeTruePredicate)
                (evaluateCondition
                    (mockSimplifier [])
                    (asPredicate
                        (metaNot SortSort falseChild)
                    )
                )
        )
    ,  let
        fOfX = metaF (x PatternSort)
        gOfX = metaG (x PatternSort)
        hOfX = metaH (x PatternSort)
      in
        testCase "Evaluates equals"
            (do
                -- TODO: Uncomment after implementing equality evaluation
                -- for constructors.
                {-
                assertEqualWithExplanation "f(x) /= g(x)"
                    makeFalsePredicate
                    (evaluateCondition
                        (mockSimplifier [])
                        (asPredicate
                            (metaEquals (ResultSort SortSort) PatternSort
                                fOfX
                                gOfX
                            )
                        )
                    )
                -}
                assertEqualWithExplanation
                    "f(x) = g(x) if f(x) => h(x) and g(x) => h(x)"
                    (predicateSubstitution
                        makeTruePredicate
                    )
                    (evaluateCondition
                        (mockSimplifier
                            [   ( asPureMetaPattern fOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate = makeTruePredicate
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ,   ( asPureMetaPattern gOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate = makeTruePredicate
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ]
                        )
                        (asPredicate
                            (metaEquals (ResultSort SortSort) PatternSort
                                (metaF (x PatternSort))
                                (metaG (x PatternSort))
                            )
                        )
                    )
                assertEqualWithExplanation
                    ("f(x) /= g(x) if f(x) => h(x) and g(x) => h(x) "
                        ++ "but incompatible f condition")
                    (predicateSubstitution
                        makeFalsePredicate
                    )
                    (evaluateCondition
                        (mockSimplifier
                            [   ( asPureMetaPattern fOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate = makeFalsePredicate
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ,   ( asPureMetaPattern gOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate = makeTruePredicate
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ]
                        )
                        (asPredicate
                            (metaEquals (ResultSort SortSort) PatternSort
                                (metaF (x PatternSort))
                                (metaG (x PatternSort))
                            )
                        )
                    )
                assertEqualWithExplanation
                    ("f(x) /= g(x) if f(x) => h(x) and g(x) => h(x) "
                        ++ "but incompatible g condition")
                    (predicateSubstitution
                        makeFalsePredicate
                    )
                    (evaluateCondition
                        (mockSimplifier
                            [   ( asPureMetaPattern fOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate = makeTruePredicate
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ,   ( asPureMetaPattern gOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate = makeFalsePredicate
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ]
                        )
                        (makeEquals
                            (metaF (x PatternSort))
                            (metaG (x PatternSort))
                        )
                    )
                assertEqualWithExplanation
                    (  "f(x) = g(x) => x=h1(x) "
                    ++ "if f(x) => h(x) /\\ x=h1(x) and g(x) => h(x) "
                    )
                    (predicateSubstitution
                        (makeEquals (x PatternSort) (metaH1 (x PatternSort)))
                    )
                    (evaluateCondition
                        (mockSimplifier
                            [   ( asPureMetaPattern fOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate =
                                                makeEquals
                                                    (x PatternSort)
                                                    (metaH1 (x PatternSort))
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ,   ( asPureMetaPattern gOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate = makeTruePredicate
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ]
                        )
                        (asPredicate
                            (metaEquals (ResultSort SortSort) PatternSort
                                (metaF (x PatternSort))
                                (metaG (x PatternSort))
                            )
                        )
                    )
                assertEqualWithExplanation
                    (  "f(x) = g(x) => x=h1(x) "
                    ++ "if f(x) => h(x) and g(x) => h(x) /\\ x=h1(x)"
                    )
                    (predicateSubstitution
                        (makeEquals (x PatternSort) (metaH1 (x PatternSort)))
                    )
                    (evaluateCondition
                        (mockSimplifier
                            [   ( asPureMetaPattern fOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate = makeTruePredicate
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ,   ( asPureMetaPattern gOfX
                                ,   (   [ ExpandedPattern
                                            { term = asPureMetaPattern hOfX
                                            , predicate =
                                                makeEquals
                                                    (x PatternSort)
                                                    (metaH1 (x PatternSort))
                                            , substitution = []
                                            }
                                        ]
                                    , SimplificationProof
                                    )
                                )
                            ]
                        )
                        (asPredicate
                            (metaEquals (ResultSort SortSort) PatternSort
                                (metaF (x PatternSort))
                                (metaG (x PatternSort))
                            )
                        )
                    )
            )
    ]

x :: MetaSort sort => sort -> MetaVariable sort
x = metaVariable "#x" AstLocationTest

sortSortTrue :: PatternTop SortSort Meta
sortSortTrue = metaTop SortSort

sortSortFalse :: PatternBottom SortSort Meta
sortSortFalse = metaBottom SortSort

fSymbol :: SymbolOrAlias Meta
fSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#f" AstLocationTest
    , symbolOrAliasParams = []
    }

newtype MetaF p1 = MetaF p1
instance (MetaPattern PatternSort p1)
    => ProperPattern Meta PatternSort (MetaF p1)
  where
    asProperPattern (MetaF p1) =
        ApplicationPattern Application
            { applicationSymbolOrAlias = fSymbol
            , applicationChildren = [asAst p1]
            }
metaF
    :: (MetaPattern PatternSort p1)
    => p1 -> MetaF p1
metaF = MetaF

gSymbol :: SymbolOrAlias Meta
gSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#g" AstLocationTest
    , symbolOrAliasParams = []
    }

newtype MetaG p1 = MetaG p1
instance (MetaPattern PatternSort p1)
    => ProperPattern Meta PatternSort (MetaG p1)
  where
    asProperPattern (MetaG p1) =
        ApplicationPattern Application
            { applicationSymbolOrAlias = gSymbol
            , applicationChildren = [asAst p1]
            }
metaG
    :: (MetaPattern PatternSort p1)
    => p1 -> MetaG p1
metaG = MetaG


hSymbol :: SymbolOrAlias Meta
hSymbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#h" AstLocationTest
    , symbolOrAliasParams = []
    }

newtype MetaH p1 = MetaH p1
instance (MetaPattern PatternSort p1)
    => ProperPattern Meta PatternSort (MetaH p1)
  where
    asProperPattern (MetaH p1) =
        ApplicationPattern Application
            { applicationSymbolOrAlias = hSymbol
            , applicationChildren = [asAst p1]
            }
metaH
    :: (MetaPattern PatternSort p1)
    => p1 -> MetaH p1
metaH = MetaH


h1Symbol :: SymbolOrAlias Meta
h1Symbol = SymbolOrAlias
    { symbolOrAliasConstructor = Id "#h1" AstLocationTest
    , symbolOrAliasParams = []
    }

newtype MetaH1 p1 = MetaH1 p1
instance (MetaPattern PatternSort p1)
    => ProperPattern Meta PatternSort (MetaH1 p1)
  where
    asProperPattern (MetaH1 p1) =
        ApplicationPattern Application
            { applicationSymbolOrAlias = h1Symbol
            , applicationChildren = [asAst p1]
            }
metaH1
    :: (MetaPattern PatternSort p1)
    => p1 -> MetaH1 p1
metaH1 = MetaH1

makeEquals
    :: (ProperPattern Meta sort patt1, ProperPattern Meta sort patt2)
    => patt1 -> patt2 -> CommonPredicate Meta
makeEquals patt1 patt2 =
    give mockSortTools
        (makeEqualsPredicate
            (asPureMetaPattern patt1)
            (asPureMetaPattern patt2)
        )

mockSortTools :: SortTools Meta
mockSortTools = const ApplicationSorts
    { applicationSortsOperands = [asAst PatternSort, asAst PatternSort]
    , applicationSortsResult = asAst PatternSort
    }

asPredicate :: ProperPattern Meta sort patt => patt -> CommonPredicate Meta
asPredicate = wrapPredicate . asPureMetaPattern

asPureMetaPattern
    :: ProperPattern Meta sort patt => patt -> CommonMetaPattern
asPureMetaPattern patt =
    case patternKoreToPure Meta (asAst patt) of
        Left err  -> error (printError err)
        Right pat -> pat

evaluateCondition
    :: CommonPureMLPatternSimplifier Meta
    -> CommonPredicate Meta
    -> CommonPredicateSubstitution Meta
evaluateCondition
    functionEvaluator
    condition
  =
    fst $ fst $ runIntCounter
        (give mockSortTools
            (Predicate.evaluate functionEvaluator condition)
        )
        0

predicateSubstitution
    :: Predicate level variable
    -> PredicateSubstitution level variable
predicateSubstitution predicate =
    PredicateSubstitution
        { predicate = predicate
        , substitution = []
        }
