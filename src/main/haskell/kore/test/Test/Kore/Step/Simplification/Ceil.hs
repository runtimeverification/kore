module Test.Kore.Step.Simplification.Ceil
    ( test_ceilSimplification
    ) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( testCase )

import Data.Reflection
       ( give )

import           Kore.AST.Common
                 ( Ceil (..), Sort (..) )
import           Kore.AST.MetaOrObject
import           Kore.ASTUtils.SmartConstructors
                 ( mkBottom, mkTop )
import           Kore.ASTUtils.SmartPatterns
                 ( pattern Bottom_ )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.Predicate.Predicate
                 ( makeAndPredicate, makeCeilPredicate, makeEqualsPredicate,
                 makeTruePredicate )
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern, ExpandedPattern (ExpandedPattern) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
                 ( ExpandedPattern (..), bottom, top )
import           Kore.Step.OrOfExpandedPattern
                 ( CommonOrOfExpandedPattern, OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import qualified Kore.Step.Simplification.Ceil as Ceil
                 ( makeEvaluate, simplify )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )

import           Test.Kore.Comparators ()
import qualified Test.Kore.IndexedModule.MockMetadataTools as Mock
                 ( makeMetadataTools, makeSortTools )
import qualified Test.Kore.Step.MockSymbols as Mock
import           Test.Tasty.HUnit.Extensions

test_ceilSimplification :: [TestTree]
test_ceilSimplification = give mockSortTools
    [ testCase "Ceil - or distribution"
        -- ceil(a or b) = (top and ceil(a)) or (top and ceil(b))
        (assertEqualWithExplanation ""
            (OrOfExpandedPattern.make
                [ ExpandedPattern
                    { term = mkTop
                    , predicate = makeCeilPredicate somethingOfA
                    , substitution = []
                    }
                , ExpandedPattern
                    { term = mkTop
                    , predicate = makeCeilPredicate somethingOfB
                    , substitution = []
                    }
                ]
            )
            (evaluate mockMetadataTools
                (makeCeil
                    [somethingOfAExpanded, somethingOfBExpanded]
                )
            )
        )
    , testCase "Ceil - bool operations"
        (do
            -- ceil(top) = top
            assertEqualWithExplanation "ceil(top)"
                (OrOfExpandedPattern.make
                    [ ExpandedPattern
                        { term = mkTop
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    ]
                )
                (evaluate mockMetadataTools
                    (makeCeil
                        [ExpandedPattern.top]
                    )
                )
            -- ceil(bottom) = bottom
            assertEqualWithExplanation "ceil(bottom)"
                (OrOfExpandedPattern.make
                    []
                )
                (evaluate mockMetadataTools
                    (makeCeil
                        []
                    )
                )
        )
    , testCase "expanded Ceil - bool operations"
        (do
            -- ceil(top) = top
            assertEqualWithExplanation "ceil(top)"
                (OrOfExpandedPattern.make
                    [ ExpandedPattern
                        { term = mkTop
                        , predicate = makeTruePredicate
                        , substitution = []
                        }
                    ]
                )
                (makeEvaluate mockMetadataTools
                    (ExpandedPattern.top :: CommonExpandedPattern Object domain)
                )
            -- ceil(bottom) = bottom
            assertEqualWithExplanation "ceil(bottom)"
                (OrOfExpandedPattern.make
                    []
                )
                (makeEvaluate mockMetadataTools
                    (ExpandedPattern.bottom :: CommonExpandedPattern Object domain)
                )
        )
    , testCase "ceil with predicates and substitutions"
        -- if term is not functional, then
        -- ceil(term and predicate and subst)
        --     = top and (ceil(term) and predicate) and subst
        (assertEqualWithExplanation "ceil(something(a) and equals(f(a), g(a)))"
            (OrOfExpandedPattern.make
                [ ExpandedPattern
                    { term = mkTop
                    , predicate =
                        fst $ makeAndPredicate
                            (makeEqualsPredicate fOfA gOfA)
                            (makeCeilPredicate somethingOfA)
                    , substitution = [(Mock.x, fOfB)]
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                ExpandedPattern
                    { term = somethingOfA
                    , predicate = makeEqualsPredicate fOfA gOfA
                    , substitution = [(Mock.x, fOfB)]
                    }
            )
        )
    , testCase "ceil with constructors"
        -- if term is a constructor(params), then
        -- ceil(term and predicate and subst)
        --     = top and (ceil(params) and predicate) and subst
        (assertEqualWithExplanation
            "ceil(constr(something(a), something(b)) and equals(f(a), g(a)))"
            (OrOfExpandedPattern.make
                [ ExpandedPattern
                    { term = mkTop
                    , predicate =
                        fst $ makeAndPredicate
                            (makeEqualsPredicate fOfA gOfA)
                            (fst $ makeAndPredicate
                                (makeCeilPredicate somethingOfA)
                                (makeCeilPredicate somethingOfB)
                            )
                    , substitution = [(Mock.x, fOfB)]
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                ExpandedPattern
                    { term = Mock.constr20 somethingOfA somethingOfB
                    , predicate = makeEqualsPredicate fOfA gOfA
                    , substitution = [(Mock.x, fOfB)]
                    }
            )
        )
    , testCase "ceil with functional symbols"
        -- if term is a functional(params), then
        -- ceil(term and predicate and subst)
        --     = top and (ceil(params) and predicate) and subst
        (assertEqualWithExplanation
            "ceil(functional(something(a), something(b)) and eq(f(a), g(a)))"
            (OrOfExpandedPattern.make
                [ ExpandedPattern
                    { term = mkTop
                    , predicate =
                        fst $ makeAndPredicate
                            (makeEqualsPredicate fOfA gOfA)
                            (fst $ makeAndPredicate
                                (makeCeilPredicate somethingOfA)
                                (makeCeilPredicate somethingOfB)
                            )
                    , substitution = [(Mock.x, fOfB)]
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                ExpandedPattern
                    { term = Mock.functional20 somethingOfA somethingOfB
                    , predicate = makeEqualsPredicate fOfA gOfA
                    , substitution = [(Mock.x, fOfB)]
                    }
            )
        )
    , testCase "ceil with functional terms"
        -- if term is functional, then
        -- ceil(term and predicate and subst)
        --     = top and predicate and subst
        (assertEqualWithExplanation
            "ceil(functional and eq(f(a), g(a)))"
            (OrOfExpandedPattern.make
                [ ExpandedPattern
                    { term = mkTop
                    , predicate = makeEqualsPredicate fOfA gOfA
                    , substitution = [(Mock.x, fOfB)]
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                ExpandedPattern
                    { term = Mock.a
                    , predicate = makeEqualsPredicate fOfA gOfA
                    , substitution = [(Mock.x, fOfB)]
                    }
            )
        )
    , testCase "ceil with functional composition"
        -- if term is functional(non-funct, non-funct), then
        -- ceil(term and predicate and subst)
        --     = top and
        --       ceil(non-funct) and ceil(non-funct) and predicate and
        --       subst
        (assertEqualWithExplanation
            "ceil(functional(non-funct, non-funct) and eq(f(a), g(a)))"
            (OrOfExpandedPattern.make
                [ ExpandedPattern
                    { term = mkTop
                    , predicate =
                        fst $ makeAndPredicate
                            (makeEqualsPredicate fOfA gOfA)
                            (fst $ makeAndPredicate
                                (makeCeilPredicate fOfA)
                                (makeCeilPredicate fOfB)
                            )
                    , substitution = [(Mock.x, fOfB)]
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                ExpandedPattern
                    { term = Mock.functional20 fOfA fOfB
                    , predicate = makeEqualsPredicate fOfA gOfA
                    , substitution = [(Mock.x, fOfB)]
                    }
            )
        )
    , testCase "ceil with constructor composition"
        -- if term is constr(non-funct, non-funct), then
        -- ceil(term and predicate and subst)
        --     = top and
        --       ceil(non-funct) and ceil(non-funct) and predicate and
        --       subst
        (assertEqualWithExplanation
            "ceil(constr(non-funct, non-funct) and eq(f(a), g(a)))"
            (OrOfExpandedPattern.make
                [ ExpandedPattern
                    { term = mkTop
                    , predicate =
                        fst $ makeAndPredicate
                            (makeEqualsPredicate fOfA gOfA)
                            (fst $ makeAndPredicate
                                (makeCeilPredicate fOfA)
                                (makeCeilPredicate fOfB)
                            )
                    , substitution = [(Mock.x, fOfB)]
                    }
                ]
            )
            (makeEvaluate mockMetadataTools
                ExpandedPattern
                    { term = Mock.constr20 fOfA fOfB
                    , predicate = makeEqualsPredicate fOfA gOfA
                    , substitution = [(Mock.x, fOfB)]
                    }
            )
        )
    -- ceil moves predicates and substitutions up
    ]
  where
    fOfA = give mockSortTools $ Mock.f Mock.a
    fOfB = give mockSortTools $ Mock.f Mock.b
    gOfA = give mockSortTools $ Mock.g Mock.a
    somethingOfA = give mockSortTools $ Mock.plain10 Mock.a
    somethingOfB = give mockSortTools $ Mock.plain10 Mock.b
    somethingOfAExpanded = ExpandedPattern
        { term = somethingOfA
        , predicate = makeTruePredicate
        , substitution = []
        }
    somethingOfBExpanded = ExpandedPattern
        { term = somethingOfB
        , predicate = makeTruePredicate
        , substitution = []
        }
    mockSortTools = Mock.makeSortTools Mock.sortToolsMapping
    mockMetadataTools =
        Mock.makeMetadataTools mockSortTools Mock.attributesMapping

makeCeil
    :: [ExpandedPattern Object domain variable]
    -> Ceil Object (OrOfExpandedPattern Object domain variable)
makeCeil patterns =
    Ceil
        { ceilOperandSort = testSort
        , ceilResultSort  = testSort
        , ceilChild       = OrOfExpandedPattern.make patterns
        }

testSort :: Sort Object
testSort =
    case mkBottom of
        Bottom_ sort -> sort
        _ -> error "unexpected"

evaluate
    ::  ( MetaOrObject level
        )
    => MetadataTools level StepperAttributes
    -> Ceil level (CommonOrOfExpandedPattern level domain)
    -> CommonOrOfExpandedPattern level domain
evaluate tools ceil =
    case Ceil.simplify tools ceil of
        (result, _proof) -> result


makeEvaluate
    ::  ( MetaOrObject level
        )
    => MetadataTools level StepperAttributes
    -> CommonExpandedPattern level domain
    -> CommonOrOfExpandedPattern level domain
makeEvaluate tools child =
    case Ceil.makeEvaluate tools child of
        (result, _proof) -> result
