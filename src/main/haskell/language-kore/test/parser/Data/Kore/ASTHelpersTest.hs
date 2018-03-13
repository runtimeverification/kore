module Data.Kore.ASTHelpersTest (astHelperTests) where

import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (assertEqual, testCase)

import           Data.Kore.AST.Common
import           Data.Kore.AST.Kore
import           Data.Kore.ASTHelpers
import           Data.Kore.Error

astHelperTests :: TestTree
astHelperTests =
    testGroup
        "Ast Helpers Tests"
        [ symbolOrAliasSortsTests ]

symbolOrAliasSortsTests :: TestTree
symbolOrAliasSortsTests =
    testGroup "symbolOrAliasSorts"
    [ testCase "simple result sort"
        (assertEqual "Expecting success"
            (applicationSorts [] simpleSortActual)
            (symbolOrAliasSorts
                []
                (symbolSentence [] [] simpleSortActual)
            )
        )
    , testCase "parameterized result sort"
        (assertEqual "Expecting success"
            (applicationSorts [] simpleSortActual)
            (symbolOrAliasSorts
                [simpleSortActual]
                (symbolSentence [sortVariable'] [] sortVariableSort')
            )
        )
    , testCase "complex parameterized result sort"
        (assertEqual "Expecting success"
            (applicationSorts [] complexSortActualSort)
            (symbolOrAliasSorts
                [simpleSortActual]
                (symbolSentence [sortVariable'] [] complexSortActualParam)
            )
        )
    , testCase "simple argument sort"
        (assertEqual "Expecting success"
            (applicationSorts [simpleSortActual] simpleSortActual)
            (symbolOrAliasSorts
                []
                (symbolSentence [] [simpleSortActual] simpleSortActual)
            )
        )
    , testCase "parameterized argument sort"
        (assertEqual "Expecting success"
            (applicationSorts [simpleSortActual] simpleSortActual)
            (symbolOrAliasSorts
                [simpleSortActual]
                (symbolSentence
                    [sortVariable'] [sortVariableSort'] simpleSortActual)
            )
        )
    , testCase "complex argument sort"
        (assertEqual "Expecting success"
            (applicationSorts [complexSortActualSort] simpleSortActual)
            (symbolOrAliasSorts
                [simpleSortActual]
                (symbolSentence
                    [sortVariable'] [complexSortActualParam] simpleSortActual)
            )
        )
    , testCase "sort variable not found"
        (assertEqual "Expecting error"
            (koreFail "Sort variable not found: 'sv'.")
            (symbolOrAliasSorts
                [simpleSortActual]
                (symbolSentence
                    [anotherSortVariable'] [sortVariableSort'] simpleSortActual)
            )
        )
    , testCase "more sorts than the declaration"
        (assertEqual "Expecting error"
            (koreFail "Application uses more sorts than the declaration.")
            (symbolOrAliasSorts
                [simpleSortActual, simpleSortActual]
                (symbolSentence
                    [sortVariable'] [simpleSortActual] simpleSortActual)
            )
        )
    , testCase "less sorts than the declaration"
        (assertEqual "Expecting error"
            (koreFail "Application uses less sorts than the declaration.")
            (symbolOrAliasSorts
                []
                (symbolSentence
                    [sortVariable'] [simpleSortActual] simpleSortActual)
            )
        )
    ]
  where
    simpleSortActual = sortActual "sa" []
    sortVariable' = sortVariable "sv"
    anotherSortVariable' = sortVariable "asv"
    sortVariableSort' = sortVariableSort "sv"
    complexSortActualParam = sortActual "sa" [sortVariableSort']
    complexSortActualSort = sortActual "sa" [simpleSortActual]

sortVariable :: String -> SortVariable level
sortVariable name =
    SortVariable { getSortVariable = Id name }

sortVariableSort :: String -> Sort level
sortVariableSort name =
    SortVariableSort (sortVariable name)

sortActual :: String -> [Sort level] -> Sort level
sortActual name sorts =
    SortActualSort SortActual
        { sortActualName = Id name
        , sortActualSorts = sorts
        }

applicationSorts
    :: [Sort level] -> Sort level -> Either b (ApplicationSorts level)
applicationSorts operandSorts resultSort =
    Right ApplicationSorts
        { applicationSortsOperands = operandSorts
        , applicationSortsResult = resultSort
        }

symbolSentence
    :: [SortVariable level]
    -> [Sort level]
    -> Sort level
    -> KoreSentenceSymbol level
symbolSentence sortParameters operandSorts resultSort =
    SentenceSymbol
        { sentenceSymbolSymbol     = Symbol
            { symbolConstructor = Id "symb"
            , symbolParams = sortParameters
            }
        , sentenceSymbolSorts      = operandSorts
        , sentenceSymbolResultSort = resultSort
        , sentenceSymbolAttributes = Attributes []
        }
