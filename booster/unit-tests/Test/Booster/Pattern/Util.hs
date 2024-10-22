module Test.Booster.Pattern.Util (
    test_freshen,
) where

import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit

import Booster.Pattern.Base
import Booster.Pattern.Util

test_freshen :: TestTree
test_freshen =
    testGroup
        "Variable renaming"
        [ testGroup
            "Name-embedded variable counter increment"
            [ testCase "No counter gets 0 as counter" $ incrementNameCounter "Var'Ques'X" @?= "Var'Ques'X0"
            , testCase "0 counter gets incremented" $ incrementNameCounter "Var'Ques'X0" @?= "Var'Ques'X1"
            ]
        , testGroup
            "Variable name freshening by updating counter"
            [ testCase "No renaming when there's nothing to rename" $
                freshenVar (Variable someSort "X") (Set.fromList []) @?= Variable someSort "X"
            , testCase "One variable gets renamed" $
                freshenVar (Variable someSort "X1") (Set.fromList [Variable someSort "X1"])
                    @?= Variable someSort "X2"
            ]
        ]

----------------------------------------

someSort :: Sort
someSort = SortApp "SomeSort" []
