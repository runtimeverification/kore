{-# LANGUAGE QuasiQuotes #-}

module Test.Booster.Pattern.Substitution (
    test_subst,
) where

import Data.Map qualified as Map
import Test.Tasty
import Test.Tasty.HUnit

import Booster.Definition.Attributes.Base
import Booster.Pattern.Base
import Booster.Pattern.Substitution
import Booster.Syntax.Json.Internalise (trm)

test_subst :: TestTree
test_subst =
    testGroup
        "Substitution"
        [ testGroup
            "Application"
            [ test
                "con1(X)[con1(Y)/X]"
                ("X" |-> [trm| con1{}(Y:SomeSort{}) |])
                ([trm| con1{}(X:SomeSort{}) |])
                ([trm| con1{}(con1{}(Y:SomeSort{})) |])
            , test
                "con1(X)/\\con1(X)[con1(Y)/X]"
                ("X" |-> [trm| con1{}(Y:SomeSort{}) |])
                (AndTerm ([trm| con1{}(X:SomeSort{}) |]) ([trm| con1{}(X:SomeSort{}) |]))
                (AndTerm ([trm| con1{}(con1{}(Y:SomeSort{})) |]) ([trm| con1{}(con1{}(Y:SomeSort{})) |]))
            , test
                "con1(X)/\\con1(Y)[con1(Y)/X]"
                ("X" |-> [trm| con1{}(Y:SomeSort{}) |])
                (AndTerm ([trm| con1{}(X:SomeSort{}) |]) ([trm| con1{}(Y:SomeSort{}) |]))
                (AndTerm ([trm| con1{}(con1{}(Y:SomeSort{})) |]) ([trm| con1{}(Y:SomeSort{}) |]))
            ]
        , testGroup
            "Composition"
            [ testCase
                "compose is idempotent"
                $ (("X" |-> [trm| con1{}(Y:SomeSort{}) |]) `compose` ("X" |-> [trm| con1{}(Y:SomeSort{}) |]))
                    @?= ("X" |-> [trm| con1{}(Y:SomeSort{}) |])
            , testCase
                "compose is transitive and saturating"
                $ (("X" |-> [trm| Z:SomeSort{} |]) `compose` ("Y" |-> [trm| X:SomeSort{} |]))
                    @?= ("X" |-> [trm| Z:SomeSort{} |])
                        <> ("Y" |-> [trm| Z:SomeSort{} |])
            ]
        ]

----------------------------------------
-- Test fixture
test :: String -> Map.Map Variable Term -> Term -> Term -> TestTree
test name substitutions term expected =
    testCase name $ substituteInTerm substitutions term @?= expected

someSort :: Sort
someSort = SortApp "SomeSort" []

(|->) :: VarName -> Term -> Substitution
(|->) v t = Map.fromList [(Variable someSort v, t)]

asConstructor :: SymbolAttributes
asConstructor =
    SymbolAttributes
        Constructor
        IsNotIdem
        IsNotAssoc
        IsNotMacroOrAlias
        CanBeEvaluated
        Nothing
        Nothing
        Nothing

con1 :: Symbol
con1 =
    Symbol
        { name = "con1"
        , sortVars = []
        , resultSort = someSort
        , argSorts = [someSort]
        , attributes = asConstructor
        }
