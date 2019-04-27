module Test.Kore.Step.Simplification.StringLiteral
    ( test_stringLiteralSimplification
    ) where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( testCase )

import           Kore.AST.Pure
import           Kore.AST.Valid
import           Kore.Predicate.Predicate
                 ( makeTruePredicate )
import           Kore.Step.Pattern
                 ( Conditional (..) )
import qualified Kore.Step.Pattern.Or as Or
import qualified Kore.Step.Representation.MultiOr as MultiOr
                 ( make )
import           Kore.Step.Simplification.StringLiteral
                 ( simplify )

import Test.Kore.Comparators ()
import Test.Tasty.HUnit.Extensions

test_stringLiteralSimplification :: [TestTree]
test_stringLiteralSimplification =
    [ testCase "StringLiteral evaluates to StringLiteral"
        (assertEqualWithExplanation ""
            (MultiOr.make
                [ Conditional
                    { term = mkStringLiteral "a"
                    , predicate = makeTruePredicate
                    , substitution = mempty
                    }
                ]
            )
            (evaluate
                (StringLiteral "a")
            )
        )
    ]

evaluate
    ::  ( MetaOrObject Meta)
    => StringLiteral
    -> Or.Pattern Object Variable
evaluate stringLiteral =
    case simplify stringLiteral of
        (result, _proof) -> result
