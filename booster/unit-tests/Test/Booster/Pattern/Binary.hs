{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Booster.Pattern.Binary (
    test_BinaryRoundTrips,
) where

import Booster.Definition.Attributes.Base
import Booster.Pattern.Base
import Booster.Pattern.Binary
import Booster.Pattern.Bool
import Booster.Pattern.Util (sortOfTerm)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import Data.Set qualified as Set
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Booster.Syntax.Json (upTo)
import Test.Tasty
import Test.Tasty.Hedgehog

genSort :: Gen Sort
genSort =
    Gen.recursive
        Gen.choice
        [SortVar <$> Gen.utf8 (Range.linear 1 32) Gen.alphaNum]
        [SortApp <$> Gen.utf8 (Range.linear 1 32) Gen.alphaNum <*> upTo 10 genSort]

genVariable :: Gen Variable
genVariable = Variable <$> genSort <*> Gen.utf8 (Range.linear 1 32) Gen.alphaNum

{- | In order to test the binary decoding without needing a proper definition,
   we generate symbols with dummy sort and attributes.
-}
genSymbolUnknownSort :: Gen Symbol
genSymbolUnknownSort =
    ( \name ->
        Symbol name [] [] (SortApp "UNKNOWN" []) $
            SymbolAttributes
                (Function Partial)
                IsNotIdem
                IsNotAssoc
                IsNotMacroOrAlias
                CannotBeEvaluated
                Nothing
                Nothing
                Nothing
    )
        <$> Gen.utf8 (Range.linear 0 32) Gen.alphaNum

genTerm :: Gen Term
genTerm =
    Gen.recursive
        Gen.choice
        [Var <$> genVariable]
        [ AndTerm <$> genTerm <*> genTerm
        , SymbolApplication <$> genSymbolUnknownSort <*> pure [] <*> upTo 10 genTerm
        , DomainValue <$> genSort <*> Gen.bytes (Range.linear 1 1000)
        , (\target t -> Injection (sortOfTerm t) target t) <$> genSort <*> genTerm
        ]

genPredicate :: Gen Predicate
genPredicate =
    Predicate
        <$> Gen.choice [pure TrueBool, pure FalseBool, genTerm]

genPattern :: Gen Pattern
genPattern = (\t cs -> Pattern t cs mempty mempty) <$> genTerm <*> (Set.fromList <$> upTo 10 genPredicate)

test_BinaryRoundTrips :: [TestTree]
test_BinaryRoundTrips =
    [ testProperty "Term -> binary -> Term" binaryTermRoundTrip
    , testProperty "Pattern -> binary -> Pattern" binaryPatternRoundTrip
    ]

binaryTermRoundTrip :: Property
binaryTermRoundTrip =
    property $ do
        term <- forAll genTerm
        let binary = runPut $ encodeMagicHeaderAndVersion (Version 1 1 0) >> encodeTerm term
            term' = runGet (decodeTerm' Nothing) binary
        term === term'

binaryPatternRoundTrip :: Property
binaryPatternRoundTrip =
    property $ do
        pat <- forAll genPattern
        let binary = runPut $ encodeMagicHeaderAndVersion (Version 1 1 0) >> encodePattern pat
            pat' = runGet (decodePattern Nothing) binary
        pat === pat'
