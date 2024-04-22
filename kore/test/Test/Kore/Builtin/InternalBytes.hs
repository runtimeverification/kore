module Test.Kore.Builtin.InternalBytes (
    test_update,
    test_get,
    test_substr,
    test_replaceAt,
    test_padRight,
    test_padLeft,
    test_reverse,
    test_length,
    test_concat,
    test_reverse_length,
    test_update_get,
    test_bytes2string_string2bytes,
    test_decodeBytes_encodeBytes,
    test_decodeBytes,
    test_encodeBytes,
    test_int2bytes,
    test_bytes2int,
    test_InternalBytes,
    test_unparse,
) where

import Control.Exception (ErrorCall (..), try)
import Data.ByteString (
    ByteString,
 )
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as BS
import Data.Char (
    ord,
 )
import Data.Text (
    Text,
 )
import Data.Text qualified as T
import Hedgehog hiding (
    Concrete,
    test,
 )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kore.Builtin.Encoding qualified as E
import Kore.Builtin.InternalBytes (decodeBytesKey, encodeBytesKey)
import Kore.Builtin.InternalBytes qualified as InternalBytes
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.OrPattern (OrPattern)
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.TermLike
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
    mkConfigVariable,
 )
import Kore.Unparser
import Prelude.Kore
import Pretty qualified
import Test.Kore.Builtin.Builtin
import Test.Kore.Builtin.Definition
import Test.Kore.Builtin.Int qualified as Test.Int
import Test.Kore.Builtin.String qualified as Test.String
import Test.SMT
import Test.Tasty
import Test.Tasty.HUnit.Ext

genString :: Gen Text
genString = Gen.text (Range.linear 0 256) Gen.latin1

genString' :: Int -> Gen Text
genString' i = Gen.text (Range.linear 0 i) Gen.latin1

test_update :: [TestTree]
test_update =
    [ testPropertyWithSolver "∀ b v. update b 0 v = v" $ do
        val <- forAll Gen.unicode
        let val' = toInteger $ ord val
            bytes = BS.pack [val]
            expect = asOrPattern bytes
        actual <-
            evaluateTermT $
                mkApplySymbol
                    updateBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal 0
                    , Test.Int.asInternal val'
                    ]
        (===) expect actual
    , testPropertyWithSolver "∀ b i v (i < 0). update b i v = ⊥" $ do
        str <- forAll genString
        val <- forAll Gen.unicode
        idx <- forAll $ Gen.int (Range.linear (-256) (-1))
        let bytes = E.encode8Bit str
            val' = toInteger $ ord val
            expect = OrPattern.bottom
        actual <-
            evaluateTermT $
                mkApplySymbol
                    updateBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal (toInteger idx)
                    , Test.Int.asInternal val'
                    ]
        (===) expect actual
    , testPropertyWithSolver "∀ b i v (i > length bs). update b i v = ⊥" $ do
        str <- forAll genString
        val <- forAll Gen.unicode
        let bytes = E.encode8Bit str
            val' = toInteger $ ord val
            expect = OrPattern.bottom
        actual <-
            evaluateTermT $
                mkApplySymbol
                    updateBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal (toInteger $ BS.length bytes)
                    , Test.Int.asInternal val'
                    ]
        (===) expect actual
    , testBytes
        "update 'abcd' 0 'x' = 'xbcd'"
        updateBytesSymbol
        [ asInternal "abcd"
        , Test.Int.asInternal 0
        , Test.Int.asInternal (toInteger $ ord 'x')
        ]
        (asOrPattern "xbcd")
    , testBytes
        "update 'abcd' 3 'x' = 'abcx"
        updateBytesSymbol
        [ asInternal "abcd"
        , Test.Int.asInternal 3
        , Test.Int.asInternal (toInteger $ ord 'x')
        ]
        (asOrPattern "abcx")
    ]

test_get :: [TestTree]
test_get =
    [ testPropertyWithSolver "∀ b i (i < 0). get b i = ⊥" $ do
        str <- forAll genString
        idx <- forAll $ Gen.int (Range.linear (-256) (-1))
        let bytes = E.encode8Bit str
            expect = OrPattern.bottom
        actual <-
            evaluateTermT $
                mkApplySymbol
                    getBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal (toInteger idx)
                    ]
        (===) expect actual
    , testPropertyWithSolver "∀ b i (i > len b). get b i = ⊥" $ do
        str <- forAll genString
        let bytes = E.encode8Bit str
            expect = OrPattern.bottom
        actual <-
            evaluateTermT $
                mkApplySymbol
                    getBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal (toInteger $ BS.length bytes)
                    ]
        (===) expect actual
    , testPropertyWithSolver "∀ i. get empty i = ⊥" $ do
        idx <- forAll $ Gen.int (Range.linear 0 256)
        let expect = OrPattern.bottom
        actual <-
            evaluateTermT $
                mkApplySymbol
                    getBytesSymbol
                    [ asInternal ""
                    , Test.Int.asInternal (toInteger idx)
                    ]
        (===) expect actual
    , testBytes
        "get 'abcd' 0 = 'a'"
        getBytesSymbol
        [ asInternal "abcd"
        , Test.Int.asInternal 0
        ]
        (Test.Int.asOrPattern $ toInteger $ ord 'a')
    , testBytes
        "get 'abcd' 3 = 'd'"
        getBytesSymbol
        [ asInternal "abcd"
        , Test.Int.asInternal 3
        ]
        (Test.Int.asOrPattern $ toInteger $ ord 'd')
    ]

test_substr :: [TestTree]
test_substr =
    [ testPropertyWithSolver "end < start -> substr bytes start end = ⊥" $ do
        str <- forAll genString
        delta <- forAll $ Gen.int (Range.linear 1 10)
        end <- forAll $ Gen.int (Range.linear 0 (T.length str - delta))
        let bytes = E.encode8Bit str
            expect = OrPattern.bottom
        actual <-
            evaluateTermT $
                mkApplySymbol
                    substrBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal (toInteger $ end + delta)
                    , Test.Int.asInternal (toInteger end)
                    ]
        (===) expect actual
    , testPropertyWithSolver "∀ b s e (e > length b). substr b s e = ⊥" $ do
        str <- forAll $ genString' 20
        end <- forAll $ Gen.int (Range.linear 21 30)
        let bytes = E.encode8Bit str
            expect = OrPattern.bottom
        actual <-
            evaluateTermT $
                mkApplySymbol
                    substrBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal 0
                    , Test.Int.asInternal (toInteger end)
                    ]
        (===) expect actual
    , testPropertyWithSolver "∀ b s e (b < 0). substr b s e = ⊥" $ do
        str <- forAll genString
        begin <- forAll $ Gen.int (Range.linear (-256) (-1))
        end <- forAll $ Gen.int (Range.linear 0 256)
        let bytes = E.encode8Bit str
            expect = OrPattern.bottom
        actual <-
            evaluateTermT $
                mkApplySymbol
                    substrBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal (toInteger begin)
                    , Test.Int.asInternal (toInteger end)
                    ]
        (===) expect actual
    , testSubstrBytes
        "substr 'abcd' 0 0 = ''"
        [ asInternal "abcd"
        , Test.Int.asInternal 0
        , Test.Int.asInternal 0
        ]
        (asOrPattern "")
    , testSubstrBytes
        "substr 'abcd' 0 1 = 'a'"
        [ asInternal "abcd"
        , Test.Int.asInternal 0
        , Test.Int.asInternal 1
        ]
        (asOrPattern "a")
    , testSubstrBytes
        "substr 'abcd' 1 3 = 'bc'"
        [ asInternal "abcd"
        , Test.Int.asInternal 1
        , Test.Int.asInternal 3
        ]
        (asOrPattern "bc")
    , testSubstrBytes
        "substr 'abcd' 0 4 = 'abcd'"
        [ asInternal "abcd"
        , Test.Int.asInternal 0
        , Test.Int.asInternal 4
        ]
        (asOrPattern "abcd")
    ]
  where
    testSubstrBytes ::
        HasCallStack =>
        TestName ->
        -- arguments of @substrBytes@ symbol
        [TermLike RewritingVariableName] ->
        -- expected result
        OrPattern RewritingVariableName ->
        TestTree
    testSubstrBytes testName = testBytes testName substrBytesSymbol

test_replaceAt :: [TestTree]
test_replaceAt =
    [ testPropertyWithSolver "∀ b i. replaceAt b i '' = n" $ do
        str <- forAll genString
        idx <- forAll $ Gen.int (Range.linear 0 256)
        let bytes = E.encode8Bit str
            expect = asOrPattern bytes
        actual <-
            evaluateTermT $
                mkApplySymbol
                    replaceAtBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal (toInteger idx)
                    , asInternal ""
                    ]
        (===) expect actual
    , testPropertyWithSolver "∀ b i (b /= ''). replaceAt '' i b = ⊥" $ do
        str <- forAll $ Gen.text (Range.linear 1 256) Gen.alphaNum
        idx <- forAll $ Gen.int (Range.linear 0 256)
        let expect = OrPattern.bottom
        actual <-
            evaluateTermT $
                mkApplySymbol
                    replaceAtBytesSymbol
                    [ asInternal ""
                    , Test.Int.asInternal (toInteger idx)
                    , asInternal $ E.encode8Bit str
                    ]
        (===) expect actual
    , testPropertyWithSolver
        "∀ b i b' (b' /= '', i >= length b). replaceAt b i b' = ⊥"
        $ do
            str <- forAll $ genString' 20
            idx <- forAll $ Gen.int (Range.linear 21 256)
            new <- forAll $ Gen.text (Range.linear 1 256) Gen.alphaNum
            let bytes = E.encode8Bit str
                bytes' = E.encode8Bit new
                expect = OrPattern.bottom
            actual <-
                evaluateTermT $
                    mkApplySymbol
                        replaceAtBytesSymbol
                        [ asInternal bytes
                        , Test.Int.asInternal (toInteger idx)
                        , asInternal bytes'
                        ]
            (===) expect actual
    , testBytes
        "replaceAt 'abcd' 0 '12' = '12cd'"
        replaceAtBytesSymbol
        [ asInternal "abcd"
        , Test.Int.asInternal 0
        , asInternal "12"
        ]
        (asOrPattern "12cd")
    , testBytes
        "replaceAt 'abcd' 1 '12' = 'a12d'"
        replaceAtBytesSymbol
        [ asInternal "abcd"
        , Test.Int.asInternal 1
        , asInternal "12"
        ]
        (asOrPattern "a12d")
    , testBytes
        "replaceAt 'abcd' 3 '12' = 'abc12'"
        replaceAtBytesSymbol
        [ asInternal "abcd"
        , Test.Int.asInternal 3
        , asInternal "12"
        ]
        (asOrPattern "abc12")
    ]

test_padRight :: [TestTree]
test_padRight =
    [ testPropertyWithSolver "∀ b i v (i >= length b). padRight b i v = b" $ do
        str <- forAll genString
        val <- forAll Gen.alphaNum
        let bytes = E.encode8Bit str
            expect = asOrPattern bytes
        actual <-
            evaluateTermT $
                mkApplySymbol
                    padRightBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal (toInteger $ BS.length bytes)
                    , Test.Int.asInternal (toInteger $ ord val)
                    ]
        (===) expect actual
    , testBytes
        "padRight 'abcd' 5 'e' = 'abcde"
        padRightBytesSymbol
        [ asInternal "abcd"
        , Test.Int.asInternal 5
        , Test.Int.asInternal (toInteger $ ord 'e')
        ]
        (asOrPattern "abcde")
    ]

test_padLeft :: [TestTree]
test_padLeft =
    [ testPropertyWithSolver "∀ b i v (i >= length b). padLeft b i v = b" $ do
        str <- forAll genString
        val <- forAll Gen.alphaNum
        let bytes = E.encode8Bit str
            expect = asOrPattern bytes
        actual <-
            evaluateTermT $
                mkApplySymbol
                    padLeftBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal (toInteger $ BS.length bytes)
                    , Test.Int.asInternal (toInteger $ ord val)
                    ]
        (===) expect actual
    , testBytes
        "padLeft 'abcd' 5 'e' = 'eabcd"
        padLeftBytesSymbol
        [ asInternal "abcd"
        , Test.Int.asInternal 5
        , Test.Int.asInternal (toInteger $ ord 'e')
        ]
        (asOrPattern "eabcd")
    ]

test_reverse :: [TestTree]
test_reverse =
    [ testPropertyWithSolver "∀ b. reverse (reverse b) = b" $ do
        str <- forAll genString
        let bytes = E.encode8Bit str
            expect = asOrPattern bytes
        actual <-
            evaluateTermT $
                mkApplySymbol
                    reverseBytesSymbol
                    [ mkApplySymbol
                        reverseBytesSymbol
                        [ asInternal bytes
                        ]
                    ]
        (===) expect actual
    , testBytes
        "reverse 'abcd' = 'dcba'"
        reverseBytesSymbol
        [ asInternal "abcd"
        ]
        (asOrPattern "dcba")
    ]

test_length :: [TestTree]
test_length =
    [ testBytes
        "length 'abcd' = 4"
        lengthBytesSymbol
        [ asInternal "abcd"
        ]
        (Test.Int.asOrPattern 4)
    , testBytes
        "length '' = 0"
        lengthBytesSymbol
        [ asInternal ""
        ]
        (Test.Int.asOrPattern 0)
    ]

test_concat :: [TestTree]
test_concat =
    [ testPropertyWithSolver "∀ b. concat b '' = b" $ do
        str <- forAll genString
        let bytes = E.encode8Bit str
            expect = asOrPattern bytes
        actual <-
            evaluateTermT $
                mkApplySymbol
                    concatBytesSymbol
                    [ asInternal bytes
                    , asInternal ""
                    ]
        (===) expect actual
    , testPropertyWithSolver "∀ b. concat '' b = b" $ do
        str <- forAll genString
        let bytes = E.encode8Bit str
            expect = asOrPattern bytes
        actual <-
            evaluateTermT $
                mkApplySymbol
                    concatBytesSymbol
                    [ asInternal ""
                    , asInternal bytes
                    ]
        (===) expect actual
    , testBytes
        "concat 'abcd' 'efgh' = 'abcdefgh'"
        concatBytesSymbol
        [ asInternal "abcd"
        , asInternal "efgh"
        ]
        (asOrPattern "abcdefgh")
    ]

test_reverse_length :: TestTree
test_reverse_length =
    testPropertyWithSolver "∀ b. length (reverse b) = length b" $ do
        str <- forAll genString
        let bytes = E.encode8Bit str
            expect = Test.Int.asOrPattern $ toInteger $ BS.length bytes
        actual <-
            evaluateTermT $
                mkApplySymbol
                    lengthBytesSymbol
                    [ mkApplySymbol
                        reverseBytesSymbol
                        [ asInternal bytes
                        ]
                    ]
        (===) expect actual

test_update_get :: TestTree
test_update_get =
    testPropertyWithSolver "∀ b i. update b i (get b i) = b" $ do
        str <- forAll $ Gen.text (Range.linear 1 256) Gen.alphaNum
        idx <- forAll $ Gen.int (Range.linear 0 (T.length str - 1))
        let bytes = E.encode8Bit str
            expect = asOrPattern bytes
        actual <-
            evaluateTermT $
                mkApplySymbol
                    updateBytesSymbol
                    [ asInternal bytes
                    , Test.Int.asInternal $ toInteger idx
                    , mkApplySymbol
                        getBytesSymbol
                        [ asInternal bytes
                        , Test.Int.asInternal $ toInteger idx
                        ]
                    ]
        (===) expect actual

test_bytes2string_string2bytes :: TestTree
test_bytes2string_string2bytes =
    testPropertyWithSolver "∀ s. bytes2string (string2bytes s) = s" $ do
        str <- forAll genString
        let expect = Test.String.asOrPattern str
        actual <-
            evaluateTermT $
                mkApplySymbol
                    bytes2stringBytesSymbol
                    [ mkApplySymbol
                        string2bytesBytesSymbol
                        [ Test.String.asInternal str
                        ]
                    ]
        (===) expect actual

test_decodeBytes_encodeBytes :: [TestTree]
test_decodeBytes_encodeBytes = map testProp encodings
  where
    testProp encoding =
        testPropertyWithSolver "∀ s. decodeBytes (encodeBytes s) = s" $ do
            str <- forAll genString
            let expect = Test.String.asOrPattern str
            actual <-
                evaluateTermT $
                    mkApplySymbol
                        decodeBytesBytesSymbol
                        [ Test.String.asInternal encoding
                        , mkApplySymbol
                            encodeBytesBytesSymbol
                            [ Test.String.asInternal encoding
                            , Test.String.asInternal str
                            ]
                        ]
            (===) expect actual
    encodings =
        [ "UTF-8"
        , "UTF-16LE"
        , "UTF-16BE"
        , "UTF-32LE"
        , "UTF-32BE"
        ]

test_decodeBytes :: TestTree
test_decodeBytes =
    testBadEvaluation
        "test bad decoding"
        decodeBytesKey
        ( mkApplySymbol
            decodeBytesBytesSymbol
            [ Test.String.asInternal "bad"
            , asInternal ""
            ]
        )

test_encodeBytes :: TestTree
test_encodeBytes =
    testBadEvaluation
        "test bad encoding"
        encodeBytesKey
        ( mkApplySymbol
            encodeBytesBytesSymbol
            [ Test.String.asInternal "bad"
            , Test.String.asInternal ""
            ]
        )

testBadEvaluation :: TestName -> Pretty.Doc a -> TermLike RewritingVariableName -> TestTree
testBadEvaluation testName hook term =
    testCase testName $ do
        try (runNoSMT $ evaluateTerm term) >>= \case
            Right patt ->
                assertFailure $
                    unlines
                        [ "Expected evaluation to fail, but it succeeded:"
                        , show patt
                        ]
            Left (ErrorCall errMsg) -> do
                let expectedErrMsg =
                        show $
                            Pretty.vsep
                                [ "Expecting hook " <> Pretty.squotes hook <> " to reduce concrete pattern:"
                                , Pretty.indent 4 (unparse term)
                                ]
                assertEqual "" expectedErrMsg errMsg

int2bytesData ::
    -- | (integer, big endian?, bytes)
    [(Integer, Bool, ByteString)]
int2bytesData =
    [ (0, True, "\x00\x00\x00\x00")
    , (128, True, "\x80")
    , (-128, True, "\x80")
    , (2, True, "\x02")
    , (-2, True, "\xfe")
    , (16, True, "\x10")
    , (-16, True, "\xf0")
    , (128, True, "\x00\x80")
    , (-128, True, "\xff\x80")
    , (128, False, "\x80\x00")
    , (-128, False, "\x80\xff")
    , (0, True, "")
    ]

test_int2bytes :: [TestTree]
test_int2bytes =
    map test int2bytesData
  where
    test ::
        HasCallStack =>
        (Integer, Bool, ByteString) ->
        TestTree
    test (integer, bigEndian, bytes) =
        testCase name $ do
            let input =
                    int2bytes
                        (Test.Int.asInternal len)
                        (Test.Int.asInternal integer)
                        end
                expect = [asPattern bytes]
            actual <- simplify input
            assertEqual "" expect actual
      where
        name =
            let args =
                    ( len
                    , integer
                    , if bigEndian then "BE" else "LE" :: String
                    )
             in show $ "int2bytes" <> Pretty.pretty args
        len = fromIntegral $ ByteString.length bytes
        end
            | bigEndian = bigEndianBytes
            | otherwise = littleEndianBytes

test_bytes2int :: [TestTree]
test_bytes2int =
    map test int2bytesData
  where
    test ::
        HasCallStack =>
        (Integer, Bool, ByteString) ->
        TestTree
    test (integer, bigEndian, bytes) =
        testGroup name (mkCase <$> [True, False])
      where
        mkCase signed =
            testCase (if signed then "signed" else "unsigned") $ do
                let sign
                        | signed = signedBytes
                        | otherwise = unsignedBytes
                    underflow = (-2) * integer >= modulus
                    int
                        | not signed, integer < 0 = integer + modulus
                        | signed, underflow = integer + modulus
                        | otherwise = integer
                    modulus = 0x100 ^ ByteString.length bytes
                    input = bytes2int (asInternal bytes) end sign
                    expect =
                        [ Test.Int.asPattern int
                            & Pattern.mapVariables (pure mkConfigVariable)
                        ]
                actual <- simplify input
                assertEqual "" expect actual
        name =
            let args =
                    ( ByteString.unpack bytes
                    , if bigEndian then "BE" else "LE" :: String
                    , "_" :: String
                    )
             in show $ "bytes2int" <> Pretty.pretty args
        end
            | bigEndian = bigEndianBytes
            | otherwise = littleEndianBytes

test_InternalBytes :: [TestTree]
test_InternalBytes =
    [ testCase "\\dv{Bytes{}}(\"00\")" $ do
        let unverified =
                mkDomainValue $
                    DomainValue bytesSort $
                        mkStringLiteral "00"
            expect = Right $ asInternal "00"
            actual = verifyPattern (Just bytesSort) unverified
        assertEqual "" expect actual
    , testCase "\\dv{Bytes{}}(\"\\x00\")" $ do
        let unverified =
                mkDomainValue $
                    DomainValue bytesSort $
                        mkStringLiteral "\x00"
            expect = Right $ asInternal "\x00"
            actual = verifyPattern (Just bytesSort) unverified
        assertEqual "" expect actual
    ]

test_unparse :: [TestTree]
test_unparse =
    [ testCase "unparse using 8-bit encoding" $ do
        let input = asInternal "\x00" :: TermLike RewritingVariableName
            actual = (show . unparse) input
            expect = "\\dv{Bytes{}}(\"\\x00\")"
        assertEqual "" expect actual
    ]

-- * Helpers

asInternal :: InternalVariable variable => ByteString -> TermLike variable
asInternal = InternalBytes.asInternal bytesSort

asPattern :: InternalVariable variable => ByteString -> Pattern variable
asPattern = InternalBytes.asPattern bytesSort

asOrPattern :: InternalVariable variable => ByteString -> OrPattern variable
asOrPattern = MultiOr.singleton . asPattern

testBytes ::
    HasCallStack =>
    String ->
    Symbol ->
    [TermLike RewritingVariableName] ->
    OrPattern RewritingVariableName ->
    TestTree
testBytes name = testSymbolWithoutSolver evaluateTerm name
