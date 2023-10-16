{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Builtin.InternalBytes (
    sort,
    assertSort,
    verifiers,
    builtinFunctions,
    expectBuiltinBytes,
    asInternal,
    internalize,
    asPattern,
    UnifyBytes (..),
    matchBytes,
    unifyBytes,

    -- * Keys
    bytes2StringKey,
    string2BytesKey,
    decodeBytesKey,
    encodeBytesKey,
    updateKey,
    getKey,
    substrKey,
    replaceAtKey,
    padRightKey,
    padLeftKey,
    reverseKey,
    lengthKey,
    concatKey,
) where

import Control.Error (
    MaybeT,
 )
import Control.Exception (
    evaluate,
    try,
 )
import Data.ByteString (
    ByteString,
 )
import Data.ByteString qualified as ByteString
import Data.ByteString.Short qualified as ShortByteString
import Data.Functor.Const
import Data.Functor.Foldable qualified as Recursive
import Data.HashMap.Strict qualified as HashMap
import Data.String (
    IsString,
 )
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Word (
    Word8,
 )
import Kore.Builtin.Builtin qualified as Builtin
import Kore.Builtin.Encoding qualified as Encoding
import Kore.Builtin.Endianness.Endianness
import Kore.Builtin.Int qualified as Int
import Kore.Builtin.InternalBytes.InternalBytes
import Kore.Builtin.Signedness.Signedness
import Kore.Builtin.String qualified as String
import Kore.Error qualified
import Kore.Internal.ApplicationSorts (
    applicationSortsResult,
 )
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.TermLike
import Kore.Log.WarnNotImplemented
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Simplify (
    BuiltinAndAxiomSimplifier,
 )
import Kore.Unification.Unify (
    MonadUnify,
 )
import Kore.Verified qualified as Verified
import Log (MonadLog)
import Prelude.Kore
import System.IO.Unsafe (
    unsafeDupablePerformIO,
 )

{- | Verify that the sort is hooked to the @Bytes@ sort.
 | See also: 'sort', 'Builtin.verifySort'.
-}
assertSort :: Builtin.SortVerifier
assertSort = Builtin.verifySort sort

verifiers :: Builtin.Verifiers
verifiers =
    Builtin.Verifiers
        { sortDeclVerifiers
        , symbolVerifiers
        , patternVerifierHook
        }

{- | Verify that hooked sort declarations are well-formed.
 | See also: 'Builtin.verifySortDecl'.
-}
sortDeclVerifiers :: Builtin.SortDeclVerifiers
sortDeclVerifiers = HashMap.fromList [(sort, Builtin.verifySortDecl)]

{- | Verify that hooked symbol declarations are well-formed.
 | See also: 'Builtin.verifySymbol'.
-}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
        [
            ( bytes2StringKey
            , Builtin.verifySymbol string [bytes]
            )
        ,
            ( string2BytesKey
            , Builtin.verifySymbol bytes [string]
            )
        ,
            ( decodeBytesKey
            , Builtin.verifySymbol string [string, bytes]
            )
        ,
            ( encodeBytesKey
            , Builtin.verifySymbol bytes [string, string]
            )
        ,
            ( updateKey
            , Builtin.verifySymbol bytes [bytes, int, int]
            )
        ,
            ( getKey
            , Builtin.verifySymbol int [bytes, int]
            )
        ,
            ( substrKey
            , Builtin.verifySymbol bytes [bytes, int, int]
            )
        ,
            ( replaceAtKey
            , Builtin.verifySymbol bytes [bytes, int, bytes]
            )
        ,
            ( padRightKey
            , Builtin.verifySymbol bytes [bytes, int, int]
            )
        ,
            ( padLeftKey
            , Builtin.verifySymbol bytes [bytes, int, int]
            )
        ,
            ( reverseKey
            , Builtin.verifySymbol bytes [bytes]
            )
        ,
            ( lengthKey
            , Builtin.verifySymbol int [bytes]
            )
        ,
            ( concatKey
            , Builtin.verifySymbol bytes [bytes, bytes]
            )
        ,
            ( int2bytesKey
            , Builtin.verifySymbol bytes [int, int, anySort]
            )
        ,
            ( bytes2intKey
            , Builtin.verifySymbol int [bytes, anySort, anySort]
            )
        ]
  where
    bytes = assertSort
    int = Int.assertSort
    string = String.assertSort
    anySort = Builtin.acceptAnySort

-- | Verify that domain value patterns are well-formed.
patternVerifierHook :: Builtin.PatternVerifierHook
patternVerifierHook =
    Builtin.domainValuePatternVerifierHook sort patternVerifierWorker
        <> (Builtin.applicationPatternVerifierHooks . HashMap.fromList)
            [(Builtin.HookedSymbolKey dotBytes, dotBytesVerifier)]
  where
    patternVerifierWorker external =
        case externalChild of
            StringLiteral_ literal -> do
                bs <- Builtin.parseString Encoding.parse8Bit literal
                let internalBytesValue = ShortByteString.toShort bs
                (return . InternalBytesF . Const)
                    InternalBytes{internalBytesSort, internalBytesValue}
            _ -> Kore.Error.koreFail "Expected literal string"
      where
        DomainValue{domainValueSort = internalBytesSort} = external
        DomainValue{domainValueChild = externalChild} = external

dotBytes :: IsString str => str
dotBytes = "BYTES.empty"

dotBytesVerifier ::
    Builtin.ApplicationVerifier Verified.Pattern
dotBytesVerifier =
    Builtin.ApplicationVerifier worker
  where
    worker application = do
        unless (null arguments) (Kore.Error.koreFail "expected zero arguments")
        (return . InternalBytesF . Const)
            InternalBytes
                { internalBytesSort
                , internalBytesValue = ShortByteString.toShort $ Encoding.encode8Bit ""
                }
      where
        arguments = applicationChildren application
        symbol = applicationSymbolOrAlias application
        internalBytesSort = applicationSortsResult . symbolSorts $ symbol

expectBuiltinBytes :: Monad m => TermLike variable -> MaybeT m ByteString
expectBuiltinBytes (InternalBytes_ _ byteString) = return $ ShortByteString.fromShort byteString
expectBuiltinBytes _ = empty

evalBytes2String :: BuiltinAndAxiomSimplifier
evalBytes2String =
    Builtin.functionEvaluator evalBytes2String0
  where
    evalBytes2String0 :: Builtin.Function
    evalBytes2String0 _ resultSort [_bytes] = do
        _bytes <- expectBuiltinBytes _bytes
        Encoding.decode8Bit _bytes
            & String.asPattern resultSort
            & return
    evalBytes2String0 _ _ _ = Builtin.wrongArity bytes2StringKey

evalString2Bytes :: BuiltinAndAxiomSimplifier
evalString2Bytes =
    Builtin.functionEvaluator evalString2Bytes0
  where
    evalString2Bytes0 :: Builtin.Function
    evalString2Bytes0 _ resultSort [_string] = do
        _string <- String.expectBuiltinString string2BytesKey _string
        Encoding.encode8Bit _string
            & asPattern resultSort
            & return
    evalString2Bytes0 _ _ _ = Builtin.wrongArity string2BytesKey

evalDecodeBytes :: BuiltinAndAxiomSimplifier
evalDecodeBytes = Builtin.applicationEvaluator evalDecodeBytes0
  where
    evalDecodeBytes0 _ app
        | [_strTerm, _bytesTerm] <- applicationChildren app = do
            let Application{applicationSymbolOrAlias = symbol} = app
                resultSort = symbolSorts symbol & applicationSortsResult
            _str <- String.expectBuiltinString decodeBytesKey _strTerm
            _bytes <- expectBuiltinBytes _bytesTerm
            decodeUtf app resultSort (Text.unpack _str) _bytes
    evalDecodeBytes0 _ _ = Builtin.wrongArity decodeBytesKey

{- | Decode a ByteString using UTF-8, UTF-16LE, UTF-16BE,
 UTF-32LE or UTF-32BE. If the decoding format is invalid,
 warn not implemented. If the ByteString contains any invalid
 UTF-* data, bottom is returned, otherwise the decoded text.
 See <https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text-Encoding.html#v:decodeUtf8-39- decodeUtf8'>
 implementation for more information about 'tryDecode'.
-}
decodeUtf ::
    MonadLog unify =>
    InternalVariable variable =>
    Application Symbol (TermLike variable) ->
    Sort ->
    String ->
    ByteString ->
    MaybeT unify (Pattern.Pattern variable)
decodeUtf app resultSort = \case
    "UTF-8" -> return . handleError . tryDecode . Text.decodeUtf8
    "UTF-16LE" -> return . handleError . tryDecode . Text.decodeUtf16LE
    "UTF-16BE" -> return . handleError . tryDecode . Text.decodeUtf16BE
    "UTF-32LE" -> return . handleError . tryDecode . Text.decodeUtf32LE
    "UTF-32BE" -> return . handleError . tryDecode . Text.decodeUtf32BE
    _ -> const (warnNotImplemented app >> empty)
  where
    tryDecode :: Text -> Either Text.UnicodeException Text
    tryDecode = unsafeDupablePerformIO . try . evaluate
    handleError = \case
        Right str -> String.asPattern resultSort str
        Left _ -> Pattern.bottomOf resultSort

evalEncodeBytes :: BuiltinAndAxiomSimplifier
evalEncodeBytes = Builtin.applicationEvaluator evalEncodeBytes0
  where
    evalEncodeBytes0 _ app
        | [_encodingTerm, _contentsTerm] <- applicationChildren app = do
            let Application{applicationSymbolOrAlias = symbol} = app
                resultSort = symbolSorts symbol & applicationSortsResult
                returnResult = return . asPattern resultSort
            _encoding <- String.expectBuiltinString encodeBytesKey _encodingTerm
            _contents <- String.expectBuiltinString encodeBytesKey _contentsTerm
            case Text.unpack _encoding of
                "UTF-8" -> returnResult $ Text.encodeUtf8 _contents
                "UTF-16LE" -> returnResult $ Text.encodeUtf16LE _contents
                "UTF-16BE" -> returnResult $ Text.encodeUtf16BE _contents
                "UTF-32LE" -> returnResult $ Text.encodeUtf32LE _contents
                "UTF-32BE" -> returnResult $ Text.encodeUtf32BE _contents
                _ -> warnNotImplemented app >> empty
    evalEncodeBytes0 _ _ = Builtin.wrongArity encodeBytesKey

evalUpdate :: BuiltinAndAxiomSimplifier
evalUpdate =
    Builtin.functionEvaluator evalUpdate0
  where
    evalUpdate0 :: Builtin.Function
    evalUpdate0 _ resultSort [_bytes, _index, _value] = do
        _bytes <- expectBuiltinBytes _bytes
        _index <- fromInteger <$> Int.expectBuiltinInt updateKey _index
        _value <- fromInteger <$> Int.expectBuiltinInt updateKey _value
        let result
                | _index >= 0
                , _index < ByteString.length _bytes =
                    ByteString.take _index _bytes
                        <> ByteString.singleton _value
                        <> ByteString.drop (_index + 1) _bytes
                        & asPattern resultSort
                | otherwise = Pattern.bottomOf resultSort
        return result
    evalUpdate0 _ _ _ = Builtin.wrongArity updateKey

evalGet :: BuiltinAndAxiomSimplifier
evalGet =
    Builtin.functionEvaluator evalGet0
  where
    evalGet0 :: Builtin.Function
    evalGet0 _ resultSort [_bytes, _index] = do
        _bytes <- expectBuiltinBytes _bytes
        _index <- fromInteger <$> Int.expectBuiltinInt getKey _index
        let result
                | _index >= 0
                , _index < ByteString.length _bytes =
                    ByteString.index _bytes _index
                        & toInteger
                        & Int.asPattern resultSort
                | otherwise =
                    Pattern.bottomOf resultSort
        return result
    evalGet0 _ _ _ = Builtin.wrongArity getKey

evalSubstr :: BuiltinAndAxiomSimplifier
evalSubstr =
    Builtin.functionEvaluator evalSubstr0
  where
    evalSubstr0 :: Builtin.Function
    evalSubstr0 _ resultSort [_bytes, _start, _end] = do
        _bytes <- expectBuiltinBytes _bytes
        _start <- fromInteger <$> Int.expectBuiltinInt substrKey _start
        _end <- fromInteger <$> Int.expectBuiltinInt substrKey _end
        let result
                | _start >= 0
                , _end >= _start
                , _end <= ByteString.length _bytes =
                    _bytes
                        & ByteString.drop _start
                        & ByteString.take (_end - _start)
                        & asPattern resultSort
                | otherwise = Pattern.bottomOf resultSort
        return result
    evalSubstr0 _ _ _ = Builtin.wrongArity substrKey

evalReplaceAt :: BuiltinAndAxiomSimplifier
evalReplaceAt =
    Builtin.functionEvaluator evalReplaceAt0
  where
    evalReplaceAt0 :: Builtin.Function
    evalReplaceAt0 _ resultSort [_bytes, _index, _new] = do
        _bytes <- expectBuiltinBytes _bytes
        _index <- fromInteger <$> Int.expectBuiltinInt replaceAtKey _index
        _new <- expectBuiltinBytes _new
        go _bytes _index _new
            & maybe (Pattern.bottomOf resultSort) (asPattern resultSort)
            & return
    evalReplaceAt0 _ _ _ = Builtin.wrongArity replaceAtKey

    go bytes index replacement
        | delta == 0 = Just bytes
        | index >= ByteString.length bytes = Nothing
        | index < 0 = Nothing
        | ByteString.length bytes == 0 = Nothing
        | otherwise =
            ByteString.take index bytes
                <> replacement
                <> ByteString.drop (index + delta) bytes
                & Just
      where
        delta = ByteString.length replacement

evalPadRight :: BuiltinAndAxiomSimplifier
evalPadRight =
    Builtin.functionEvaluator evalPadRight0
  where
    evalPadRight0 :: Builtin.Function
    evalPadRight0 _ resultSort [_bytes, _length, _value] = do
        _bytes <- expectBuiltinBytes _bytes
        _length <- fromInteger <$> Int.expectBuiltinInt padRightKey _length
        _value <- fromInteger <$> Int.expectBuiltinInt padRightKey _value
        (return . asPattern resultSort) (go _bytes _length _value)
    evalPadRight0 _ _ _ = Builtin.wrongArity padRightKey

    go bytes len2 val =
        bytes <> ByteString.replicate (max 0 delta) val
      where
        len1 = ByteString.length bytes
        delta = len2 - len1

evalPadLeft :: BuiltinAndAxiomSimplifier
evalPadLeft =
    Builtin.functionEvaluator evalPadLeft0
  where
    evalPadLeft0 :: Builtin.Function
    evalPadLeft0 _ resultSort [_bytes, _length, _value] = do
        _bytes <- expectBuiltinBytes _bytes
        _length <- fromInteger <$> Int.expectBuiltinInt padLeftKey _length
        _value <- fromInteger <$> Int.expectBuiltinInt padLeftKey _value
        return . asPattern resultSort $ go _bytes _length _value
    evalPadLeft0 _ _ _ = Builtin.wrongArity padLeftKey

    go bytes len2 val =
        ByteString.replicate (max 0 delta) val <> bytes
      where
        len1 = ByteString.length bytes
        delta = len2 - len1

evalReverse :: BuiltinAndAxiomSimplifier
evalReverse =
    Builtin.functionEvaluator evalReverse0
  where
    evalReverse0 :: Builtin.Function
    evalReverse0 _ resultSort [_bytes] = do
        _bytes <- expectBuiltinBytes _bytes
        ByteString.reverse _bytes
            & asPattern resultSort
            & return
    evalReverse0 _ _ _ = Builtin.wrongArity reverseKey

evalLength :: BuiltinAndAxiomSimplifier
evalLength =
    Builtin.functionEvaluator evalLength0
  where
    evalLength0 :: Builtin.Function
    evalLength0 _ resultSort [_bytes] = do
        _bytes <- expectBuiltinBytes _bytes
        toInteger (ByteString.length _bytes)
            & Int.asPattern resultSort
            & return
    evalLength0 _ _ _ = Builtin.wrongArity lengthKey

evalConcat :: BuiltinAndAxiomSimplifier
evalConcat =
    Builtin.functionEvaluator evalConcat0
  where
    evalConcat0 :: Builtin.Function
    evalConcat0 _ resultSort [_lhs, _rhs] = do
        _lhs <- expectBuiltinBytes _lhs
        _rhs <- expectBuiltinBytes _rhs
        _lhs <> _rhs
            & asPattern resultSort
            & return
    evalConcat0 _ _ _ = Builtin.wrongArity concatKey

evalInt2bytes :: BuiltinAndAxiomSimplifier
evalInt2bytes =
    Builtin.functionEvaluator evalInt2bytes0
  where
    evalInt2bytes0 :: Builtin.Function
    evalInt2bytes0 _ resultSort [len, int, _end] = do
        _end <- matchEndianness _end
        len' <- Int.expectBuiltinInt int2bytesKey len
        int' <- Int.expectBuiltinInt int2bytesKey int
        int2bytes (fromInteger len') int' _end
            & asPattern resultSort
            & return
    evalInt2bytes0 _ _ _ = Builtin.wrongArity int2bytesKey

matchEndianness :: Alternative f => TermLike variable -> f Endianness
matchEndianness (Endianness_ endianness) = pure endianness
matchEndianness _ = empty

matchSignedness :: Alternative f => TermLike variable -> f Signedness
matchSignedness (Signedness_ signedness) = pure signedness
matchSignedness _ = empty

int2bytes :: Int -> Integer -> Endianness -> ByteString
int2bytes len int end =
    case end of
        LittleEndian _ -> littleEndian
        BigEndian _ -> ByteString.reverse littleEndian
  where
    (littleEndian, _) = ByteString.unfoldrN len go int
    go int'
        | int' == 0 = Just (pad, 0)
        | otherwise = let (d, m) = divMod int' 0x100 in Just (word8 m, d)
    pad
        | int < 0 = 0xFF
        | otherwise = 0x00

    word8 :: Integer -> Word8
    word8 = toEnum . fromEnum

evalBytes2int :: BuiltinAndAxiomSimplifier
evalBytes2int =
    Builtin.functionEvaluator evalBytes2int0
  where
    evalBytes2int0 :: Builtin.Function
    evalBytes2int0 _ resultSort [bytes, _end, _sign] = do
        _end <- matchEndianness _end
        _sign <- matchSignedness _sign
        bytes' <- expectBuiltinBytes bytes
        bytes2int bytes' _end _sign
            & Int.asPattern resultSort
            & return
    evalBytes2int0 _ _ _ = Builtin.wrongArity bytes2intKey

bytes2int :: ByteString -> Endianness -> Signedness -> Integer
bytes2int bytes end sign =
    case sign of
        Unsigned _ -> unsigned
        Signed _
            | 2 * unsigned > modulus -> unsigned - modulus
            | otherwise -> unsigned
  where
    (modulus, unsigned) = ByteString.foldl' go (1, 0) littleEndian
    go (!place, !acc) byte =
        let !place' = place * 0x100
            !acc' = acc + place * fromIntegral byte
         in (place', acc')
    littleEndian =
        case end of
            LittleEndian _ -> bytes
            BigEndian _ -> ByteString.reverse bytes

builtinFunctions :: Text -> Maybe BuiltinAndAxiomSimplifier
builtinFunctions key
    | key == bytes2StringKey = Just evalBytes2String
    | key == string2BytesKey = Just evalString2Bytes
    | key == decodeBytesKey = Just evalDecodeBytes
    | key == encodeBytesKey = Just evalEncodeBytes
    | key == updateKey = Just evalUpdate
    | key == getKey = Just evalGet
    | key == substrKey = Just evalSubstr
    | key == replaceAtKey = Just evalReplaceAt
    | key == padRightKey = Just evalPadRight
    | key == padLeftKey = Just evalPadLeft
    | key == reverseKey = Just evalReverse
    | key == lengthKey = Just evalLength
    | key == concatKey = Just evalConcat
    | key == int2bytesKey = Just evalInt2bytes
    | key == bytes2intKey = Just evalBytes2int
    | otherwise = Nothing

-- | @UnifyBytes@ matches unification problems on @\\dv{Bytes}(_)@ itself.
data UnifyBytes = UnifyBytes
    { bytes1, bytes2 :: InternalBytes
    }

{- | Matches the unification problem:

@\\dv{Bytes}(bytes1)@ with @\\dv{Bytes}(bytes2)@.
-}
matchBytes ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe UnifyBytes
matchBytes first second
    | _ :< InternalBytesF (Const bytes1) <- Recursive.project first
    , _ :< InternalBytesF (Const bytes2) <- Recursive.project second =
        Just UnifyBytes{bytes1, bytes2}
    | otherwise = Nothing
{-# INLINE matchBytes #-}

unifyBytes ::
    MonadUnify unifier =>
    UnifyBytes ->
    unifier (Pattern RewritingVariableName)
unifyBytes UnifyBytes{bytes1, bytes2}
    | bytes1 == bytes2 = return $ Pattern.fromTermLike $ mkInternalBytes' bytes1
    | otherwise = empty
{-# INLINE unifyBytes #-}
