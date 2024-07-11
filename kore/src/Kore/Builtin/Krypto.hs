{- |
Module      : Kore.Builtin.Krypto
Description : Built-in cryptographic functions.
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : vladimir.ciobanu@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Krypto as Krypto
@
-}
module Kore.Builtin.Krypto (
    verifiers,
    builtinFunctions,
    signatureToKey,

    -- * Constants
    keccak256Key,
    keccak256RawKey,
    sha256Key,
    hashSha256Key,
    sha3256Key,
    hashSha3_256Key,
    ripemd160Key,
    hashRipemd160Key,
    ecdsaRecoverKey,
    secp256k1EcdsaRecoverKey,
    hashKeccak256Key,
) where

import Crypto.Hash (
    HashAlgorithm,
    Keccak_256 (..),
    RIPEMD160 (..),
    SHA256 (..),
    SHA3_256 (..),
    SHA512t_256 (..),
    hashWith,
 )
import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types
import Crypto.Secp256k1.Internal.BaseOps qualified as Secp256k1
import Crypto.Secp256k1.Internal.Context qualified as Secp256k1
import Crypto.Secp256k1.Internal.ForeignTypes qualified as Secp256k1
import Crypto.Secp256k1.Internal.Util qualified as Secp256k1
import Data.Bits
import Data.ByteString (
    ByteString,
 )
import Data.ByteString qualified as ByteString
import Data.HashMap.Strict qualified as HashMap
import Data.String (
    IsString,
    fromString,
 )
import Data.Text (
    Text,
 )
import Data.Word (
    Word8,
 )
import Foreign (Ptr, alloca, allocaBytes, peek, poke)
import Kore.Builtin.Builtin qualified as Builtin
import Kore.Builtin.Encoding (
    decode8Bit,
    toBase16,
 )
import Kore.Builtin.Int qualified as Int
import Kore.Builtin.InternalBytes qualified as InternalBytes
import Kore.Builtin.String qualified as String
import Kore.Simplify.Simplify (
    BuiltinAndAxiomSimplifier,
 )
import Prelude.Kore
import System.IO.Unsafe (unsafePerformIO)

keccak256Key
    , keccak256RawKey
    , ecdsaRecoverKey
    , ecdsaPubKey
    , sha256Key
    , sha512_256RawKey
    , sha3256Key
    , ripemd160Key ::
        IsString s => s
keccak256Key = "KRYPTO.keccak256"
keccak256RawKey = "KRYPTO.keccak256raw"
ecdsaRecoverKey = "KRYPTO.ecdsaRecover"
ecdsaPubKey = "KRYPTO.ecdsaPubKey"
sha256Key = "KRYPTO.sha256"
sha512_256RawKey = "KRYPTO.sha512_256raw"
sha3256Key = "KRYPTO.sha3256"
ripemd160Key = "KRYPTO.ripemd160"

hashKeccak256Key
    , hashSha3_256Key
    , hashSha256Key
    , hashRipemd160Key ::
        IsString s => s
hashKeccak256Key = "HASH.keccak256"
hashSha3_256Key = "HASH.sha3_256"
hashSha256Key = "HASH.sha256"
hashRipemd160Key = "HASH.ripemd160"

secp256k1EcdsaRecoverKey :: IsString s => s
secp256k1EcdsaRecoverKey = "SECP256K1.ecdsaRecover"

verifiers :: Builtin.Verifiers
verifiers =
    Builtin.Verifiers
        { sortDeclVerifiers = mempty
        , symbolVerifiers
        , patternVerifierHook = mempty
        }

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'
-}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
        [ (keccak256Key, verifyHashFunction)
        , (keccak256RawKey, verifyHashFunctionRaw)
        , (hashKeccak256Key, verifyHashFunction)
        , (sha256Key, verifyHashFunction)
        , (hashSha256Key, verifyHashFunction)
        , (sha3256Key, verifyHashFunction)
        , (hashSha3_256Key, verifyHashFunction)
        , (sha512_256RawKey, verifyHashFunctionRaw)
        , (ripemd160Key, verifyHashFunction)
        , (hashRipemd160Key, verifyHashFunction)
        , (ecdsaPubKey, verifyHashFunction)
        ,
            ( ecdsaRecoverKey
            , Builtin.verifySymbol
                InternalBytes.assertSort
                [ InternalBytes.assertSort
                , Int.assertSort
                , InternalBytes.assertSort
                , InternalBytes.assertSort
                ]
            )
        ,
            ( secp256k1EcdsaRecoverKey
            , Builtin.verifySymbol
                InternalBytes.assertSort
                [ InternalBytes.assertSort
                , Int.assertSort
                , InternalBytes.assertSort
                , InternalBytes.assertSort
                ]
            )
        ]

-- | Implement builtin function evaluation.
builtinFunctions :: Text -> Maybe BuiltinAndAxiomSimplifier
builtinFunctions key
    | key == keccak256Key = Just evalKeccak
    | key == keccak256RawKey = Just evalKeccakRaw
    | key == hashKeccak256Key = Just evalKeccak
    | key == sha256Key = Just evalSha256
    | key == hashSha256Key = Just evalSha256
    | key == sha3256Key = Just evalSha3256
    | key == sha512_256RawKey = Just evalSha512_256Raw
    | key == hashSha3_256Key = Just evalSha3256
    | key == ripemd160Key = Just evalRipemd160
    | key == hashRipemd160Key = Just evalRipemd160
    | key == ecdsaRecoverKey = Just evalECDSARecover
    | key == ecdsaPubKey = Just evalECDSAPubKey
    | key == secp256k1EcdsaRecoverKey = Just evalECDSARecover
    | otherwise = Nothing

verifyHashFunction :: Builtin.SymbolVerifier
verifyHashFunction = Builtin.verifySymbol String.assertSort [InternalBytes.assertSort]

verifyHashFunctionRaw :: Builtin.SymbolVerifier
verifyHashFunctionRaw = Builtin.verifySymbol InternalBytes.assertSort [InternalBytes.assertSort]

{- | A function evaluator for builtin hash function hooks.

The symbol's argument is a byte string which will be interpreted as a raw sequence
of 8-bit bytes. The result is the hash as a string in big-endian base-16
encoding.
-}
evalHashFunction ::
    HashAlgorithm algorithm =>
    -- | hook name for error messages
    String ->
    -- | hash function
    algorithm ->
    BuiltinAndAxiomSimplifier
evalHashFunction context algorithm =
    Builtin.functionEvaluator evalHashFunctionWorker
  where
    evalHashFunctionWorker :: Builtin.Function
    evalHashFunctionWorker _ resultSort [input] = do
        bytes <- InternalBytes.expectBuiltinBytes input
        let digest = hashWith algorithm bytes
            result = fromString (show digest)
        return (String.asPattern resultSort result)
    evalHashFunctionWorker _ _ _ = Builtin.wrongArity context

{- | A function evaluator for builtin hash function hooks.

The symbol's argument is a byte string which will be interpreted as a raw sequence
of 8-bit bytes. The result is the hash as a raw byte string.
-}
evalHashFunctionRaw ::
    HashAlgorithm algorithm =>
    -- | hook name for error messages
    String ->
    -- | hash function
    algorithm ->
    BuiltinAndAxiomSimplifier
evalHashFunctionRaw context algorithm =
    Builtin.functionEvaluator evalHashFunctionWorker
  where
    evalHashFunctionWorker :: Builtin.Function
    evalHashFunctionWorker _ resultSort [input] = do
        bytes <- InternalBytes.expectBuiltinBytes input
        let digest = hashWith algorithm bytes
            result = decode8Bit digest
        return (String.asPattern resultSort result)
    evalHashFunctionWorker _ _ _ = Builtin.wrongArity context

evalKeccak :: BuiltinAndAxiomSimplifier
evalKeccak = evalHashFunction keccak256Key Keccak_256

evalKeccakRaw :: BuiltinAndAxiomSimplifier
evalKeccakRaw = evalHashFunctionRaw keccak256RawKey Keccak_256

evalSha256 :: BuiltinAndAxiomSimplifier
evalSha256 = evalHashFunction sha256Key SHA256

evalSha512_256Raw :: BuiltinAndAxiomSimplifier
evalSha512_256Raw = evalHashFunctionRaw sha512_256RawKey SHA512t_256

evalSha3256 :: BuiltinAndAxiomSimplifier
evalSha3256 = evalHashFunction sha3256Key SHA3_256

evalRipemd160 :: BuiltinAndAxiomSimplifier
evalRipemd160 = evalHashFunction ripemd160Key RIPEMD160

secp256k1Ctx :: Ptr Secp256k1.LCtx
secp256k1Ctx = unsafePerformIO $ Secp256k1.contextCreate Secp256k1.sign
{-# NOINLINE secp256k1Ctx #-}

evalECDSAPubKey :: BuiltinAndAxiomSimplifier
evalECDSAPubKey =
    Builtin.functionEvaluator evalWorker
  where
    evalWorker :: Builtin.Function
    evalWorker _ resultSort [input] = do
        sec_key <- InternalBytes.expectBuiltinBytes input
        return $
            String.asPattern resultSort $
                if ByteString.length sec_key /= 32
                    then ""
                    else unsafePerformIO $ Secp256k1.unsafeUseByteString sec_key $ \(sec_key_ptr, _) -> allocaBytes 64 $ \pub_key_ptr -> do
                        createdKeySuccessfully <-
                            Secp256k1.isSuccess <$> Secp256k1.ecPubKeyCreate secp256k1Ctx pub_key_ptr sec_key_ptr
                        if not createdKeySuccessfully
                            then pure ""
                            else alloca $ \len_ptr -> allocaBytes 65 $ \out_ptr -> do
                                poke len_ptr 65
                                serializedKeySuccessfully <-
                                    Secp256k1.isSuccess
                                        <$> Secp256k1.ecPubKeySerialize secp256k1Ctx out_ptr len_ptr pub_key_ptr Secp256k1.uncompressed
                                if not serializedKeySuccessfully
                                    then pure ""
                                    else do
                                        final_len <- peek len_ptr
                                        toBase16 . ByteString.tail <$> Secp256k1.packByteString (out_ptr, final_len)
    evalWorker _ _ _ = Builtin.wrongArity ecdsaPubKey

evalECDSARecover :: BuiltinAndAxiomSimplifier
evalECDSARecover =
    Builtin.functionEvaluator eval0
  where
    eval0 :: Builtin.Function
    eval0 _ resultSort [messageHash0, v0, r0, s0] = do
        messageHash <-
            bstring2Integer <$> InternalBytes.expectBuiltinBytes messageHash0
        v <- Int.expectBuiltinInt "" v0
        r <-
            bstring2Integer <$> InternalBytes.expectBuiltinBytes r0
        s <-
            bstring2Integer <$> InternalBytes.expectBuiltinBytes s0
        pad 64 0 (signatureToKey messageHash r s v)
            & InternalBytes.asPattern resultSort
            & return
    eval0 _ _ _ = Builtin.wrongArity ecdsaRecoverKey

pad :: Int -> Word8 -> ByteString -> ByteString
pad n w s = ByteString.append s padding
  where
    padding =
        ByteString.replicate (n - ByteString.length s) w

signatureToKey ::
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    ByteString
signatureToKey messageHash r s v =
    assert (27 <= v && v <= 34) $
        ByteString.drop 1 $
            encodePoint compressed $
                recoverPublicKey recId (r, s) messageHash
  where
    recId = v - 27
    compressed = v >= 31

recoverPublicKey ::
    Integer ->
    (Integer, Integer) ->
    Integer ->
    Point
recoverPublicKey recId (r, s) e =
    assert (recId >= 0) $
        assert (r >= 0) $
            assert (s >= 0) $
                assert (pt_x <= p) $
                    assert (pointMul p256k1 n pt == PointO) $
                        pointAddTwoMuls
                            p256k1
                            (mulMod n (invMod n r) s)
                            pt
                            (mulMod n (invMod n r) (n - e `mod` n))
                            (ecc_g curveParams)
  where
    p256k1 = getCurveByName SEC_p256k1
    CurvePrime p curveParams =
        case p256k1 of
            CurveFP curvePrime@(CurvePrime _ _) -> curvePrime
            _ -> error "Expected CurveFP!"

    n = ecc_n curveParams

    i = recId `div` 2

    pt_x = r + i * n

    pt = decompressPt pt_x (recId .&. 1 == 1)

    decompressPt x signBit = Point x (if signBit /= even y then y else p - y)
      where
        y =
            sqrtMod
                p
                ( powMod p x 3
                    + mulMod p (ecc_a curveParams) x
                    + ecc_b curveParams
                )

invMod ::
    Integer ->
    Integer ->
    Integer
invMod p x = powMod p x (p - 2)

sqrtMod ::
    Integer ->
    Integer ->
    Integer
sqrtMod p x = powMod p x ((p + 1) `div` 4)

mulMod ::
    Integer ->
    Integer ->
    Integer ->
    Integer
mulMod p x y = (x * y) `mod` p

powMod ::
    Integer ->
    Integer ->
    Integer ->
    Integer
powMod _ _ 0 = 1
powMod p x a =
    mulMod
        p
        (if even a then 1 else x)
        (powMod p (mulMod p x x) (a `div` 2))

-- Leading byte signals whether the point is compressed.
-- Superfluous because we drop it later on.
-- Kept here for completeness sake, to match
-- the code in the java backend.
encodePoint ::
    HasCallStack =>
    Bool ->
    Point ->
    ByteString
encodePoint compressed (Point x y)
    | compressed =
        ByteString.cons
            (if even y then 0x02 else 0x03)
            (integer2ByteString x)
    | otherwise =
        ByteString.concat
            [ ByteString.pack [0x04]
            , integer2ByteString x
            , integer2ByteString y
            ]
encodePoint _ _ = error "Should never obtain point-at-infinity here!"

{- | Interprets a 'ByteString' as an 'Integer' in big-endian unsigned
 representation.
-}
bstring2Integer :: ByteString -> Integer
bstring2Integer =
    ByteString.foldr (\word i -> fromIntegral word + (i `shiftL` 8)) 0
        . ByteString.reverse

-- | Converts an Integer to its big-endian unsigned representation.
integer2ByteString :: Integer -> ByteString
integer2ByteString =
    ByteString.reverse . ByteString.unfoldr integer2ByteStringWorker
  where
    integer2ByteStringWorker i
        | i == 0 = Nothing
        | otherwise = Just (fromIntegral r, q)
      where
        (q, r) = divMod i 256
