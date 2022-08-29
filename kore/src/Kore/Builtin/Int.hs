{-# LANGUAGE MagicHash #-}

{- |
Module      : Kore.Builtin.Int
Description : Built-in arbitrary-precision integer sort
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Int as Int
@
-}
module Kore.Builtin.Int (
    sort,
    assertSort,
    verifiers,
    builtinFunctions,
    expectBuiltinInt,
    extractIntDomainValue,
    asInternal,
    asPattern,
    asPartialPattern,
    parse,
    unifyInt,
    matchInt,
    matchUnifyIntEq,

    -- * keys
    randKey,
    srandKey,
    gtKey,
    geKey,
    eqKey,
    leKey,
    ltKey,
    neKey,
    minKey,
    maxKey,
    addKey,
    subKey,
    mulKey,
    absKey,
    edivKey,
    emodKey,
    tdivKey,
    tmodKey,
    andKey,
    orKey,
    xorKey,
    notKey,
    shlKey,
    shrKey,
    powKey,
    powmodKey,
    log2Key,

    -- * implementations (for testing)
    tdiv,
    tmod,
    ediv,
    emod,
    pow,
    powmod,
    log2,
) where

import Control.Error (
    MaybeT,
 )
import Control.Monad qualified as Monad
import Data.Bits (
    complement,
    shift,
    xor,
    (.&.),
    (.|.),
 )
import Data.Functor.Const
import Data.HashMap.Strict qualified as HashMap
import Data.Text (
    Text,
 )
import GHC.Integer (
    smallInteger,
 )
import GHC.Integer.GMP.Internals (
    powModInteger,
    recipModInteger,
 )
import GHC.Integer.Logarithms (
    integerLog2#,
 )
import Kore.Builtin.Bool qualified as Bool
import Kore.Builtin.Builtin (
    UnifyEq (..),
 )
import Kore.Builtin.Builtin qualified as Builtin
import Kore.Builtin.Int.Int
import Kore.Error qualified
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.InternalInt
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    makeCeilPredicate,
 )
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.TermLike as TermLike
import Kore.Log.DebugUnifyBottom (debugUnifyBottomAndReturnBottom)
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Simplify (
    AttemptedAxiom,
    Simplifier,
 )
import Kore.Unification.Unify as Unify
import Prelude.Kore
import Text.Megaparsec.Char.Lexer qualified as Parsec

{- | Verify that the sort is hooked to the builtin @Int@ sort.

  See also: 'sort', 'Builtin.verifySort'
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

  See also: 'Builtin.verifySortDecl'
-}
sortDeclVerifiers :: Builtin.SortDeclVerifiers
sortDeclVerifiers = HashMap.fromList [(sort, Builtin.verifySortDecl)]

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'
-}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
        [ (randKey, Builtin.verifySymbol assertSort [assertSort])
        , (srandKey, Builtin.verifySymbolArguments [assertSort])
        , (gtKey, Builtin.verifySymbol Bool.assertSort [assertSort, assertSort])
        , (geKey, Builtin.verifySymbol Bool.assertSort [assertSort, assertSort])
        , (eqKey, Builtin.verifySymbol Bool.assertSort [assertSort, assertSort])
        , (leKey, Builtin.verifySymbol Bool.assertSort [assertSort, assertSort])
        , (ltKey, Builtin.verifySymbol Bool.assertSort [assertSort, assertSort])
        , (neKey, Builtin.verifySymbol Bool.assertSort [assertSort, assertSort])
        , -- Ordering operations
          (minKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (maxKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , -- Arithmetic operations
          (addKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (subKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (mulKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (absKey, Builtin.verifySymbol assertSort [assertSort])
        , (edivKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (emodKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (tdivKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (tmodKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , -- Bitwise operations
          (andKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (orKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (xorKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (notKey, Builtin.verifySymbol assertSort [assertSort])
        , (shlKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , (shrKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        , -- Exponential and logarithmic operations
          (powKey, Builtin.verifySymbol assertSort [assertSort, assertSort])
        ,
            ( powmodKey
            , Builtin.verifySymbol assertSort [assertSort, assertSort, assertSort]
            )
        , (log2Key, Builtin.verifySymbol assertSort [assertSort])
        ]

-- | Verify that domain value patterns are well-formed.
patternVerifierHook :: Builtin.PatternVerifierHook
patternVerifierHook =
    Builtin.domainValuePatternVerifierHook sort patternVerifierWorker
  where
    patternVerifierWorker external =
        case externalChild of
            StringLiteral_ lit -> do
                internalIntValue <- Builtin.parseString parse lit
                (return . InternalIntF . Const)
                    InternalInt
                        { internalIntSort
                        , internalIntValue
                        }
            _ -> Kore.Error.koreFail "Expected literal string"
      where
        DomainValue{domainValueSort = internalIntSort} = external
        DomainValue{domainValueChild = externalChild} = external

-- | get the value from a (possibly encoded) domain value
extractIntDomainValue ::
    -- | error message Context
    Text ->
    TermLike variable ->
    Maybe Integer
extractIntDomainValue _ =
    \case
        InternalInt_ InternalInt{internalIntValue} -> Just internalIntValue
        _ -> Nothing

-- | Parse a string literal as an integer.
parse :: Builtin.Parser Integer
parse = Parsec.signed noSpace Parsec.decimal
  where
    noSpace = pure ()

{- | Abort function evaluation if the argument is not a Int domain value.

If the operand pattern is not a domain value, the function is simply
'NotApplicable'. If the operand is a domain value, but not represented
by a 'BuiltinDomainMap', it is a bug.
-}
expectBuiltinInt ::
    Monad m =>
    -- | Context for error message
    Text ->
    -- | Operand pattern
    TermLike variable ->
    MaybeT m Integer
expectBuiltinInt _ =
    \case
        InternalInt_ InternalInt{internalIntValue} -> return internalIntValue
        _ -> empty

-- | Implement builtin function evaluation.
builtinFunctions ::
    Text ->
    TermLike RewritingVariableName ->
    SideCondition RewritingVariableName ->
    Maybe (Simplifier (AttemptedAxiom RewritingVariableName))
builtinFunctions key termLike sideCondition
    -- TODO (thomas.tuegel): Add MonadRandom to evaluation context to
    -- implement rand and srand.
    | key == randKey = Just $ Builtin.notImplemented termLike sideCondition
    | key == srandKey = Just $ Builtin.notImplemented termLike sideCondition
    | key == gtKey = Just $ comparator gtKey (>) termLike sideCondition
    | key == geKey = Just $ comparator geKey (>=) termLike sideCondition
    | key == eqKey = Just $ Builtin.functionEvaluator evalEq termLike sideCondition
    | key == leKey = Just $ comparator leKey (<=) termLike sideCondition
    | key == ltKey = Just $ comparator ltKey (<) termLike sideCondition
    | key == neKey = Just $ comparator neKey (/=) termLike sideCondition
    -- Ordering operations
    | key == minKey = Just $ binaryOperator minKey min termLike sideCondition
    | key == maxKey = Just $ binaryOperator maxKey max termLike sideCondition
    -- Arithmetic operations
    | key == addKey = Just $ binaryOperator addKey (+) termLike sideCondition
    | key == subKey = Just $ binaryOperator subKey (-) termLike sideCondition
    | key == mulKey = Just $ binaryOperator mulKey (*) termLike sideCondition
    | key == absKey = Just $ unaryOperator absKey abs termLike sideCondition
    -- Division operations
    | key == edivKey = Just $ partialBinaryOperator edivKey ediv termLike sideCondition
    | key == emodKey = Just $ partialBinaryOperator emodKey emod termLike sideCondition
    | key == tdivKey = Just $ partialBinaryOperator tdivKey tdiv termLike sideCondition
    | key == tmodKey = Just $ partialBinaryOperator tmodKey tmod termLike sideCondition
    -- Bitwise operations
    | key == andKey = Just $ binaryOperator andKey (.&.) termLike sideCondition
    | key == orKey = Just $ binaryOperator orKey (.|.) termLike sideCondition
    | key == xorKey = Just $ binaryOperator xorKey xor termLike sideCondition
    | key == notKey = Just $ unaryOperator notKey complement termLike sideCondition
    | key == shlKey = Just $ binaryOperator shlKey (\a -> shift a . fromInteger) termLike sideCondition
    | key == shrKey = Just $ binaryOperator shrKey (\a -> shift a . fromInteger . negate) termLike sideCondition
    -- Exponential and logarithmic operations
    | key == powKey = Just $ partialBinaryOperator powKey pow termLike sideCondition
    | key == powmodKey = Just $ partialTernaryOperator powmodKey powmod termLike sideCondition
    | key == log2Key = Just $ partialUnaryOperator log2Key log2 termLike sideCondition
    | otherwise = Nothing
  where
    unaryOperator name op =
        Builtin.unaryOperator
            extractIntDomainValue
            asPattern
            name
            op
    binaryOperator name op =
        Builtin.binaryOperator
            extractIntDomainValue
            asPattern
            name
            op
    comparator name op =
        Builtin.binaryOperator
            extractIntDomainValue
            Bool.asPattern
            name
            op
    partialUnaryOperator name op =
        Builtin.unaryOperator
            extractIntDomainValue
            asPartialPattern
            name
            op
    partialBinaryOperator name op =
        Builtin.binaryOperator
            extractIntDomainValue
            asPartialPattern
            name
            op
    partialTernaryOperator name op =
        Builtin.ternaryOperator
            extractIntDomainValue
            asPartialPattern
            name
            op

tdiv
    , tmod
    , ediv
    , emod
    , pow ::
        Integer -> Integer -> Maybe Integer
tdiv n d
    | d == 0 = Nothing
    | otherwise = Just (quot n d)
tmod n d
    | d == 0 = Nothing
    | otherwise = Just (rem n d)
ediv n d
    | d == 0 = Nothing
    | n < 0
      , d < 0
      , mod n d /= 0 =
        Just (1 + div (-n) (-d))
    | d < 0 = Just (quot n d)
    | otherwise = Just (div n d)
emod n d
    | d == 0 = Nothing
    | n < 0
      , d < 0
      , mod n d /= 0 =
        Just (n - d * (1 + div (-n) (-d)))
    | d < 0 = Just (rem n d)
    | otherwise = Just (mod n d)
pow b e
    | e < 0 = Nothing
    | otherwise = Just (b ^ e)

log2 ::
    Integer -> Maybe Integer
log2 n
    | n > 0 = Just (smallInteger (integerLog2# n))
    | otherwise = Nothing

powmod ::
    Integer -> Integer -> Integer -> Maybe Integer
powmod b e m
    | m == 0 = Nothing
    | e < 0 && recipModInteger b m == 0 = Nothing
    | otherwise = Just (powModInteger b e m)

evalEq :: Builtin.Function
evalEq sideCondition resultSort arguments@[_intLeft, _intRight] =
    concrete <|> symbolicReflexivity
  where
    concrete = do
        _intLeft <- expectBuiltinInt eqKey _intLeft
        _intRight <- expectBuiltinInt eqKey _intRight
        _intLeft == _intRight
            & Bool.asPattern resultSort
            & return

    symbolicReflexivity = do
        Monad.guard (TermLike.isFunctionPattern _intLeft)
        -- Do not need to check _intRight because we only return a result
        -- when _intLeft and _intRight are equal.
        if _intLeft == _intRight
            then True & Bool.asPattern resultSort & returnPattern
            else empty

    mkCeilUnlessDefined termLike
        | SideCondition.isDefined sideCondition termLike = Condition.top
        | otherwise =
            Condition.fromPredicate (makeCeilPredicate termLike)
    returnPattern = return . flip Pattern.andCondition conditions
    conditions = foldMap mkCeilUnlessDefined arguments
evalEq _ _ _ = Builtin.wrongArity eqKey

data UnifyInt = UnifyInt
    { int1, int2 :: !InternalInt
    , term1, term2 :: !(TermLike RewritingVariableName)
    }

{- | Matches

@
\\equals{_, _}(\\dv{Int}(_), \\dv{Int}(_))
@

and

@
\\and{_}(\\dv{Int}(_), \\dv{Int}(_))
@
-}
matchInt ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe UnifyInt
matchInt term1 term2
    | InternalInt_ int1 <- term1
      , InternalInt_ int2 <- term2 =
        Just UnifyInt{int1, int2, term1, term2}
    | otherwise = Nothing
{-# INLINE matchInt #-}

-- | When int values are equal, returns first term; otherwise returns bottom.
unifyInt ::
    forall unifier.
    MonadUnify unifier =>
    UnifyInt ->
    unifier (Pattern RewritingVariableName)
unifyInt unifyData =
    assert (on (==) internalIntSort int1 int2) worker
  where
    UnifyInt{int1, int2, term1, term2} = unifyData
    worker :: unifier (Pattern RewritingVariableName)
    worker
        | on (==) internalIntValue int1 int2 =
            return $ Pattern.fromTermLike term1
        | otherwise =
            debugUnifyBottomAndReturnBottom "distinct integers" term1 term2

{- | Matches
@
\\equals{_, _}(eqInt{_}(_, _), \\dv{Bool}(_)),
@

symmetric in the two arguments.
-}
matchUnifyIntEq ::
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    Maybe UnifyEq
matchUnifyIntEq = Builtin.matchUnifyEq eqKey
{-# INLINE matchUnifyIntEq #-}
