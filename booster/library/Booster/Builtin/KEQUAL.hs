{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause

Built-in functions (hooks) in the KEQUAL namespace, as described in
[docs/hooks.md](https://github.com/runtimeverification/haskell-backend/blob/master/docs/hooks.md).

* Depends on the built-in Bool type.
-}
module Booster.Builtin.KEQUAL (
    builtinsKEQUAL,
) where

import Control.Monad
import Control.Monad.Trans.Except (throwE)
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Prettyprinter

import Booster.Builtin.Base
import Booster.Pattern.Base
import Booster.Pattern.Bool
import Booster.Pattern.Pretty
import Booster.Pattern.Util (isConstructorSymbol, sortOfTerm)
import Booster.Prettyprinter (renderText)

builtinsKEQUAL :: Map ByteString BuiltinFunction
builtinsKEQUAL =
    Map.fromList
        [ "KEQUAL.ite" ~~> iteHook
        , "KEQUAL.eq" ~~> equalsKHook
        , "KEQUAL.ne" ~~> nequalsKHook
        ]

iteHook :: BuiltinFunction
iteHook args
    | [cond, thenVal, elseVal] <- args = do
        cond `shouldHaveSort` "SortBool"
        unless (sortOfTerm thenVal == sortOfTerm elseVal) $
            throwE . Text.unlines $
                [ "Different sorts in alternatives:"
                , renderText $ pretty (PrettyWithModifiers @['Decoded, 'Truncated] thenVal)
                , renderText $ pretty (PrettyWithModifiers @['Decoded, 'Truncated] elseVal)
                ]
        case cond of
            TrueBool -> pure $ Just thenVal
            FalseBool -> pure $ Just elseVal
            _other -> pure Nothing
    | otherwise =
        arityError "KEQUAL.ite" 3 args

equalsKHook :: BuiltinFunction
equalsKHook args
    | [KSeq _ l, KSeq _ r] <- args = pure $ evalEqualsK l r
    | otherwise =
        arityError "KEQUAL.eq" 2 args

nequalsKHook :: BuiltinFunction
nequalsKHook args
    | [KSeq _ l, KSeq _ r] <- args = pure $ negateBool <$> evalEqualsK l r
    | otherwise =
        arityError "KEQUAL.ne" 2 args

evalEqualsK :: Term -> Term -> Maybe Term
evalEqualsK (SymbolApplication sL _ argsL) (SymbolApplication sR _ argsR)
    | isConstructorSymbol sL && isConstructorSymbol sR =
        if sL == sR
            then foldAndBool <$> zipWithM evalEqualsK argsL argsR
            else pure FalseBool
evalEqualsK (SymbolApplication symbol _ _) DomainValue{}
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK (SymbolApplication symbol _ _) Injection{}
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK (SymbolApplication symbol _ _) KMap{}
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK (SymbolApplication symbol _ _) KList{}
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK (SymbolApplication symbol _ _) KSet{}
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK DomainValue{} (SymbolApplication symbol _ _)
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK Injection{} (SymbolApplication symbol _ _)
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK KMap{} (SymbolApplication symbol _ _)
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK KList{} (SymbolApplication symbol _ _)
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK KSet{} (SymbolApplication symbol _ _)
    | isConstructorSymbol symbol = pure FalseBool
evalEqualsK (Injection s1L s2L l) (Injection s1R s2R r)
    | s1L == s1R && s2L == s2R = evalEqualsK l r
evalEqualsK l@DomainValue{} r@DomainValue{} =
    pure $ if l == r then TrueBool else FalseBool
evalEqualsK l r =
    if l == r
        then pure TrueBool
        else fail "cannot evaluate" -- i.e., result is Nothing
