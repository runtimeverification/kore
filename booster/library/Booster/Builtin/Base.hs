{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause

Base type definitions and helpers for built-in functions (hooks).
-}
module Booster.Builtin.Base (
    BuiltinFunction,
    -- helpers
    (~~>),
    arityError,
    isConstructorLike_,
    shouldHaveSort,
) where

import Control.Monad.Trans.Except
import Data.ByteString.Char8 (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Prettyprinter (pretty)

import Booster.Pattern.Base
import Booster.Pattern.Pretty
import Booster.Pattern.Util
import Booster.Prettyprinter

{- |

Built-in functions may fail on arity or sort errors, and may be
partial (returning a Maybe type)

The built-in function fails outright when its function is called with
a wrong argument count. When required arguments are unevaluated, the
hook returns 'Nothing'.
-}
type BuiltinFunction = [Term] -> Except Text (Maybe Term)

------------------------------------------------------------
-- Helpers

(~~>) :: ByteString -> BuiltinFunction -> (ByteString, BuiltinFunction)
(~~>) = (,)

isConstructorLike_ :: Term -> Bool
isConstructorLike_ = (.isConstructorLike) . getAttributes

{- | checks that the arguments list has the expected length.

Returns nothing if the arg.count matches, so it can be used as a
fall-through case in hook function definitions.
-}
arityError :: Text -> Int -> [Term] -> Except Text (Maybe Term)
arityError fname argCount args
    | l == argCount =
        pure Nothing
    | otherwise =
        throwE $ fname <> Text.pack msg
  where
    l = length args
    msg = unwords [": wrong arity. Expected ", show argCount, ", got ", show l]

-- check for simple (parameter-less) sorts
shouldHaveSort :: Term -> SortName -> Except Text ()
t `shouldHaveSort` s
    | sortOfTerm t == SortApp s [] =
        pure ()
    | otherwise =
        throwE $
            Text.unlines
                [ "Argument term has unexpected sort (expected " <> Text.decodeLatin1 s <> "):"
                , renderText (pretty (PrettyWithModifiers @['Decoded, 'Truncated] t))
                ]
