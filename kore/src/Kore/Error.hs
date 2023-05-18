{- |
Module      : Kore.Error
Description : Kore error handling.
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.Error (
    Error (..),
    printError,
    koreError,
    koreFail,
    koreFailText,
    koreFailWhen,
    koreFailWhenText,
    withContext,
    withTextContext,
    castError,
    assertRight,
    module Control.Monad.Except,
) where

import Control.Monad.Except (
    MonadError (..),
 )
import Data.Aeson.Types (ToJSON)
import Data.Binary (Binary)
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import Deriving.Aeson (
    CamelToKebab,
    CustomJSON (..),
    FieldLabelModifier,
    StripPrefix,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Debug
import Prelude.Kore
import Pretty qualified

{- | 'Error' represents a Kore error with a stacktrace-like context.

The 'a' phantom type is used to differentiate between various kinds of error.
-}
data Error a = Error
    { errorContext :: ![String]
    , errorError :: !String
    }
    deriving stock (Eq, GHC.Generic, Show)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving
        (ToJSON)
        via CustomJSON '[FieldLabelModifier '[StripPrefix "error", CamelToKebab]] (Error a)

-- | 'printError' provides a one-line representation of a string.
printError :: Error a -> String
printError e@(Error _ _) =
    (show . Pretty.vsep)
        [ "Error:"
        , (Pretty.indent 2 . Pretty.vsep) (printContext <$> errorContext e)
        , (Pretty.indent 4 . Pretty.pretty) (errorError e)
        ]
  where
    printContext ctx = Pretty.pretty ctx <> Pretty.colon

-- | 'koreError' constructs an error object with an empty context.
koreError :: String -> Error a
koreError err =
    Error
        { errorContext = []
        , errorError = err
        }

-- | 'koreFail' produces an error result with an empty context.
koreFail :: MonadError (Error a) m => String -> m b
koreFail errorMessage =
    throwError (koreError errorMessage)

-- | 'koreFailText' produces an error result with an empty context.
koreFailText :: MonadError (Error a) m => Text -> m b
koreFailText errorMessage =
    throwError (koreError (Text.unpack errorMessage))

{- | 'koreFailWhen' produces an error result with an empty context whenever the
provided flag is true.
-}
koreFailWhen :: MonadError (Error a) m => Bool -> String -> m ()
koreFailWhen condition errorMessage =
    when condition (koreFail errorMessage)

{- | 'koreFailWhen' produces an error result with an empty context whenever the
provided flag is true.
-}
koreFailWhenText :: MonadError (Error a) m => Bool -> Text -> m ()
koreFailWhenText condition errorMessage =
    koreFailWhen condition (Text.unpack errorMessage)

{- | 'withContext' prepends the given string to the context whenever the given
action fails.
-}
withContext :: MonadError (Error a) m => String -> m result -> m result
withContext ~localContext action =
    catchError action inContext
  where
    inContext err@Error{errorContext} =
        throwError err{errorContext = localContext : errorContext}

{- | 'withContext' prepends the given text to the context whenever the given
action fails.
-}
withTextContext :: MonadError (Error a) m => Text -> m result -> m result
withTextContext ~localContext = withContext (Text.unpack localContext)

-- | 'castError' changes an error's tag.
castError :: Either (Error a) result -> Either (Error b) result
castError
    ( Left
            Error
                { errorContext = context
                , errorError = err
                }
        ) =
        Left
            Error
                { errorContext = context
                , errorError = err
                }
castError (Right r) = Right r

{- | `error` with a helpful message in case of `Left`.
 | Otherwise, return what `Right` returns.
-}
assertRight :: Either (Error err) desired -> desired
assertRight wrapped =
    case wrapped of
        Left err -> error (printError err)
        Right desired -> desired
