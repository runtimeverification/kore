{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.ErrorOutOfDate (
    ErrorOutOfDate (..),
    errorOutOfDate,
) where

import Control.Monad.Catch (
    Exception (..),
    MonadThrow,
    throwM,
 )
import Log hiding (message)
import Prelude.Kore
import Pretty

newtype ErrorOutOfDate = ErrorOutOfDate {message :: String}
    deriving stock (Show)

instance Exception ErrorOutOfDate where
    toException = toException . SomeEntry []
    fromException exn = fromException exn >>= fromEntry
    displayException = message

instance Pretty ErrorOutOfDate where
    pretty ErrorOutOfDate{message} =
        Pretty.pretty message

instance Entry ErrorOutOfDate where
    entrySeverity _ = Error
    oneLineDoc _ = "ErrorOutOfDate"
    oneLineContextDoc _ = single CtxError

errorOutOfDate :: MonadThrow log => String -> log a
errorOutOfDate message =
    throwM ErrorOutOfDate{message}
