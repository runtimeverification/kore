{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Log.ErrorException (
    ErrorException,
    errorException,
    handleSomeException,
) where

import Control.Exception (
    AssertionFailed,
 )
import Control.Monad.Catch (
    MonadThrow,
    SomeException,
    fromException,
    throwM,
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
    hsep,
    prettyException,
    vsep,
 )

newtype ErrorException = ErrorException {getException :: SomeException}
    deriving stock (Show)

instance Pretty ErrorException where
    pretty (ErrorException someException) =
        (vsep . catMaybes)
            [ Just $ prettyException someException
            , pleaseFileBugReport
            ]
      where
        pleaseFileBugReport = do
            _ <- fromException someException :: Maybe AssertionFailed
            (pure . hsep)
                [ "Please file a bug report:"
                , "https://github.com/runtimeverification/haskell-backend/issues"
                ]

instance Entry ErrorException where
    entrySeverity _ = Error
    oneLineDoc _ = "ErrorException"
    oneLineContextDoc _ = single CtxError
    helpDoc _ = "log internal errors"

errorException :: MonadLog log => SomeException -> log ()
errorException = logEntry . ErrorException

{- | Handle and catch exceptions. If 'SomeException' is 'cast' to 'SomeEntry'
 using 'fromException' then log the entry. If it
 is not 'SomeEntry' then use 'errorException' to wrap the exception within
 'ErrorException' and log the entry. Lastly, re-throw the exception.
-}
handleSomeException ::
    MonadLog m =>
    MonadThrow m =>
    SomeException ->
    m a
handleSomeException someException = do
    case fromException someException of
        Just entry@(SomeEntry _ _) -> logEntry entry
        Nothing -> errorException someException
    throwM someException
