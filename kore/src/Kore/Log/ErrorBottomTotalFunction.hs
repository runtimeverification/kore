{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.ErrorBottomTotalFunction (
    ErrorBottomTotalFunction (..),
    errorBottomTotalFunction,
) where

import Control.Monad.Catch (
    Exception (..),
    MonadThrow,
    throwM,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Internal.TermLike
import Kore.Unparser (
    unparse,
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified
import SQL qualified

newtype ErrorBottomTotalFunction = ErrorBottomTotalFunction
    { term :: TermLike VariableName
    }
    deriving stock (Show)
    deriving stock (GHC.Generic)

instance SOP.Generic ErrorBottomTotalFunction

instance SOP.HasDatatypeInfo ErrorBottomTotalFunction

instance Pretty ErrorBottomTotalFunction where
    pretty ErrorBottomTotalFunction{term} =
        Pretty.vsep
            [ "Evaluating total function"
            , Pretty.indent 4 (unparse term)
            , "has resulted in \\bottom."
            ]

instance Exception ErrorBottomTotalFunction where
    toException = toException . SomeEntry []
    fromException exn =
        fromException exn >>= fromEntry

instance Entry ErrorBottomTotalFunction where
    entrySeverity _ = Error
    oneLineDoc _ = "ErrorBottomTotalFunction"
    oneLineContextDoc _ = single CtxError
    helpDoc _ = "errors raised when a total function is undefined"

instance SQL.Table ErrorBottomTotalFunction

errorBottomTotalFunction ::
    MonadThrow logger =>
    InternalVariable variable =>
    TermLike variable ->
    logger ()
errorBottomTotalFunction (mapVariables (pure toVariableName) -> term) =
    throwM ErrorBottomTotalFunction{term}
