{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright : (c) Runtime Verification 2021
License   : BSD-3-Clause
-}
module Kore.Log.ErrorEquationRightFunction (
    ErrorEquationRightFunction (..),
    errorEquationRightFunction,
) where

import Control.Exception (
    Exception (..),
    throw,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom (
    Axiom (..),
 )
import Kore.Equation.Equation (
    Equation (..),
 )
import Kore.Internal.TermLike (
    VariableName,
 )
import Log (
    Entry (..),
    Severity (Error),
    SimpleContext (CtxError),
    SomeEntry (SomeEntry),
    single,
 )
import Prelude.Kore
import Pretty (
    Pretty,
    indent,
    pretty,
    vsep,
 )
import SQL (
    Table,
 )

-- | Error when RHS of equation is not a function pattern.
newtype ErrorEquationRightFunction = ErrorEquationRightFunction
    { equation :: Equation VariableName
    }
    deriving stock (Show, GHC.Generic)

instance SOP.Generic ErrorEquationRightFunction

instance SOP.HasDatatypeInfo ErrorEquationRightFunction

instance Pretty ErrorEquationRightFunction where
    pretty ErrorEquationRightFunction{equation} =
        vsep
            [ "Checking equation"
            , indent 4 $ pretty equation
            , "right-hand side is not a function pattern."
            ]

instance Exception ErrorEquationRightFunction where
    toException = toException . SomeEntry []
    fromException exn =
        fromException exn >>= fromEntry

instance Entry ErrorEquationRightFunction where
    entrySeverity _ = Error
    oneLineDoc
        ( ErrorEquationRightFunction
                Equation{attributes = Axiom{sourceLocation}}
            ) =
            pretty sourceLocation
    oneLineContextDoc _ = single CtxError
    helpDoc _ = "errors raised when right-hand side of equation is not a function pattern"

instance SQL.Table ErrorEquationRightFunction

-- | Error when RHS of equation is not a function pattern.
errorEquationRightFunction :: Equation VariableName -> m ()
errorEquationRightFunction = throw . ErrorEquationRightFunction
