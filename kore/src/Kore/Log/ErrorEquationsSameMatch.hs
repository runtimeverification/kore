{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright : (c) Runtime Verification 2021
License   : BSD-3-Clause
-}
module Kore.Log.ErrorEquationsSameMatch (
    ErrorEquationsSameMatch (..),
    errorEquationsSameMatch,
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
    comma,
    hsep,
    indent,
    pretty,
    vsep,
 )
import SQL (
    Table,
 )

-- | Error when two equations both match a term.
data ErrorEquationsSameMatch = ErrorEquationsSameMatch
    { equation1, equation2 :: Equation VariableName
    }
    deriving stock (Show, GHC.Generic)

instance SOP.Generic ErrorEquationsSameMatch

instance SOP.HasDatatypeInfo ErrorEquationsSameMatch

instance Pretty ErrorEquationsSameMatch where
    pretty ErrorEquationsSameMatch{equation1, equation2} =
        vsep
            [ "Equations"
            , indent 4 $ pretty equation1
            , "and"
            , indent 4 $ pretty equation2
            , "match the same term."
            ]

instance Exception ErrorEquationsSameMatch where
    toException = toException . SomeEntry []
    fromException exn =
        fromException exn >>= fromEntry

instance Entry ErrorEquationsSameMatch where
    entrySeverity _ = Error
    helpDoc _ =
        "errors raised when two equations from a\
        \ function definition can match the same term"
    oneLineDoc
        ( ErrorEquationsSameMatch
                Equation{attributes = Axiom{sourceLocation = sourceLoc1}}
                Equation{attributes = Axiom{sourceLocation = sourceLoc2}}
            ) =
            Pretty.hsep
                [ pretty sourceLoc1
                , Pretty.comma
                , pretty sourceLoc2
                ]
    oneLineContextDoc _ = single CtxError

instance SQL.Table ErrorEquationsSameMatch

errorEquationsSameMatch ::
    Equation VariableName ->
    Equation VariableName ->
    m ()
errorEquationsSameMatch eq1 = throw . ErrorEquationsSameMatch eq1
