{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugUnifyBottom (
    DebugUnifyBottom (..),
    mkDebugUnifyBottom,
    debugUnifyBottom,
    debugUnifyBottomAndReturnBottom,
) where

import Data.Text (
    Text,
 )
import Kore.Internal.TermLike (
    InternalVariable,
    TermLike (..),
    VariableName,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Unparser (unparse)
import Log (
    CLContext (CLNullary),
    Entry (..),
    MonadLog (..),
    Severity (..),
    SimpleContext (CtxFailure, CtxUnify),
    logEntry,
 )
import Logic (
    MonadLogic,
 )
import Prelude.Kore
import Pretty (
    Pretty,
    pretty,
    unAnnotate,
 )
import Pretty qualified

data DebugUnifyBottom = DebugUnifyBottom
    { info :: Text
    , first :: TermLike VariableName
    , second :: TermLike VariableName
    }
    deriving stock (Show, Eq)

instance Pretty DebugUnifyBottom where
    pretty (DebugUnifyBottom info first second) =
        Pretty.vsep
            [ unAnnotate $ pretty info
            , "When unifying:"
            , Pretty.indent 4 . unparse $ first
            , "With:"
            , Pretty.indent 4 . unparse $ second
            ]

instance Entry DebugUnifyBottom where
    entrySeverity _ = Debug
    oneLineDoc _ = "DebugUnifyBottom"
    oneLineContextDoc _ = map CLNullary [CtxUnify, CtxFailure]
    helpDoc _ = "log failed unification"

mkDebugUnifyBottom ::
    InternalVariable variable =>
    Text ->
    TermLike variable ->
    TermLike variable ->
    DebugUnifyBottom
mkDebugUnifyBottom info first second =
    DebugUnifyBottom
        info
        (TermLike.mapVariables (pure $ from @_ @VariableName) first)
        (TermLike.mapVariables (pure $ from @_ @VariableName) second)

debugUnifyBottom ::
    MonadLog log =>
    InternalVariable variable =>
    Text ->
    TermLike variable ->
    TermLike variable ->
    log ()
debugUnifyBottom info first second =
    logEntry $
        DebugUnifyBottom
            info
            (TermLike.mapVariables (pure $ from @_ @VariableName) first)
            (TermLike.mapVariables (pure $ from @_ @VariableName) second)

debugUnifyBottomAndReturnBottom ::
    MonadLog log =>
    MonadLogic log =>
    InternalVariable variable =>
    Text ->
    TermLike variable ->
    TermLike variable ->
    log a
debugUnifyBottomAndReturnBottom info first second = do
    debugUnifyBottom
        info
        first
        second
    empty
