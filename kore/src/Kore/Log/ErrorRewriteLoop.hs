{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.ErrorRewriteLoop (
    ErrorRewriteLoop,
    errorRewriteLoop,
) where

import Control.Exception (
    Exception (..),
    throw,
 )
import GHC.Exception (
    prettyCallStackLines,
 )
import GHC.Stack (
    CallStack,
    callStack,
 )
import Kore.Attribute.Axiom (
    Axiom (..),
    SourceLocation,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Rewrite.RulePattern (
    RewriteRule,
    RulePattern (..),
    getRewriteRule,
 )
import Log
import Prelude.Kore
import Pretty

data ErrorRewriteLoop = ErrorRewriteLoop
    { rule :: !(RewriteRule RewritingVariableName)
    , errorCallStack :: !CallStack
    }
    deriving stock (Show)

instance Exception ErrorRewriteLoop where
    toException = toException . SomeEntry []
    fromException exn =
        fromException exn >>= fromEntry

instance Pretty ErrorRewriteLoop where
    pretty ErrorRewriteLoop{rule, errorCallStack} =
        Pretty.vsep $
            [ "Found semantic rule with the same left- and right-hand side at:"
            , Pretty.pretty
                . sourceLocation
                . attributes
                . getRewriteRule
                $ rule
            , "Execution would not terminate when the rule applies."
            ]
                <> fmap Pretty.pretty (prettyCallStackLines errorCallStack)

instance Entry ErrorRewriteLoop where
    oneLineDoc ErrorRewriteLoop{rule} = pretty @SourceLocation $ from rule
    oneLineContextDoc _ = single CtxError
    entrySeverity _ = Error

errorRewriteLoop ::
    HasCallStack =>
    RewriteRule RewritingVariableName ->
    log a
errorRewriteLoop rule =
    throw ErrorRewriteLoop{rule, errorCallStack = callStack}
