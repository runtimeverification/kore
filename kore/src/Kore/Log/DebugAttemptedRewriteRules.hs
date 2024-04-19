{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugAttemptedRewriteRules (
    DebugAttemptedRewriteRules (..),
    debugAttemptedRewriteRule,
) where

import Data.Aeson qualified as JSON
import Data.Text (Text)
import Kore.Attribute.Axiom (
    SourceLocation,
 )
import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Internal.Variable (
    VariableName,
    toVariableName,
 )
import Kore.Rewrite.RewritingVariable
import Kore.Unparser
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

data DebugAttemptedRewriteRules = DebugAttemptedRewriteRules
    { configuration :: !(Pattern VariableName)
    , label :: !(Maybe Text)
    , attemptedRewriteRule :: !SourceLocation
    }
    deriving stock (Show)

instance Pretty DebugAttemptedRewriteRules where
    pretty DebugAttemptedRewriteRules{..} =
        Pretty.vsep
            [ (Pretty.hsep . catMaybes)
                [ Just "The rule"
                , (\l -> Pretty.hsep ["with label:", "[", pretty l, "]"]) <$> label
                , Just "at the following location was attempted:"
                , Just . pretty $ attemptedRewriteRule
                ]
            , "On configuration:"
            , Pretty.indent 2 . unparse $ configuration
            ]

instance Entry DebugAttemptedRewriteRules where
    entrySeverity _ = Debug
    helpDoc _ = "log attempted rewrite rules"
    oneLineJson entry =
        JSON.object ["entry" JSON..= Log.entryTypeText (Log.toEntry entry)]
    oneLineDoc DebugAttemptedRewriteRules{attemptedRewriteRule} =
        pretty attemptedRewriteRule

debugAttemptedRewriteRule ::
    MonadLog log =>
    Pattern RewritingVariableName ->
    Maybe Text ->
    SourceLocation ->
    log ()
debugAttemptedRewriteRule initial label attemptedRewriteRule =
    logEntry DebugAttemptedRewriteRules{..}
  where
    configuration = mapConditionalVariables TermLike.mapVariables initial
    mapConditionalVariables mapTermVariables =
        Conditional.mapVariables mapTermVariables (pure toVariableName)
