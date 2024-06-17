{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugAppliedRewriteRules (
    DebugAppliedRewriteRules (..),
    debugAppliedRewriteRules,
    DebugAppliedLabeledRewriteRule (..),
    debugAppliedLabeledRewriteRule,
) where

import Data.Aeson (toJSON)
import Data.Aeson qualified as Json
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

data DebugAppliedRewriteRules = DebugAppliedRewriteRules
    { configuration :: !(Pattern VariableName)
    , appliedRewriteRules :: ![SourceLocation]
    }
    deriving stock (Show)

instance Pretty DebugAppliedRewriteRules where
    pretty DebugAppliedRewriteRules{configuration, appliedRewriteRules} =
        Pretty.vsep $
            (<>)
                prettyUnifiedRules
                [ "On configuration:"
                , Pretty.indent 2 . unparse $ configuration
                ]
      where
        prettyUnifiedRules =
            case appliedRewriteRules of
                [] -> ["No rules were applied."]
                rules ->
                    ["The rules at following locations were applied:"]
                        <> fmap pretty rules

instance Entry DebugAppliedRewriteRules where
    entrySeverity _ = Debug
    helpDoc _ = "log applied rewrite rules"

    isEmpty DebugAppliedRewriteRules{appliedRewriteRules}
        | null appliedRewriteRules = True
        | otherwise = False
    oneLineDoc DebugAppliedRewriteRules{appliedRewriteRules}
        | null appliedRewriteRules =
            "failed to apply " <> pretty (length appliedRewriteRules) <> " rewrite rules"
        | otherwise =
            "applied " <> pretty (length appliedRewriteRules) <> " rewrite rules"
    oneLineJson DebugAppliedRewriteRules{appliedRewriteRules}
        | null appliedRewriteRules = Json.Null
        | otherwise =
            Json.toJSON $
                "applied " <> show (length appliedRewriteRules) <> " rewrite rules"
    oneLineContextDoc DebugAppliedRewriteRules{appliedRewriteRules}
        | null appliedRewriteRules =
            ["failure"]
        | otherwise =
            ["success"]

debugAppliedRewriteRules ::
    MonadLog log =>
    Pattern RewritingVariableName ->
    [SourceLocation] ->
    log ()
debugAppliedRewriteRules initial appliedRewriteRules =
    logEntry
        DebugAppliedRewriteRules
            { configuration
            , appliedRewriteRules
            }
  where
    configuration = mapConditionalVariables TermLike.mapVariables initial
    mapConditionalVariables mapTermVariables =
        Conditional.mapVariables mapTermVariables (pure toVariableName)

data DebugAppliedLabeledRewriteRule = DebugAppliedLabeledRewriteRule
    { configuration :: !(Pattern VariableName)
    , label :: !(Maybe Text)
    , sourceLocation :: !SourceLocation
    }
    deriving stock (Show)

instance Pretty DebugAppliedLabeledRewriteRule where
    pretty DebugAppliedLabeledRewriteRule{..} =
        Pretty.vsep
            [ Pretty.hsep
                [ "The rule with label"
                , "["
                , pretty $ fromMaybe "" label
                , "]"
                , "at location:"
                , pretty sourceLocation
                , "was applied"
                ]
            , "on configuration:"
            , Pretty.indent 2 . unparse $ configuration
            ]

instance Entry DebugAppliedLabeledRewriteRule where
    entrySeverity _ = Debug
    helpDoc _ = "log applied rewrite rule with label"
    oneLineDoc DebugAppliedLabeledRewriteRule{label, sourceLocation} =
        maybe (pretty sourceLocation) pretty label
    oneLineJson = toJSON . renderDefault . oneLineDoc
    oneLineContextDoc DebugAppliedLabeledRewriteRule{} = ["success"]
    oneLineContextJson DebugAppliedLabeledRewriteRule{} = toJSON ("success" :: Text)

debugAppliedLabeledRewriteRule ::
    MonadLog log =>
    Pattern RewritingVariableName ->
    Maybe Text ->
    SourceLocation ->
    log ()
debugAppliedLabeledRewriteRule initial label sourceLocation =
    logEntry DebugAppliedLabeledRewriteRule{..}
  where
    configuration = mapConditionalVariables TermLike.mapVariables initial
    mapConditionalVariables mapTermVariables =
        Conditional.mapVariables mapTermVariables (pure toVariableName)
