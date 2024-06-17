{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugRewriteRulesRemainder (
    DebugRewriteRulesRemainder (..),
    debugRewriteRulesRemainder,
) where

import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Predicate (
    Predicate,
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

data DebugRewriteRulesRemainder = DebugRewriteRulesRemainder
    { configuration :: !(Pattern VariableName)
    , rules :: !Int
    , remainder :: !(Predicate RewritingVariableName)
    }
    deriving stock (Show)

instance Pretty DebugRewriteRulesRemainder where
    pretty DebugRewriteRulesRemainder{..} =
        Pretty.vsep
            [ (Pretty.hsep . catMaybes)
                [ Just "The rules"
                , -- TODO add rules
                  Just "produced a remainder"
                , Just . pretty $ remainder
                ]
            , "On configuration:"
            , Pretty.indent 2 . unparse $ configuration
            ]

instance Entry DebugRewriteRulesRemainder where
    entrySeverity _ = Debug
    helpDoc _ = "log rewrite rules remainder"

    -- oneLineContextJson
    --     DebugRewriteRulesRemainder{configuration, ruleId} =
    --         Array $
    --             fromList
    --                 [ object
    --                     [ "term" .= showHashHex (hash configuration)
    --                     ]
    --                 , object
    --                     [ "rewrite" .= shortRuleIdTxt ruleId
    --                     ]
    --                 ]

    oneLineDoc entry@(DebugRewriteRulesRemainder{rules, remainder}) =
        let context = map Pretty.brackets (pretty <$> oneLineContextDoc entry <> ["detail"])
            logMsg =
                ( Pretty.hsep . concat $
                    [ ["After applying ", pretty rules, " rewrite rules"]
                    , ["there is a remainder condition: ", Pretty.group . pretty $ remainder]
                    ]
                )
         in mconcat context <> logMsg

-- oneLineJson DebugRewriteRulesRemainder{label, attemptedRewriteRule} =
--     toJSON $ renderDefault $ maybe (Pretty.pretty attemptedRewriteRule) Pretty.pretty label

-- whileDebugAttemptRewriteRule ::
--     MonadLog log =>
--     Pattern RewritingVariableName ->
--     UniqueId ->
--     Maybe Text ->
--     SourceLocation ->
--     log a ->
--     log a
-- whileDebugAttemptRewriteRule initial ruleId label attemptedRewriteRule =
--     logWhile (DebugAttemptedRewriteRules{..})
--   where
--     configuration = mapConditionalVariables TermLike.mapVariables initial
--     mapConditionalVariables mapTermVariables =
--         Conditional.mapVariables mapTermVariables (pure toVariableName)

debugRewriteRulesRemainder ::
    MonadLog log =>
    Pattern RewritingVariableName ->
    Int ->
    Predicate RewritingVariableName ->
    log ()
debugRewriteRulesRemainder pat rules remainder =
    logEntry DebugRewriteRulesRemainder{..}
  where
    configuration = mapConditionalVariables TermLike.mapVariables pat
    mapConditionalVariables mapTermVariables =
        Conditional.mapVariables mapTermVariables (pure toVariableName)
