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

import Data.Aeson (object, (.=))
import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.TermLike qualified as TermLike
import Kore.Internal.Variable (
    VariableName,
    toVariableName,
 )
import Kore.Rewrite.RewritingVariable
import Kore.Syntax.Json qualified as PatternJson
import Kore.Unparser
import Kore.Util (showHashHex)
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

{- This log entry will be emitted if, after unifying with rewrite rules,
   there is a satisfiable remainder condition  -}
data DebugRewriteRulesRemainder = DebugRewriteRulesRemainder
    { configuration :: !(Pattern VariableName)
    -- ^ the state the rules unified with
    , rulesCount :: !Int
    -- ^ how many rules were unified
    , remainder :: !(Predicate RewritingVariableName)
    -- ^ the condition not covered by the rules
    }
    deriving stock (Show)

instance Pretty DebugRewriteRulesRemainder where
    pretty DebugRewriteRulesRemainder{..} =
        Pretty.vsep
            [ (Pretty.hsep . catMaybes)
                [ Just "The rules"
                , Just "produced a remainder"
                , Just . pretty $ remainder
                ]
            , "On configuration:"
            , Pretty.indent 2 . unparse $ configuration
            ]

instance Entry DebugRewriteRulesRemainder where
    entrySeverity _ = Debug
    helpDoc _ = "log rewrite rules remainder"

    oneLineContextDoc
        DebugRewriteRulesRemainder{configuration} =
            [CLNullary CtxRemainder, CtxTerm `withShortId` showHashHex (hash configuration)]

    oneLineDoc (DebugRewriteRulesRemainder{rulesCount, remainder}) =
        let context = [Pretty.brackets "detail"]
            logMsg =
                ( Pretty.hsep
                    [ "After applying "
                    , pretty rulesCount
                    , " rewrite rules"
                    , "there is a satisfiable remainder condition: "
                    , Pretty.group . pretty $ remainder
                    ]
                )
         in mconcat context <> logMsg

    oneLineJson DebugRewriteRulesRemainder{remainder, rulesCount} =
        object
            [ "remainder"
                .= PatternJson.fromPredicate
                    sortBool
                    (Predicate.mapVariables (pure toVariableName) remainder)
            , "rules-count" .= rulesCount
            ]

sortBool :: TermLike.Sort
sortBool =
    (TermLike.SortActualSort $ TermLike.SortActual (TermLike.Id "SortBool" TermLike.AstLocationNone) [])

debugRewriteRulesRemainder ::
    MonadLog log =>
    Pattern RewritingVariableName ->
    Int ->
    Predicate RewritingVariableName ->
    log ()
debugRewriteRulesRemainder pat rulesCount remainder =
    logEntry DebugRewriteRulesRemainder{..}
  where
    configuration = mapConditionalVariables TermLike.mapVariables pat
    mapConditionalVariables mapTermVariables =
        Conditional.mapVariables mapTermVariables (pure toVariableName)
