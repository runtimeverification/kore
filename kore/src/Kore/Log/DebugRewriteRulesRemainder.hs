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

import Data.Aeson (Value (Array), object, toJSON, (.=))
import Data.Text qualified as Text
import Data.Vector (fromList)
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

data DebugRewriteRulesRemainder = DebugRewriteRulesRemainder
    { configuration :: !(Pattern VariableName)
    , rulesCount :: !Int
    , remainder :: !(Predicate RewritingVariableName)
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

    oneLineContextJson
        entry@DebugRewriteRulesRemainder{configuration, rulesCount} =
            Array $
                fromList
                    [ object ["entry" .= entryTypeText (toEntry entry)]
                    , object
                        [ "term" .= showHashHex (hash configuration)
                        ]
                    , object
                        [ "rules-count" .= Text.pack (show rulesCount)
                        ]
                    ]

    oneLineDoc entry@(DebugRewriteRulesRemainder{rulesCount, remainder}) =
        let context = map Pretty.brackets (pretty <$> oneLineContextDoc entry <> ["detail"])
            logMsg =
                ( Pretty.hsep . concat $
                    [ ["After applying ", pretty rulesCount, " rewrite rules"]
                    , ["there is a remainder condition: ", Pretty.group . pretty $ remainder]
                    ]
                )
         in mconcat context <> logMsg

    oneLineJson DebugRewriteRulesRemainder{remainder} =
        toJSON
            . PatternJson.fromPredicate sortBool
            . Predicate.mapVariables (pure toVariableName)
            $ remainder

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
