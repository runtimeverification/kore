{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Log.WarnUnsimplified (
    WarnUnsimplifiedPredicate (..),
    warnUnsimplifiedPredicate,
    WarnUnsimplifiedCondition (..),
    warnUnsimplifiedCondition,
) where

import Kore.Internal.Condition (
    Condition,
 )
import Kore.Internal.Conditional (
    Conditional,
 )
import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import Kore.Internal.MultiOr (
    MultiOr,
 )
import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Rewrite.RewritingVariable
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

data WarnUnsimplifiedPredicate = WarnUnsimplifiedPredicate
    { limit :: Int
    , original :: (Predicate RewritingVariableName)
    , output :: (MultiOr (MultiAnd (Predicate RewritingVariableName)))
    }
    deriving stock (Show)

data WarnUnsimplifiedCondition = WarnUnsimplifiedCondition
    { limit :: Int
    , original :: (Condition RewritingVariableName)
    , output :: (Condition RewritingVariableName)
    }
    deriving stock (Show)

instance Pretty WarnUnsimplifiedPredicate where
    pretty WarnUnsimplifiedPredicate{original, output, limit} =
        Pretty.vsep
            [ Pretty.hsep
                [ "Predicate not simplified after"
                , Pretty.pretty limit
                , "rounds."
                ]
            , "Original predicate:"
            , (Pretty.indent 4) (Pretty.pretty original)
            , Pretty.hsep
                [ "Output after"
                , Pretty.pretty limit
                , "rounds:"
                ]
            , (Pretty.indent 4) (Pretty.pretty output)
            ]

instance Pretty WarnUnsimplifiedCondition where
    pretty WarnUnsimplifiedCondition{original, output, limit} =
        Pretty.vsep
            [ Pretty.hsep
                [ "Condition not simplified after"
                , Pretty.pretty limit
                , "rounds."
                ]
            , "Original condition:"
            , (Pretty.indent 4) (Pretty.pretty original)
            , Pretty.hsep
                [ "Output after"
                , Pretty.pretty limit
                , "rounds:"
                ]
            , (Pretty.indent 4) (Pretty.pretty output)
            ]

instance Entry WarnUnsimplifiedPredicate where
    entrySeverity _ = Debug
    oneLineDoc WarnUnsimplifiedPredicate{limit} = Pretty.pretty limit
    oneLineContextDoc _ = single CtxDetail
    helpDoc _ = "warn when a predicate is not simplified"

instance Entry WarnUnsimplifiedCondition where
    entrySeverity _ = Debug
    oneLineDoc WarnUnsimplifiedCondition{limit} = Pretty.pretty limit
    oneLineContextDoc _ = single CtxDetail
    helpDoc _ = "warn when a condition is not simplified"

warnUnsimplifiedPredicate ::
    MonadLog log =>
    Int ->
    Predicate RewritingVariableName ->
    MultiOr (MultiAnd (Predicate RewritingVariableName)) ->
    log ()
warnUnsimplifiedPredicate limit original output =
    logEntry WarnUnsimplifiedPredicate{limit, original, output}

warnUnsimplifiedCondition ::
    MonadLog log =>
    Int ->
    Conditional RewritingVariableName any ->
    Conditional RewritingVariableName any ->
    log ()
warnUnsimplifiedCondition
    limit
    (Conditional.withoutTerm -> original)
    (Conditional.withoutTerm -> output) =
        logEntry WarnUnsimplifiedCondition{limit, original, output}
