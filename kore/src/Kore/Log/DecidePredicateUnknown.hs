{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DecidePredicateUnknown (
    OnDecidePredicateUnknown (..),
    DecidePredicateUnknown (..),
    throwDecidePredicateUnknown,
    liftLoc,
    srcLoc,
    Loc,
    externaliseDecidePredicateUnknown,
) where

import Control.Exception (
    Exception (..),
    throw,
 )
import Debug
import Kore.Attribute.SourceLocation (
    SourceLocation (..),
 )
import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.TermLike qualified as TermLike
import Kore.Internal.Variable
import Kore.Syntax.Json qualified as PatternJson
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q, qLocation)
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified
import SMT qualified

data OnDecidePredicateUnknown
    = WarnDecidePredicateUnknown Loc (Maybe SourceLocation)
    | ErrorDecidePredicateUnknown Loc (Maybe SourceLocation)
    deriving stock (Show, Eq)

liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) =
    [|Loc a b c (d1, d2) (e1, e2)|]

srcLoc :: Q Exp
srcLoc = qLocation >>= liftLoc

data DecidePredicateUnknown = DecidePredicateUnknown
    { action :: OnDecidePredicateUnknown
    , smtLimit :: SMT.RetryLimit
    , predicates :: NonEmpty (Predicate VariableName)
    }
    deriving stock (Show, Eq)

instance Exception DecidePredicateUnknown where
    toException = toException . SomeEntry []
    fromException exn =
        fromException exn >>= fromEntry

instance Debug DecidePredicateUnknown where
    debugPrec w = \_ -> Pretty.pretty . show $ w

instance Diff DecidePredicateUnknown where
    diffPrec = diffPrecEq

instance Pretty DecidePredicateUnknown where
    pretty DecidePredicateUnknown{} =
        Pretty.vsep ["Failed to decide predicate."]

instance Entry DecidePredicateUnknown where
    entrySeverity DecidePredicateUnknown{action} =
        case action of
            WarnDecidePredicateUnknown _ _ -> Warning
            _ -> Error
    contextDoc DecidePredicateUnknown{action} =
        Just $
            Pretty.align $
                Pretty.vsep
                    [ Pretty.hsep . catMaybes $
                        [ Just "while applying equation"
                        , (\loc -> Pretty.hsep ["defined at", pretty loc]) <$> case action of
                            WarnDecidePredicateUnknown _ koreLoc -> koreLoc
                            ErrorDecidePredicateUnknown _ koreLoc -> koreLoc
                        ]
                    , Pretty.hsep
                        [ "in"
                        , case action of
                            ErrorDecidePredicateUnknown hsLoc _ -> prettyHsLoc hsLoc
                            WarnDecidePredicateUnknown hsLoc _ -> prettyHsLoc hsLoc
                        ]
                    ]
      where
        prettyHsLoc Loc{loc_module, loc_start = (row, col)} =
            Pretty.pretty loc_module <> ":" <> Pretty.pretty row <> ":" <> Pretty.pretty col
    oneLineDoc _ = "DecidePredicateUnknown"
    helpDoc _ =
        "error or a warning when the solver cannot decide the satisfiability of a formula"

throwDecidePredicateUnknown ::
    (MonadLog log, InternalVariable variable) =>
    OnDecidePredicateUnknown ->
    SMT.RetryLimit ->
    NonEmpty (Predicate variable) ->
    log ()
throwDecidePredicateUnknown action smtLimit predicates' =
    case action of
        WarnDecidePredicateUnknown _ _ ->
            logEntry DecidePredicateUnknown{action, smtLimit, predicates}
        ErrorDecidePredicateUnknown _ _ ->
            throw DecidePredicateUnknown{action, smtLimit, predicates}
  where
    predicates = Predicate.mapVariables (pure toVariableName) <$> predicates'

externaliseDecidePredicateUnknown :: DecidePredicateUnknown -> PatternJson.KoreJson
externaliseDecidePredicateUnknown err =
    PatternJson.fromPredicate
        (TermLike.SortActualSort $ TermLike.SortActual (TermLike.Id "SortBool" TermLike.AstLocationNone) [])
        (Predicate.makeMultipleAndPredicate . toList $ predicates err)
