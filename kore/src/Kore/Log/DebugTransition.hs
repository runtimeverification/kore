{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugTransition (
    DebugTransition (..),
    debugBeforeTransition,
    debugAfterTransition,
    debugFinalTransition,
) where

import Kore.Attribute.SourceLocation (
    SourceLocation,
 )
import Kore.Reachability.ClaimState (
    ClaimState,
 )
import Kore.Reachability.Prim (
    Prim (..),
 )
import Kore.Reachability.SomeClaim (
    SomeClaim (..),
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

data DebugTransition
    = DebugAfterTransition AfterTransition
    | DebugBeforeTransition BeforeTransition
    deriving stock (Show)

data AfterTransition = AfterTransition
    { resultState :: Maybe (ClaimState SomeClaim)
    , transition :: Prim
    , appliedRules :: [SourceLocation]
    }
    deriving stock (Show)

data BeforeTransition = BeforeTransition
    { proofState :: ClaimState SomeClaim
    , transition :: Prim
    }
    deriving stock (Show)

instance Pretty DebugTransition where
    pretty (DebugAfterTransition afterTransition) =
        pretty afterTransition
    pretty (DebugBeforeTransition beforeTransition) =
        pretty beforeTransition

instance Pretty AfterTransition where
    pretty AfterTransition{resultState, transition, appliedRules} =
        Pretty.vsep $
            concat
                [
                    [ "Applied the following transition:"
                    , Pretty.indent 4 (pretty transition)
                    ]
                , prettyRules
                ,
                    [ "Resulting in:"
                    , Pretty.indent 4 (maybe "Terminal state." pretty resultState)
                    ]
                ]
      where
        shouldDisplayRules = case transition of
            ApplyAxioms -> True
            ApplyClaims -> True
            _ -> False
        prettyRules
            -- match behavior of DebugAppliedRewriteRules
            | shouldDisplayRules = case appliedRules of
                [] -> ["No rules were applied."]
                rules ->
                    [ "The rules at following locations were applied:"
                    , Pretty.indent 4 $ Pretty.vsep $ fmap pretty rules
                    ]
            | otherwise = []

instance Pretty BeforeTransition where
    pretty BeforeTransition{proofState, transition} =
        Pretty.vsep
            [ "Reached proof state with the following configuration:"
            , Pretty.indent 4 (pretty proofState)
            , "To which the following transition will be applied:"
            , Pretty.indent 4 (pretty transition)
            ]

instance Entry DebugTransition where
    entrySeverity _ = Debug
    helpDoc _ = "log proof state"
    oneLineDoc
        (DebugAfterTransition AfterTransition{transition, appliedRules}) =
            transitionHeader
                <> Pretty.hsep (pretty <$> appliedRules)
          where
            transitionHeader =
                "after  "
                    <> case appliedRules of
                        [] -> pretty transition
                        _otherwise -> pretty transition <> ": "
    oneLineDoc
        (DebugBeforeTransition BeforeTransition{transition}) =
            "before " <> pretty transition
    oneLineContextDoc _ = single CtxDetail

debugBeforeTransition ::
    MonadLog log =>
    ClaimState SomeClaim ->
    Prim ->
    log ()
debugBeforeTransition proofState transition =
    logEntry $ DebugBeforeTransition before
  where
    before = BeforeTransition{proofState, transition}

debugAfterTransition ::
    MonadLog log =>
    From rule SourceLocation =>
    ClaimState SomeClaim ->
    Prim ->
    [rule] ->
    log ()
debugAfterTransition resultState transition rules =
    logEntry $ DebugAfterTransition after
  where
    appliedRules = map from rules
    after =
        AfterTransition
            { resultState = Just resultState
            , transition
            , appliedRules
            }

debugFinalTransition ::
    MonadLog log =>
    Prim ->
    log ()
debugFinalTransition transition =
    logEntry $ DebugAfterTransition after
  where
    after =
        AfterTransition
            { resultState = Nothing
            , transition
            , appliedRules = []
            }
