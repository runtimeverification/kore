{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.InfoReachability (
    InfoReachability (..),
    whileReachability,
) where

import Kore.Reachability.Prim
import Log qualified
import Prelude.Kore
import Pretty (
    Doc,
    Pretty,
    (<+>),
 )
import Pretty qualified

newtype InfoReachability = InfoReachability {prim :: Prim}
    deriving stock (Show)

instance Pretty InfoReachability where
    pretty InfoReachability{prim} =
        (Pretty.hsep . catMaybes) [primDoc prim]

instance Log.Entry InfoReachability where
    entrySeverity _ = Log.Info
    contextDoc InfoReachability{prim} = (<+>) "while" <$> primDoc prim
    oneLineDoc InfoReachability{prim} = Pretty.pretty . show $ prim
    oneLineContextDoc _ = Log.single Log.CtxInfo
    helpDoc _ = "log reachability proof steps"

primDoc :: Prim -> Maybe (Doc ann)
primDoc Simplify = Just "simplifying the claim"
primDoc CheckImplication = Just "checking the implication"
primDoc ApplyClaims = Just "applying claims"
primDoc ApplyAxioms = Just "applying axioms"
primDoc _ = Nothing

whileReachability ::
    Log.MonadLog log =>
    Prim ->
    log a ->
    log a
whileReachability prim log
    | Just _ <- primDoc prim = Log.logWhile InfoReachability{prim} log
    | otherwise = log
