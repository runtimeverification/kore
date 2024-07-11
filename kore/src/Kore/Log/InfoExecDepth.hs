{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.InfoExecDepth (
    InfoExecDepth (..),
    ExecDepth (..),
    infoExecDepth,
) where

import Data.Semigroup qualified as Semigroup
import Debug
import Log
import Numeric.Natural (
    Natural,
 )
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

newtype ExecDepth = ExecDepth {getExecDepth :: Natural}
    deriving stock (Eq, Ord, Show)
    deriving newtype (Enum)
    deriving (Semigroup) via (Semigroup.Max Natural)

instance Pretty ExecDepth where
    pretty execDepth =
        Pretty.hsep ["exec depth:", Pretty.pretty (getExecDepth execDepth)]

data InfoExecDepth = InfoExecDepth ExecDepth
    deriving stock (Eq, Show)

instance Debug InfoExecDepth where
    debugPrec w = \_ -> Pretty.pretty . show $ w

instance Diff InfoExecDepth where
    diffPrec = diffPrecEq

instance Pretty InfoExecDepth where
    pretty (InfoExecDepth execDepth) =
        Pretty.hsep ["execution complete:", Pretty.pretty execDepth]

instance Entry InfoExecDepth where
    entrySeverity _ = Info
    oneLineDoc (InfoExecDepth (ExecDepth execDepth)) = Pretty.pretty execDepth
    oneLineContextDoc _ = single CtxInfo
    helpDoc _ = "log depth of execution graph"

infoExecDepth :: MonadLog log => ExecDepth -> log ()
infoExecDepth = logEntry . InfoExecDepth
