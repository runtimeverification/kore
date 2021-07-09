{- |
Copyright   : (c) Runtime Verification, 2020
License     : BSD-3-Clause
-}
module Kore.Log.InfoExecBreadth (
    InfoExecBreadth,
    ExecBreadth (..),
    infoExecBreadth,
) where

import Log
import Numeric.Natural
import Prelude.Kore
import Pretty (
    Pretty,
 )
import qualified Pretty

newtype ExecBreadth = ExecBreadth {getExecBreadth :: Natural}
    deriving stock (Show)

instance Pretty ExecBreadth where
    pretty = Pretty.pretty . getExecBreadth

newtype InfoExecBreadth = InfoExecBreadth {breadth :: ExecBreadth}
    deriving stock (Show)

instance Pretty InfoExecBreadth where
    pretty (InfoExecBreadth breadth) =
        Pretty.hsep
            [ "number of concurrent branches:"
            , Pretty.pretty breadth
            ]

instance Entry InfoExecBreadth where
    entrySeverity _ = Info
    helpDoc _ = "log number of concurrent branches"

infoExecBreadth :: MonadLog log => ExecBreadth -> log ()
infoExecBreadth = logEntry . InfoExecBreadth
