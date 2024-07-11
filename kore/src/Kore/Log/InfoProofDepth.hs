{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.InfoProofDepth (
    InfoProofDepth (..),
    infoUnprovenDepth,
    infoProvenDepth,
    ProofDepth (..),
) where

import Data.Semigroup qualified as Semigroup
import Log
import Numeric.Natural (
    Natural,
 )
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

newtype ProofDepth = ProofDepth {getProofDepth :: Natural}
    deriving stock (Eq, Ord, Show)
    deriving newtype (Enum)
    deriving (Semigroup) via (Semigroup.Max Natural)

instance Pretty ProofDepth where
    pretty proofDepth =
        Pretty.hsep ["proof depth:", Pretty.pretty (getProofDepth proofDepth)]

data InfoProofDepth
    = InfoUnprovenDepth ProofDepth
    | InfoProvenDepth ProofDepth
    deriving stock (Show)

instance Pretty InfoProofDepth where
    pretty (InfoUnprovenDepth depth) =
        Pretty.hsep ["proof incomplete:", Pretty.pretty depth]
    pretty (InfoProvenDepth depth) =
        Pretty.hsep ["proof complete:", Pretty.pretty depth]

instance Entry InfoProofDepth where
    entrySeverity _ = Info
    oneLineDoc (InfoUnprovenDepth (ProofDepth depth)) =
        Pretty.hsep
            [ "InfoUnprovenDepth"
            , Pretty.colon
            , Pretty.pretty depth
            ]
    oneLineDoc (InfoProvenDepth (ProofDepth depth)) =
        Pretty.hsep
            [ "InfoProvenDepth"
            , Pretty.colon
            , Pretty.pretty depth
            ]

    oneLineContextDoc _ = single CtxInfo
    helpDoc _ = "log depth of proof graph"

infoUnprovenDepth :: MonadLog log => ProofDepth -> log ()
infoUnprovenDepth = logEntry . InfoUnprovenDepth

infoProvenDepth :: MonadLog log => ProofDepth -> log ()
infoProvenDepth = logEntry . InfoProvenDepth
