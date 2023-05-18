{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Owise (
    Owise (..),
    owiseId,
    owiseSymbol,
    owiseAttribute,
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

newtype Owise = Owise {isOwise :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving anyclass (Hashable)

instance Default Owise where
    def = Owise False

owiseId :: Id
owiseId = "owise"

owiseSymbol :: SymbolOrAlias
owiseSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = owiseId
        , symbolOrAliasParams = []
        }

owiseAttribute :: AttributePattern
owiseAttribute = attributePattern_ owiseSymbol

instance ParseAttributes Owise where
    parseAttribute = parseBoolAttribute owiseId

instance From Owise Attributes where
    from = toBoolAttributes owiseAttribute
