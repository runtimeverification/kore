{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Axiom.NonExecutable (
    NonExecutable (..),
    nonExecutableId,
    nonExecutableSymbol,
    nonExecutableAttribute,
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @NonExecutable@ represents the @non-executable@ attribute for axioms.
newtype NonExecutable = NonExecutable {isNonExecutable :: Bool}
    deriving stock (Eq, GHC.Generic, Ord, Show)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving anyclass (NFData)
    deriving anyclass (Hashable)

instance Default NonExecutable where
    def = NonExecutable False

-- | Kore identifier representing the @non-executable@ attribute symbol.
nonExecutableId :: Id
nonExecutableId = "non-executable"

-- | Kore symbol representing the @non-executable@ attribute.
nonExecutableSymbol :: SymbolOrAlias
nonExecutableSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = nonExecutableId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @non-executable@ attribute.
nonExecutableAttribute :: AttributePattern
nonExecutableAttribute = attributePattern_ nonExecutableSymbol

instance ParseAttributes NonExecutable where
    parseAttribute = parseBoolAttribute nonExecutableId

instance From NonExecutable Attributes where
    from = toBoolAttributes nonExecutableAttribute
