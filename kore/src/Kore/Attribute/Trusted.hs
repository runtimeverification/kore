{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause

A trusted claim is a reachability logic verification claim
which can be used as a circularity without needing to be proven.
-}
module Kore.Attribute.Trusted (
    Trusted (..),
    trustedId,
    trustedSymbol,
    trustedAttribute,
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @Trusted@ represents the @trusted@ attribute for claim sentences.
newtype Trusted = Trusted {isTrusted :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Trusted where
    def = Trusted False

-- | Kore identifier representing the @trusted@ attribute symbol.
trustedId :: Id
trustedId = "trusted"

-- | Kore symbol representing the @trusted@ attribute.
trustedSymbol :: SymbolOrAlias
trustedSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = trustedId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @trusted@ attribute.
trustedAttribute :: AttributePattern
trustedAttribute = attributePattern_ trustedSymbol

instance ParseAttributes Trusted where
    parseAttribute = parseBoolAttribute trustedId

instance From Trusted Attributes where
    from = toBoolAttributes trustedAttribute
