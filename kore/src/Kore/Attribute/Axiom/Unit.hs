{- |
Module      : Kore.Attribute.Axiom.Unit
Description : Unit axiom attribute
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.Axiom.Unit (
    Unit (..),
    unitId,
    unitSymbol,
    unitAttribute,
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @Unit@ represents the @unit@ attribute for axioms.
newtype Unit = Unit {isUnit :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Unit where
    def = Unit False

-- | Kore identifier representing the @unit@ attribute symbol.
unitId :: Id
unitId = "unit"

-- | Kore symbol representing the @unit@ attribute.
unitSymbol :: SymbolOrAlias
unitSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = unitId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @unit@ attribute.
unitAttribute :: AttributePattern
unitAttribute = attributePattern_ unitSymbol

instance ParseAttributes Unit where
    parseAttribute = parseBoolAttribute unitId

instance From Unit Attributes where
    from = toBoolAttributes unitAttribute
