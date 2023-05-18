{- |
Module      : Kore.Attribute.Idem
Description : Idempotency axiom attribute
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.Idem (
    Idem (..),
    idemId,
    idemSymbol,
    idemAttribute,
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @Idem@ represents the @idem@ attribute for axioms.
newtype Idem = Idem {isIdem :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Idem where
    def = Idem False

-- | Kore identifier representing the @idem@ attribute symbol.
idemId :: Id
idemId = "idem"

-- | Kore symbol representing the @idem@ attribute.
idemSymbol :: SymbolOrAlias
idemSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = idemId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @idem@ attribute.
idemAttribute :: AttributePattern
idemAttribute = attributePattern_ idemSymbol

instance ParseAttributes Idem where
    parseAttribute = parseBoolAttribute idemId

instance From Idem Attributes where
    from = toBoolAttributes idemAttribute
