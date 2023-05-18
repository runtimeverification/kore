{- |
Module      : Kore.Attribute.Injective
Description : Injective symbol attribute
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.Injective (
    Injective (..),
    injectiveId,
    injectiveSymbol,
    injectiveAttribute,
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @Injective@ represents the @injective@ attribute for symbols.
newtype Injective = Injective {isDeclaredInjective :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Semigroup Injective where
    (<>) (Injective a) (Injective b) = Injective (a || b)

instance Monoid Injective where
    mempty = Injective False

instance Default Injective where
    def = mempty

-- | Kore identifier representing the @injective@ attribute symbol.
injectiveId :: Id
injectiveId = "injective"

-- | Kore symbol representing the @injective@ attribute.
injectiveSymbol :: SymbolOrAlias
injectiveSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = injectiveId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @injective@ attribute.
injectiveAttribute :: AttributePattern
injectiveAttribute = attributePattern_ injectiveSymbol

instance ParseAttributes Injective where
    parseAttribute = parseBoolAttribute injectiveId

instance From Injective Attributes where
    from = toBoolAttributes injectiveAttribute
