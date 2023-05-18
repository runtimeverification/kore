{- |
Copyright   : (c) Runtime Verification, 2019-2023
License     : BSD-3-Clause
-}
module Kore.Attribute.Symbol.Macro (
    Macro (..),
    macroId,
    macroSymbol,
    macroAttribute,
) where

import Data.Binary (Binary)
import Data.Monoid (
    Any (..),
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @Macro@ represents the @macro@ attribute for symbols.
newtype Macro = Macro {isMacro :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving (Semigroup, Monoid) via Any

instance Default Macro where
    def = mempty

-- | Kore identifier representing the @symbolKywd@ attribute symbol.
macroId :: Id
macroId = "macro"

-- | Kore symbol representing the @macro@ attribute.
macroSymbol :: SymbolOrAlias
macroSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = macroId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @macro@ attribute.
macroAttribute :: AttributePattern
macroAttribute = attributePattern_ macroSymbol

instance ParseAttributes Macro where
    parseAttribute = parseBoolAttribute macroId

instance From Macro Attributes where
    from = toBoolAttributes macroAttribute
