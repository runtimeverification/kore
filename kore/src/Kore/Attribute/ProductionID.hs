{- |
Module      : Kore.Attribute.ProductionID
Description : Production ID attribute
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.ProductionID (
    ProductionID (..),
    productionIDId,
    productionIDSymbol,
    productionIDAttribute,
) where

import Data.Binary (Binary)
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @ProductionID@ represents the @productionID@ attribute.
newtype ProductionID = ProductionID {getProductionID :: Maybe Text}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default ProductionID where
    def = ProductionID Nothing

-- | Kore identifier representing the @productionID@ attribute symbol.
productionIDId :: Id
productionIDId = "productionID"

-- | Kore symbol representing the @productionID@ attribute.
productionIDSymbol :: SymbolOrAlias
productionIDSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = productionIDId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing a @productionID@ attribute.
productionIDAttribute :: Text -> AttributePattern
productionIDAttribute name =
    attributePattern productionIDSymbol [attributeString name]

instance ParseAttributes ProductionID where
    parseAttribute =
        withApplication' $ \params args (ProductionID productionID) -> do
            Parser.getZeroParams params
            arg <- Parser.getOneArgument args
            StringLiteral name <- Parser.getStringLiteral arg
            unless (isNothing productionID) failDuplicate'
            return ProductionID{getProductionID = Just name}
      where
        withApplication' = Parser.withApplication productionIDId
        failDuplicate' = Parser.failDuplicate productionIDId

instance From ProductionID Attributes where
    from =
        maybe def toAttribute . getProductionID
      where
        toAttribute = from @AttributePattern . productionIDAttribute
