{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Attributes (
    Attributes (..),
    ParsedPattern,
    AttributePattern,
    asAttributePattern,
    attributePattern,
    attributePattern_,
    attributeString,
    attributeInteger,
    attributeVariable,
) where

import Data.Binary (Binary)
import Data.Default (
    Default (..),
 )
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Null qualified as Attribute (
    Null,
 )
import Kore.Debug
import Kore.Syntax
import Kore.Unparser
import Prelude.Kore
import Pretty qualified
import SQL qualified

-- | A pure pattern which has only been parsed.
type ParsedPattern = Pattern VariableName Attribute.Null

type AttributePattern = ParsedPattern

asAttributePattern ::
    (PatternF VariableName) AttributePattern ->
    AttributePattern
asAttributePattern = asPattern . (mempty :<)

-- | An 'AttributePattern' of the attribute symbol applied to its arguments.
attributePattern ::
    -- | symbol
    SymbolOrAlias ->
    -- | arguments
    [AttributePattern] ->
    AttributePattern
attributePattern applicationSymbolOrAlias applicationChildren =
    (asAttributePattern . ApplicationF)
        Application{applicationSymbolOrAlias, applicationChildren}

-- | An 'AttributePattern' of the attribute symbol applied to no arguments.
attributePattern_ ::
    -- | symbol
    SymbolOrAlias ->
    AttributePattern
attributePattern_ applicationSymbolOrAlias =
    attributePattern applicationSymbolOrAlias []

attributeString :: Text -> AttributePattern
attributeString literal =
    (asAttributePattern . StringLiteralF . Const) (StringLiteral literal)

attributeInteger :: Integer -> AttributePattern
attributeInteger = attributeString . Text.pack . show

attributeVariable :: SomeVariable VariableName -> AttributePattern
attributeVariable v = (asAttributePattern . VariableF . Const) v

{- | 'Attributes' corresponds to the @attributes@ Kore syntactic declaration.
It is parameterized by the types of Patterns, @pat@.
-}
newtype Attributes = Attributes {getAttributes :: [AttributePattern]}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving newtype (Semigroup, Monoid)

instance Unparse Attributes where
    unparse = attributes . getAttributes
    unparse2 = attributes . getAttributes

instance Default Attributes where
    def = Attributes []

instance From Attribute.Null Attributes where
    from _ = Attributes []

instance From AttributePattern Attributes where
    from = Attributes . (: [])

instance From Attributes Attributes where
    from = id

instance SQL.Column Attributes where
    defineColumn = SQL.defineTextColumn

    -- TODO (thomas.tuegel): Use a better toColumn for lists.
    toColumn =
        SQL.toColumn
            . Pretty.renderText
            . Pretty.layoutOneLine
            . Pretty.hsep
            . Pretty.punctuate (Pretty.comma <> Pretty.space)
            . map unparse
            . getAttributes
