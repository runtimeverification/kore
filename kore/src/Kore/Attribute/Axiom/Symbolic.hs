{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Axiom.Symbolic (
    Symbolic (..),
    isSymbolic,
    symbolicId,
    symbolicSymbol,
    symbolicAttribute,
    mapSymbolicVariables,
    parseSymbolicAttribute,

    -- * Re-exports
    FreeVariables,
) where

import Data.Binary (Binary)
import Data.Set (
    Set,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom.Concrete (
    parseFreeVariables,
 )
import Kore.Attribute.Parser as Parser
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
    isFreeVariable,
    mapFreeVariables,
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Debug
import Kore.Syntax.Variable
import Prelude.Kore

-- | @Symbolic@ represents the @symbolic@ attribute for axioms.
newtype Symbolic variable = Symbolic {unSymbolic :: FreeVariables variable}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving newtype (Semigroup, Monoid)

instance Default (Symbolic variable) where
    def = Symbolic FreeVariables.emptyFreeVariables

instance From (Symbolic variable) (Set (SomeVariable variable)) where
    from = from @(FreeVariables _) . unSymbolic
    {-# INLINE from #-}

instance From (Symbolic variable) (Set (SomeVariableName variable)) where
    from = from @(FreeVariables _) . unSymbolic
    {-# INLINE from #-}

-- | Kore identifier representing the @symbolic@ attribute symbol.
symbolicId :: Id
symbolicId = "symbolic"

-- | Kore symbol representing the @symbolic@ attribute.
symbolicSymbol :: SymbolOrAlias
symbolicSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = symbolicId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @symbolic@ attribute.
symbolicAttribute :: [SomeVariable VariableName] -> AttributePattern
symbolicAttribute = attributePattern symbolicSymbol . map attributeVariable

parseSymbolicAttribute ::
    FreeVariables VariableName ->
    AttributePattern ->
    Symbolic VariableName ->
    Parser (Symbolic VariableName)
parseSymbolicAttribute freeVariables =
    Parser.withApplication symbolicId parseApplication
  where
    parseApplication params args (Symbolic concreteVars) =
        Symbolic <$> parseFreeVariables freeVariables params args concreteVars

instance From (Symbolic VariableName) Attributes where
    from =
        from @AttributePattern @Attributes
            . symbolicAttribute
            . FreeVariables.toList
            . unSymbolic

mapSymbolicVariables ::
    Ord variable2 =>
    AdjSomeVariableName (variable1 -> variable2) ->
    Symbolic variable1 ->
    Symbolic variable2
mapSymbolicVariables adj (Symbolic freeVariables) =
    Symbolic (mapFreeVariables adj freeVariables)

isSymbolic ::
    Ord variable =>
    Symbolic variable ->
    SomeVariableName variable ->
    Bool
isSymbolic (Symbolic vars) = not . flip isFreeVariable vars
