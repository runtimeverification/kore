{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Axiom.Concrete (
    Concrete (..),
    isConcrete,
    concreteId,
    concreteSymbol,
    concreteAttribute,
    mapConcreteVariables,
    parseConcreteAttribute,
    parseFreeVariables, -- used by Symbolic

    -- * Re-exports
    FreeVariables,
) where

import Control.Error qualified as Safe
import Control.Monad qualified as Monad
import Data.Binary (Binary)
import Data.List qualified as List
import Data.Set (
    Set,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
    freeVariable,
    isFreeVariable,
    mapFreeVariables,
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Debug
import Kore.Error qualified
import Kore.Syntax.Variable hiding (
    Concrete,
 )
import Prelude.Kore

-- | @Concrete@ represents the @concrete@ attribute for axioms.
newtype Concrete variable = Concrete {unConcrete :: FreeVariables variable}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving newtype (Semigroup, Monoid)

instance Default (Concrete variable) where
    def = Concrete FreeVariables.emptyFreeVariables

instance From (Concrete variable) (Set (SomeVariable variable)) where
    from = from @(FreeVariables _) . unConcrete
    {-# INLINE from #-}

instance From (Concrete variable) (Set (SomeVariableName variable)) where
    from = from @(FreeVariables _) . unConcrete
    {-# INLINE from #-}

-- | Kore identifier representing the @concrete@ attribute symbol.
concreteId :: Id
concreteId = "concrete"

-- | Kore symbol representing the @concrete@ attribute.
concreteSymbol :: SymbolOrAlias
concreteSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = concreteId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @concrete@ attribute.
concreteAttribute :: [SomeVariable VariableName] -> AttributePattern
concreteAttribute = attributePattern concreteSymbol . map attributeVariable

parseConcreteAttribute ::
    FreeVariables VariableName ->
    AttributePattern ->
    Concrete VariableName ->
    Parser (Concrete VariableName)
parseConcreteAttribute freeVariables =
    Parser.withApplication concreteId parseApplication
  where
    parseApplication params args (Concrete concreteVars) =
        Concrete <$> parseFreeVariables freeVariables params args concreteVars

parseFreeVariables ::
    FreeVariables VariableName ->
    [Sort] ->
    [AttributePattern] ->
    FreeVariables VariableName ->
    Parser (FreeVariables VariableName)
parseFreeVariables freeVariables params args concreteVars = do
    Parser.getZeroParams params
    vars <- mapM getVariable args
    mapM_ checkFree vars
    let newVars =
            -- if no arguments are provides, assume all free variables
            if null vars
                then FreeVariables.toList freeVariables
                else vars
        allVars = newVars ++ FreeVariables.toList concreteVars
        groupedVars = List.group . List.sort $ allVars
        nubVars = mapMaybe Safe.headMay groupedVars
        duplicateVars =
            mapMaybe (Safe.headMay Monad.<=< Safe.tailMay) groupedVars
    unless (null duplicateVars) $
        Kore.Error.koreFail
            ( "duplicate concrete/symbolic variable annotations for "
                ++ show duplicateVars
            )
    return (foldMap freeVariable nubVars)
  where
    checkFree :: SomeVariable VariableName -> Parser ()
    checkFree variable@Variable{variableName} =
        unless (isFreeVariable variableName freeVariables) $
            Kore.Error.koreFail
                ("expected free variable, found " ++ show variable)

instance From (Concrete VariableName) Attributes where
    from =
        from @AttributePattern
            . concreteAttribute
            . FreeVariables.toList
            . unConcrete

mapConcreteVariables ::
    Ord variable2 =>
    AdjSomeVariableName (variable1 -> variable2) ->
    Concrete variable1 ->
    Concrete variable2
mapConcreteVariables adj (Concrete freeVariables) =
    Concrete (mapFreeVariables adj freeVariables)

isConcrete ::
    Ord variable =>
    Concrete variable ->
    SomeVariableName variable ->
    Bool
isConcrete Concrete{unConcrete} someVariableName =
    isFreeVariable someVariableName unConcrete
