{- |
Module      : Kore.Rewrite.SMT.Resolvers
Description : Translates sorts and symbols to a SMT representation.
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Rewrite.SMT.Resolvers (
    translateSort,
    translateSymbol,
) where

import Data.Map.Strict qualified as Map
import Data.Reflection (
    Given,
    given,
 )
import Kore.Attribute.Symbol qualified as Attribute
import Kore.IndexedModule.MetadataTools (
    MetadataTools (MetadataTools),
    SmtMetadataTools,
 )
import Kore.IndexedModule.MetadataTools qualified as MetadataTools (
    MetadataTools (smtData),
 )
import Kore.Internal.Symbol
import Kore.Rewrite.SMT.AST qualified as AST (
    Declarations (Declarations),
    Sort (Sort),
    Symbol (Symbol),
 )
import Kore.Rewrite.SMT.AST qualified as AST.DoNotUse
import Kore.Sort (
    Sort (SortActualSort, SortVariableSort),
    SortActual (SortActual, sortActualName, sortActualSorts),
 )
import Prelude.Kore
import SMT qualified

{- | Creates the SMT representation of a symbol assuming the smt declarations in
the given SmtMetadataTools.
-}
translateSymbol ::
    Given (SmtMetadataTools Attribute.Symbol) =>
    Symbol ->
    Maybe SMT.SExpr
translateSymbol Symbol{symbolConstructor, symbolParams} = do
    AST.Symbol{symbolSmtFromSortArgs} <- Map.lookup symbolConstructor symbols
    symbolSmtFromSortArgs sorts symbolParams
  where
    MetadataTools{smtData = AST.Declarations{sorts, symbols}} = tools

    tools :: SmtMetadataTools Attribute.Symbol
    tools = given

translateSort ::
    Given (SmtMetadataTools Attribute.Symbol) =>
    Sort ->
    Maybe SMT.SExpr
translateSort
    (SortActualSort SortActual{sortActualName, sortActualSorts}) =
        do
            AST.Sort{sortSmtFromSortArgs} <- Map.lookup sortActualName sorts
            sortSmtFromSortArgs sorts sortActualSorts
      where
        MetadataTools{smtData = AST.Declarations{sorts}} = tools

        tools :: SmtMetadataTools Attribute.Symbol
        tools = given
translateSort (SortVariableSort _) = Nothing
