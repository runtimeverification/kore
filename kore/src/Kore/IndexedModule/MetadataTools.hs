{- |
Module      : Kore.IndexedModule.MetadataTools
Description : Datastructures and functionality for retrieving metadata
              information from patterns
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.IndexedModule.MetadataTools (
    MetadataTools (..),
    MetadataSyntaxData (..),
    MetadataTables (..),
    SmtMetadataTools,
    extractMetadataTools,
    findSortConstructors,
    sortAttributes,
    applicationSorts,
    symbolAttributes,
) where

import Data.Bifunctor (first)
import Data.Binary (Binary (..))
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import GHC.Generics qualified as GHC
import Kore.Attribute.Sort qualified as Attribute
import Kore.Attribute.Sort.Constructors qualified as Attribute (
    Constructors,
 )
import Kore.IndexedModule.IndexedModule
import Kore.IndexedModule.Resolvers
import Kore.Internal.ApplicationSorts hiding (
    applicationSorts,
 )
import Kore.Rewrite.SMT.AST qualified as SMT.AST (
    SmtDeclarations,
 )
import Kore.Sort
import Kore.Syntax.Application (
    SymbolOrAlias (..),
 )
import Prelude.Kore

data MetadataSyntaxData attributes
    = MetadataSyntaxData (VerifiedModuleSyntax attributes)
    | MetadataSyntaxDataTable (MetadataTables attributes)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)

data MetadataTables attributes = MetadataTables
    { sortAttributeTable :: [(Sort, Attribute.Sort)]
    , applicationSortTable :: [(SymbolOrAlias, ApplicationSorts)]
    , symbolAttributeTable :: [(SymbolOrAlias, attributes)]
    }
    deriving stock (Eq, Show, GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)

{- | 'MetadataTools' defines a dictionary of functions which can be used to
 access the metadata needed during the unification process.

We do not derive Functor on this type because it is not currently possible
to guarantee that the type the Functor is mapping to will implement
NFData. If you need to implement Functor here, your best bet is to replace
the NFData constraint in MetadataSyntaxData with a Typeable constraint and
modify the NFData instance so that it uses Typeable to check that the type
it contains has an NFData instance.
-}
data MetadataTools sortConstructors smt attributes = MetadataTools
    { syntax :: MetadataSyntaxData attributes
    -- ^ syntax of module
    , smtData :: smt
    -- ^ The SMT data for the given module.
    , sortConstructors :: Map Id sortConstructors
    -- ^ The constructors for each sort.
    }
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)

type SmtMetadataTools attributes =
    MetadataTools Attribute.Constructors SMT.AST.SmtDeclarations attributes

{- | 'extractMetadataTools' extracts a set of 'MetadataTools' from a
 'KoreIndexedModule'.  The metadata tools are functions yielding information
 about application heads, such as its attributes or
 its argument and result sorts.
-}
extractMetadataTools ::
    forall declAtts smt sortConstructors.
    VerifiedModule declAtts ->
    (VerifiedModule declAtts -> Map Id sortConstructors) ->
    (VerifiedModule declAtts -> Map Id sortConstructors -> smt) ->
    MetadataTools sortConstructors smt declAtts
extractMetadataTools m constructorsExtractor smtExtractor =
    MetadataTools
        { syntax = MetadataSyntaxData $ indexedModuleSyntax m
        , smtData = smtExtractor m constructors
        , sortConstructors = constructors
        }
  where
    constructors = constructorsExtractor m

{- | Extracts all constructors for a sort.

Should return Nothing if the sort has no constructors or if the sort has no
constructors, or `inj` behaves like a constructor, but the latter is only true
if the constructor axioms actually include `inj` when needed, which is not true
right now.
-}
findSortConstructors ::
    SmtMetadataTools attributes -> Id -> Maybe Attribute.Constructors
findSortConstructors
    MetadataTools{sortConstructors}
    sortId =
        Map.lookup sortId sortConstructors

sortAttributes ::
    MetadataTools sortConstructors smt attributes ->
    Sort ->
    Attribute.Sort
sortAttributes MetadataTools{syntax = MetadataSyntaxData sdata} =
    getSortAttributes sdata
sortAttributes MetadataTools{syntax = MetadataSyntaxDataTable sdata} =
    either error id . lookupThingIn (sortAttributeTable sdata)

applicationSorts ::
    MetadataTools sortConstructors smt attributes ->
    SymbolOrAlias ->
    ApplicationSorts
applicationSorts MetadataTools{syntax = MetadataSyntaxData sdata} =
    getHeadApplicationSorts sdata
applicationSorts MetadataTools{syntax = MetadataSyntaxDataTable sdata} =
    either error id . lookupThingIn (applicationSortTable sdata)

symbolAttributes ::
    MetadataTools sortConstructors smt attributes ->
    Id ->
    attributes
symbolAttributes MetadataTools{syntax = MetadataSyntaxData sdata} =
    getSymbolAttributes sdata
symbolAttributes MetadataTools{syntax = MetadataSyntaxDataTable sdata} =
    let table = map (first symbolOrAliasConstructor) $ symbolAttributeTable sdata
     in either error id . lookupThingIn table

lookupThingIn :: (Show a, Eq a) => [(a, b)] -> a -> Either String b
lookupThingIn table k =
    maybe (Left $ show k <> " not found") Right $ lookup k table
