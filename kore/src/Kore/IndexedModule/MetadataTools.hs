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
    ExtractSyntax (..),
    MetadataTools (..),
    MetadataSyntaxData (..),
    SmtMetadataTools,
    extractMetadataTools,
    findSortConstructors,
    sortAttributes,
    applicationSorts,
    symbolAttributes,
) where

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

class ExtractSyntax a where
    extractSortAttributes :: a attributes -> Sort -> Attribute.Sort
    extractApplicationSorts :: a attributes -> SymbolOrAlias -> ApplicationSorts
    extractSymbolAttributes :: a attributes -> Id -> attributes

instance ExtractSyntax VerifiedModuleSyntax where
    extractSortAttributes = getSortAttributes
    extractApplicationSorts = getHeadApplicationSorts
    extractSymbolAttributes = getSymbolAttributes

data MetadataSyntaxData attributes
    = MetadataSyntaxData (VerifiedModuleSyntax attributes)
    | forall syntaxData.
        ( ExtractSyntax syntaxData
        , NFData (syntaxData attributes)
        ) =>
      MetadataSyntaxDataExtension (syntaxData attributes)

instance
    Binary (VerifiedModuleSyntax attributes) =>
    Binary (MetadataSyntaxData attributes)
    where
    put (MetadataSyntaxData vms) = put vms
    put MetadataSyntaxDataExtension{} = error "urk"
    get = error "ork"

instance NFData (MetadataSyntaxData attributes) where
    rnf (MetadataSyntaxData sdata) = sdata `seq` ()
    rnf (MetadataSyntaxDataExtension sdata) = sdata `seq` ()

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
sortAttributes MetadataTools{syntax = MetadataSyntaxData sdata} s =
    extractSortAttributes sdata s
sortAttributes MetadataTools{syntax = MetadataSyntaxDataExtension sdata} s =
    extractSortAttributes sdata s

applicationSorts ::
    MetadataTools sortConstructors smt attributes ->
    SymbolOrAlias ->
    ApplicationSorts
applicationSorts MetadataTools{syntax = MetadataSyntaxData sdata} s =
    extractApplicationSorts sdata s
applicationSorts MetadataTools{syntax = MetadataSyntaxDataExtension sdata} s =
    extractApplicationSorts sdata s

symbolAttributes ::
    MetadataTools sortConstructors smt attributes ->
    Id ->
    attributes
symbolAttributes MetadataTools{syntax = MetadataSyntaxData sdata} s =
    extractSymbolAttributes sdata s
symbolAttributes MetadataTools{syntax = MetadataSyntaxDataExtension sdata} s =
    extractSymbolAttributes sdata s
