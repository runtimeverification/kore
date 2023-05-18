{- |
Module      : Kore.IndexedModule.IndexedModule
Description : Indexed representation for a module.
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.IndexedModule.IndexedModule (
    ImplicitIndexedModule (ImplicitIndexedModule),
    IndexedModule (
        -- the IndexedModule data constructor not included in the list on
        -- purpose.
        indexedModuleSyntax,
        indexedModuleAxioms,
        indexedModuleClaims,
        indexedModuleAttributes,
        indexedModuleImports,
        indexedModuleHooks
    ),
    IndexedModuleSyntax (..),
    IndexModuleError,
    KoreImplicitIndexedModule,
    KoreIndexedModule,
    VerifiedModule,
    VerifiedModuleSyntax,
    indexedModuleWithDefaultImports,
    eraseAttributes,
    eraseAxiomAttributes,
    erasePatterns,
    emptyIndexedModule,
    mapPatterns,
    mapAliasPatterns,
    indexedModuleRawSentences,
    SortDescription,
    getIndexedSentence,
    hookedObjectSymbolSentences,
    indexedModuleSubsorts,
    internalIndexedModuleSubsorts,
    indexedModulesInScope,
    toModule,
    toVerifiedModule,
    toVerifiedDefinition,
    recursiveIndexedModuleAxioms,
    recursiveIndexedModuleSortDescriptions,
    recursiveIndexedModuleSymbolSentences,

    -- * Implicit Kore
    implicitModuleName,
    implicitNames,
    implicitSortNames,
    implicitIndexedModule,
    implicitModules,
) where

import Control.Arrow ((&&&))
import Control.Lens qualified as Lens
import Control.Monad.Extra (
    unlessM,
 )
import Control.Monad.State.Strict (
    execState,
 )
import Control.Monad.State.Strict qualified as Monad.State
import Data.Binary (Binary)
import Data.Default as Default
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.InternedText (InternedText)
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Data.Text (
    Text,
 )
import GHC.Generics (
    Generic,
 )
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Null qualified as Attribute
import Kore.Attribute.Parser qualified as Attribute.Parser
import Kore.Attribute.Sort qualified as Attribute (
    Sort,
 )
import Kore.Attribute.Subsort
import Kore.Attribute.Symbol qualified as Attribute (
    Symbol,
 )
import Kore.Error
import Kore.Internal.Symbol qualified as Internal.Symbol
import Kore.Syntax
import Kore.Syntax.Definition
import Kore.Verified qualified as Verified
import Prelude.Kore

type SortDescription = SentenceSort

data IndexModuleError

{- | 'IndexedModule' represents an AST 'Module' somewhat optimized for resolving
IDs.

It contains mappings from IDs to the sentence that declares them
(or to the 'SortDescription' for sorts). Note that only symbols defined
in the current module are included.

It also contains the imported modules as 'IndexedModule's and all the other
module data in raw-ish form.

All 'IndexedModule' instances should be returned by
'indexedModuleWithDefaultImports'.
-}
data IndexedModuleSyntax pat declAtts = IndexedModuleSyntax
    { indexedModuleName :: !ModuleName
    , indexedModuleAliasSentences ::
        !(Map.Map Id (declAtts, SentenceAlias pat))
    , indexedModuleSymbolSentences ::
        !(Map.Map Id (declAtts, SentenceSymbol))
    , indexedModuleSortDescriptions ::
        !(Map.Map Id (Attribute.Sort, SentenceSort))
    , indexedModuleImportsSyntax ::
        ![ ( declAtts
           , Attributes
           , IndexedModuleSyntax pat declAtts
           )
         ]
    , indexedModuleHookedIdentifiers ::
        !(Set.Set Id)
    -- ^ set of hooked identifiers
    }
    deriving stock (Generic, Show, Functor, Foldable, Traversable)
    deriving anyclass (Binary)

-- TODO (thomas.tuegel): Consider splitting IndexedModule into separate sort,
-- symbol, and axiom indices.
data IndexedModule pat declAtts axiomAtts = IndexedModule
    { indexedModuleSyntax :: !(IndexedModuleSyntax pat declAtts)
    , indexedModuleAxioms :: ![(axiomAtts, SentenceAxiom pat)]
    , indexedModuleClaims :: ![(axiomAtts, SentenceClaim pat)]
    , indexedModuleAttributes :: !(declAtts, Attributes)
    , indexedModuleImports ::
        ![ ( declAtts
           , Attributes
           , IndexedModule pat declAtts axiomAtts
           )
         ]
    , -- TODO (thomas.tuegel): Having multiple identifiers hooked to the same
      -- builtin is not actually valid, but the index must admit invalid data
      -- because verification only happens after.

      indexedModuleHooks ::
        !(Map.Map Text [Id])
    -- ^ map from builtin domain (symbol and sort) identifiers to the hooked
    -- identifiers
    }
    deriving stock (Generic, Show, Functor, Foldable, Traversable)
    deriving anyclass (Binary)

recursiveIndexedModuleSortDescriptions ::
    forall pat declAtts axiomAtts.
    IndexedModule pat declAtts axiomAtts ->
    Map.Map Id (Attribute.Sort, SentenceSort)
recursiveIndexedModuleSortDescriptions =
    recursiveIndexedModuleStuff
        (indexedModuleSortDescriptions . indexedModuleSyntax)

recursiveIndexedModuleSymbolSentences ::
    forall pat axiomAtts.
    IndexedModule pat Attribute.Symbol axiomAtts ->
    Map.Map Id (Attribute.Symbol, SentenceSymbol)
recursiveIndexedModuleSymbolSentences =
    recursiveIndexedModuleStuff
        (indexedModuleSymbolSentences . indexedModuleSyntax)

recursiveIndexedModuleAxioms ::
    forall pat declAtts axiomAtts.
    IndexedModule pat declAtts axiomAtts ->
    [(axiomAtts, SentenceAxiom pat)]
recursiveIndexedModuleAxioms = recursiveIndexedModuleStuff indexedModuleAxioms

recursiveIndexedModuleStuff ::
    forall pat declAtts axiomAtts stuff.
    Monoid stuff =>
    (IndexedModule pat declAtts axiomAtts -> stuff) ->
    IndexedModule pat declAtts axiomAtts ->
    stuff
recursiveIndexedModuleStuff stuffExtractor m =
    fold (stuffExtractor m : subModuleStuffs)
  where
    subModuleStuffs :: [stuff]
    subModuleStuffs =
        map subModuleStuff (indexedModuleImports m)
    subModuleStuff ::
        ( declAtts
        , Attributes
        , IndexedModule pat declAtts axiomAtts
        ) ->
        stuff
    subModuleStuff (_, _, subMod) =
        recursiveIndexedModuleStuff stuffExtractor subMod

-- | Strip module of its parsed attributes, replacing them with 'Attribute.Null'
eraseAxiomAttributes ::
    IndexedModule patternType1 declAttributes axiomAttributes ->
    IndexedModule patternType1 declAttributes Attribute.Null
eraseAxiomAttributes
    indexedModule@IndexedModule
        { indexedModuleAxioms
        , indexedModuleClaims
        , indexedModuleImports
        } =
        indexedModule
            { indexedModuleAxioms =
                indexedModuleAxioms
                    & Lens.set (Lens.mapped . Lens._1) Attribute.Null
            , indexedModuleClaims =
                indexedModuleClaims
                    & Lens.set (Lens.mapped . Lens._1) Attribute.Null
            , indexedModuleImports =
                indexedModuleImports
                    & Lens.over (Lens.mapped . Lens._3) eraseAxiomAttributes
            }

eraseSyntaxAttributes ::
    IndexedModuleSyntax patternType1 declAttributes ->
    IndexedModuleSyntax patternType1 Attribute.Null
eraseSyntaxAttributes
    indexedModuleSyntax@IndexedModuleSyntax
        { indexedModuleAliasSentences
        , indexedModuleSymbolSentences
        , indexedModuleImportsSyntax
        } =
        indexedModuleSyntax
            { indexedModuleAliasSentences =
                indexedModuleAliasSentences
                    & Lens.set (Lens.mapped . Lens._1) Attribute.Null
            , indexedModuleSymbolSentences =
                indexedModuleSymbolSentences
                    & Lens.set (Lens.mapped . Lens._1) Attribute.Null
            , indexedModuleImportsSyntax =
                indexedModuleImportsSyntax
                    & Lens.set (Lens.mapped . Lens._1) Attribute.Null
                    & Lens.over (Lens.mapped . Lens._3) eraseSyntaxAttributes
            }

-- | Strip module of its parsed attributes, replacing them with 'Attribute.Null'
eraseAttributes ::
    IndexedModule patternType1 declAttributes axiomAttributes ->
    IndexedModule patternType1 Attribute.Null Attribute.Null
eraseAttributes
    indexedModule@IndexedModule
        { indexedModuleSyntax
        , indexedModuleAxioms
        , indexedModuleClaims
        , indexedModuleAttributes
        , indexedModuleImports
        } =
        indexedModule
            { indexedModuleSyntax =
                eraseSyntaxAttributes indexedModuleSyntax
            , indexedModuleAxioms =
                indexedModuleAxioms
                    & Lens.set (Lens.mapped . Lens._1) Attribute.Null
            , indexedModuleClaims =
                indexedModuleClaims
                    & Lens.set (Lens.mapped . Lens._1) Attribute.Null
            , indexedModuleAttributes =
                indexedModuleAttributes
                    & Lens.set Lens._1 Attribute.Null
            , indexedModuleImports =
                indexedModuleImports
                    & Lens.set (Lens.mapped . Lens._1) Attribute.Null
                    & Lens.over (Lens.mapped . Lens._3) eraseAttributes
            }

instance
    (NFData patternType, NFData declAttributes, NFData axiomAttributes) =>
    NFData (IndexedModule patternType declAttributes axiomAttributes)
instance
    (NFData patternType, NFData declAttributes) =>
    NFData (IndexedModuleSyntax patternType declAttributes)

{- | Convenient notation for retrieving a sentence from a
 @(attributes,sentence)@ pair format.
-}
getIndexedSentence :: (atts, sentence) -> sentence
getIndexedSentence = snd

{- | Erase the patterns carried by an 'IndexedModule'.

The alias declarations are preserved, but the right-hand side of each alias is
erased. The axiom and claim declarations are erased entirely.

This is useful because pattern verification needs to know about declared sorts,
symbols, and aliases, but it does not need to know anything about patterns.
-}
eraseAliasPatterns ::
    IndexedModuleSyntax patternType1 declAttributes ->
    IndexedModuleSyntax () declAttributes
eraseAliasPatterns indexedModuleSyntax =
    indexedModuleSyntax
        { indexedModuleAliasSentences =
            Lens.set (Lens.mapped . Lens._2 . Lens.mapped) () $
                indexedModuleAliasSentences indexedModuleSyntax
        , indexedModuleImportsSyntax =
            Lens.over (Lens.mapped . Lens._3) eraseAliasPatterns $
                indexedModuleImportsSyntax indexedModuleSyntax
        }

erasePatterns ::
    IndexedModule patternType1 declAttributes axiomAttributes ->
    IndexedModule () declAttributes axiomAttributes
erasePatterns indexedModule =
    indexedModule
        { indexedModuleSyntax =
            eraseAliasPatterns $ indexedModuleSyntax indexedModule
        , indexedModuleAxioms = []
        , indexedModuleClaims = []
        , indexedModuleImports =
            Lens.over (Lens.mapped . Lens._3) erasePatterns $
                indexedModuleImports indexedModule
        }

mapAliasPatterns ::
    (patternType1 -> patternType2) ->
    IndexedModuleSyntax patternType1 declAttributes ->
    IndexedModuleSyntax patternType2 declAttributes
mapAliasPatterns mapping indexedModuleSyntax =
    indexedModuleSyntax
        { indexedModuleAliasSentences =
            (fmap . fmap . fmap) mapping indexedModuleAliasSentences
        , indexedModuleImportsSyntax =
            indexedModuleImportsSyntax
                & Lens.over (Lens.mapped . Lens._3) (mapAliasPatterns mapping)
        }
  where
    IndexedModuleSyntax{indexedModuleAliasSentences} = indexedModuleSyntax
    IndexedModuleSyntax{indexedModuleImportsSyntax} = indexedModuleSyntax

mapPatterns ::
    (patternType1 -> patternType2) ->
    IndexedModule patternType1 declAttributes axiomAttributes ->
    IndexedModule patternType2 declAttributes axiomAttributes
mapPatterns mapping indexedModule =
    indexedModule
        { indexedModuleSyntax =
            mapAliasPatterns mapping indexedModuleSyntax
        , indexedModuleAxioms =
            (fmap . fmap . fmap) mapping indexedModuleAxioms
        , indexedModuleClaims =
            (fmap . fmap . fmap) mapping indexedModuleClaims
        , indexedModuleImports =
            indexedModuleImports
                & Lens.over (Lens.mapped . Lens._3) (mapPatterns mapping)
        }
  where
    IndexedModule{indexedModuleSyntax} = indexedModule
    IndexedModule{indexedModuleAxioms} = indexedModule
    IndexedModule{indexedModuleClaims} = indexedModule
    IndexedModule{indexedModuleImports} = indexedModule

type KoreIndexedModule = IndexedModule ParsedPattern

type VerifiedModule declAtts =
    IndexedModule
        Verified.Pattern
        declAtts
        (Attribute.Axiom Internal.Symbol.Symbol VariableName)

type VerifiedModuleSyntax =
    IndexedModuleSyntax
        Verified.Pattern

{- | Convert a 'IndexedModule' back into a 'Module'.

The original module attributes /are/ preserved.
-}
toModule ::
    IndexedModule patternType declAtts axiomAtts ->
    Module (Sentence patternType)
toModule module' =
    Module
        { moduleName = indexedModuleName $ indexedModuleSyntax module'
        , moduleSentences = indexedModuleRawSentences module'
        , moduleAttributes = snd (indexedModuleAttributes module')
        }

{- | Convert a 'VerifiedModule' back into a 'Module'.

The original module attributes /are/ preserved.
-}
toVerifiedModule ::
    VerifiedModule declAtts ->
    Module Verified.Sentence
toVerifiedModule = toModule

{- | Convert any collection of 'VerifiedModule's back into a 'Definition'.

The definition attributes are lost in the process of indexing the original
definition.

Although all 'IndexedModule's refer to the implicit Kore module, it is not
included in the output of this function because it is /implicit/.

See also: 'toVerifiedPureModule'
-}
toVerifiedDefinition ::
    Foldable t =>
    t (VerifiedModule declAtts) ->
    Definition Verified.Sentence
toVerifiedDefinition idx =
    Definition
        { definitionAttributes = Default.def
        , definitionModules =
            toVerifiedModule
                <$> filter notImplicitKoreModule (toList idx)
        }
  where
    notImplicitKoreModule verifiedModule =
        indexedModuleName (indexedModuleSyntax verifiedModule) /= "kore"

indexedModuleRawSentences ::
    IndexedModule pat atts atts' -> [Sentence pat]
indexedModuleRawSentences im =
    map
        (SentenceAliasSentence . getIndexedSentence)
        (Map.elems (indexedModuleAliasSentences $ indexedModuleSyntax im))
        ++ map
            hookSymbolIfNeeded
            (Map.toList (indexedModuleSymbolSentences $ indexedModuleSyntax im))
        ++ map
            hookSortIfNeeded
            (Map.toList (indexedModuleSortDescriptions $ indexedModuleSyntax im))
        ++ map
            (SentenceAxiomSentence . getIndexedSentence)
            (indexedModuleAxioms im)
        ++ map
            (SentenceClaimSentence . getIndexedSentence)
            (indexedModuleClaims im)
        ++ [ SentenceImportSentence
            (SentenceImport (indexedModuleName $ indexedModuleSyntax m) attributes)
           | (_, attributes, m) <- indexedModuleImports im
           ]
  where
    hookedIds :: Set.Set Id
    hookedIds = indexedModuleHookedIdentifiers $ indexedModuleSyntax im
    hookSortIfNeeded (x, (_, sortDescription))
        | x `Set.member` hookedIds =
            SentenceHookSentence (SentenceHookedSort sortDescription)
        | otherwise =
            SentenceSortSentence sortDescription
    hookSymbolIfNeeded (x, (_, symbolSentence))
        | x `Set.member` hookedIds =
            SentenceHookSentence (SentenceHookedSymbol symbolSentence)
        | otherwise =
            SentenceSymbolSentence symbolSentence

{- | 'ImplicitIndexedModule' is the type for the 'IndexedModule' containing
things that are implicitly defined.
-}
newtype ImplicitIndexedModule pat declAtts axiomAtts
    = ImplicitIndexedModule (IndexedModule pat declAtts axiomAtts)
    deriving stock (Show)
    deriving newtype (Generic)
    deriving newtype (Binary)

type KoreImplicitIndexedModule = ImplicitIndexedModule ParsedPattern

emptyIndexedModule ::
    Default parsedDeclAttributes =>
    ModuleName ->
    IndexedModule pat parsedDeclAttributes parsedAxiomAttributes
emptyIndexedModule name =
    IndexedModule
        { indexedModuleSyntax =
            IndexedModuleSyntax
                { indexedModuleName = name
                , indexedModuleAliasSentences = Map.empty
                , indexedModuleSymbolSentences = Map.empty
                , indexedModuleSortDescriptions = Map.empty
                , indexedModuleImportsSyntax = []
                , indexedModuleHookedIdentifiers = Set.empty
                }
        , indexedModuleAxioms = []
        , indexedModuleClaims = []
        , indexedModuleAttributes = (def, Attributes [])
        , indexedModuleImports = []
        , indexedModuleHooks = Map.empty
        }

{- | 'indexedModuleWithDefaultImports' provides an 'IndexedModule' with the given
name and containing the implicit definitions module.
-}
indexedModuleWithDefaultImports ::
    Default declAttrs =>
    ModuleName ->
    Maybe (ImplicitIndexedModule patternType declAttrs axiomAttrs) ->
    IndexedModule patternType declAttrs axiomAttrs
indexedModuleWithDefaultImports name defaultImport =
    indexedModule
        { indexedModuleSyntax =
            (indexedModuleSyntax indexedModule)
                { indexedModuleImportsSyntax =
                    case defaultImport of
                        Just (ImplicitIndexedModule implicitModule) ->
                            [(def, Attributes [], indexedModuleSyntax implicitModule)]
                        Nothing ->
                            []
                }
        , indexedModuleImports =
            case defaultImport of
                Just (ImplicitIndexedModule implicitModule) ->
                    [(def, Attributes [], implicitModule)]
                Nothing ->
                    []
        }
  where
    indexedModule = emptyIndexedModule name

-- | Retrieve those object-level symbol sentences that are hooked.
hookedObjectSymbolSentences ::
    IndexedModuleSyntax pat declAtts ->
    Map.Map Id (declAtts, SentenceSymbol)
hookedObjectSymbolSentences
    IndexedModuleSyntax
        { indexedModuleSymbolSentences
        , indexedModuleHookedIdentifiers
        } =
        Map.restrictKeys
            indexedModuleSymbolSentences
            indexedModuleHookedIdentifiers

indexedModuleSubsorts :: IndexedModule pat declAtts axiomAtts -> [Subsort]
indexedModuleSubsorts imod =
    case internalIndexedModuleSubsorts imod of
        Right subsorts -> subsorts
        Left err ->
            error $
                "IndexedModule should already have checked"
                    ++ " form of subsort attributes, but parsing failed\n:"
                    ++ show err

internalIndexedModuleSubsorts ::
    MonadError (Error e) error =>
    IndexedModule pat declAtts axiomAtts ->
    error [Subsort]
internalIndexedModuleSubsorts imod = do
    let attributes =
            [ sentenceAxiomAttributes
            | (_, SentenceAxiom{sentenceAxiomAttributes}) <-
                recursiveIndexedModuleAxioms imod
            ]
    Subsorts subsorts <-
        Attribute.Parser.liftParser $
            foldrM Attribute.Parser.parseAttributesWith def attributes
    return subsorts

{- | Determine all indexed modules in scope from the given module.

@indexedModulesInScope@ resolves all imported modules recursively so that
traversing the 'Map' is equivalent to traversing all the (transitively) imported
modules once.
-}
indexedModulesInScope ::
    IndexedModule pat declAtts axiomAtts ->
    Map ModuleName (IndexedModule pat declAtts axiomAtts)
indexedModulesInScope =
    \imod -> execState (resolveModule imod) Map.empty
  where
    alreadyResolved name =
        Monad.State.gets (Map.member name)
    resolveModule imod =
        unlessM (alreadyResolved name) $ do
            -- resolve the module: insert it into the map
            Monad.State.modify' (Map.insert name imod)
            -- resolve this modules imports
            mapM_ resolveImport (indexedModuleImports imod)
      where
        name = indexedModuleName $ indexedModuleSyntax imod

    resolveImport (_, _, imod) = resolveModule imod

implicitModules ::
    Default declAttrs =>
    Map ModuleName (IndexedModule patternType declAttrs axiomAttrs)
implicitModules = Map.singleton implicitModuleName implicitIndexedModule

-- | The name of the module containing the implicit Kore declarations.
implicitModuleName :: ModuleName
implicitModuleName = ModuleName "kore"

-- | The 'IndexedModule' that indexes the implicit Kore declarations.
implicitIndexedModule ::
    Default declAttrs =>
    IndexedModule patternType declAttrs axiomAttrs
implicitIndexedModule =
    indexedModule
        { indexedModuleSyntax =
            (indexedModuleSyntax indexedModule)
                { indexedModuleSortDescriptions =
                    Map.fromSet makeSortIndex implicitSortNames
                }
        }
  where
    indexedModule = emptyIndexedModule implicitModuleName
    makeSortIndex sortId = (Default.def, declareSort sortId)
    declareSort sortId =
        SentenceSort
            { sentenceSortName = sortId
            , sentenceSortParameters = []
            , sentenceSortAttributes = Attributes []
            }

implicitNames :: HashMap InternedText AstLocation
implicitNames =
    HashMap.fromList $
        (getInternedId &&& idLocation)
            <$> (predicateSortId : Set.toList implicitSortNames)

implicitSortNames :: Set Id
implicitSortNames = Set.fromList [stringMetaSortId]
