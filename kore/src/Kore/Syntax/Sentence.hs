{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.Sentence (
    Symbol (..),
    groundSymbol,
    Alias (..),
    SentenceAlias (..),
    SentenceSymbol (..),
    SentenceImport (..),
    SentenceSort (..),
    SentenceAxiom (..),
    SentenceClaim (..),
    sentenceClaimAttributes,
    SentenceHook (..),
    Sentence (..),
    projectSentenceImport,
    projectSentenceSort,
    projectSentenceSymbol,
    projectSentenceHookedSort,
    projectSentenceHookedSymbol,
    projectSentenceAlias,
    projectSentenceAxiom,
    projectSentenceClaim,
    sentenceAttributes,
    eraseSentenceAnnotations,

    -- * Injections and projections
    asSentence,
    SentenceSymbolOrAlias (..),

    -- * Type synonyms
    PureSentenceSymbol,
    PureSentenceAlias,
    PureSentenceImport,
    PureSentenceAxiom,
    PureSentenceHook,
    PureSentence,
    PureModule,
    ParsedSentenceAlias,
    ParsedSentenceSymbol,
    ParsedSentenceImport,
    ParsedSentenceAxiom,
    ParsedSentenceSort,
    ParsedSentenceHook,
    ParsedSentence,
    ParsedModule,

    -- * Re-exports
    module Kore.Attribute.Attributes,
    module Kore.Syntax.Module,
) where

import Control.Monad qualified as Monad
import Data.Binary (Binary)
import Data.Generics.Sum.Typed (
    projectTyped,
 )
import Data.Kind (
    Type,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Attributes
import Kore.Attribute.Null qualified as Attribute (
    Null (..),
 )
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables (..),
    freeVariable,
 )
import Kore.Debug
import Kore.Sort
import Kore.Syntax.Application
import Kore.Syntax.Module
import Kore.Syntax.Pattern (
    Pattern,
 )
import Kore.Syntax.Variable
import Kore.Unparser
import Kore.Variables.Free (
    freePureVariables,
 )
import Prelude.Kore
import Pretty qualified

{- | 'Symbol' is the @sort-variables@ part of the
@symbol@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>

See also: 'SymbolOrAlias'
-}
data Symbol = Symbol
    { symbolConstructor :: !Id
    , symbolParams :: ![SortVariable]
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse Symbol where
    unparse Symbol{symbolConstructor, symbolParams} =
        unparse symbolConstructor
            <> parameters symbolParams

    unparse2 Symbol{symbolConstructor} =
        unparse2 symbolConstructor

{- | Given an 'Id', 'groundSymbol' produces the unparameterized 'Symbol'
 corresponding to that argument.
-}
groundSymbol :: Id -> Symbol
groundSymbol ctor =
    Symbol
        { symbolConstructor = ctor
        , symbolParams = []
        }

{- | 'Alias' corresponds to the @sort-variables@ part of
the @alias@ syntactic category from
<https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>.

See also: 'SymbolOrAlias'.
-}
data Alias = Alias
    { aliasConstructor :: !Id
    , aliasParams :: ![SortVariable]
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse Alias where
    unparse Alias{aliasConstructor, aliasParams} =
        unparse aliasConstructor <> parameters aliasParams
    unparse2 Alias{aliasConstructor} =
        unparse2 aliasConstructor

-- | 'SentenceAlias' corresponds to the @sentence-alias@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>.
data SentenceAlias (patternType :: Type) = SentenceAlias
    { sentenceAliasAlias :: !Alias
    , sentenceAliasSorts :: ![Sort]
    , sentenceAliasResultSort :: !Sort
    , sentenceAliasLeftPattern ::
        !(Application SymbolOrAlias (SomeVariable VariableName))
    , sentenceAliasRightPattern :: !patternType
    , sentenceAliasAttributes :: !Attributes
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse patternType => Unparse (SentenceAlias patternType) where
    unparse
        SentenceAlias
            { sentenceAliasAlias
            , sentenceAliasSorts
            , sentenceAliasResultSort
            , sentenceAliasLeftPattern
            , sentenceAliasRightPattern
            , sentenceAliasAttributes
            } =
            Pretty.fillSep
                [ "alias"
                , unparse sentenceAliasAlias <> arguments sentenceAliasSorts
                , ":"
                , unparse sentenceAliasResultSort
                , "where"
                , unparse sentenceAliasLeftPattern
                , ":="
                , unparse sentenceAliasRightPattern
                , unparse sentenceAliasAttributes
                ]

    unparse2
        SentenceAlias
            { sentenceAliasAlias
            , sentenceAliasSorts
            , sentenceAliasResultSort
            , sentenceAliasLeftPattern
            , sentenceAliasRightPattern
            , sentenceAliasAttributes
            } =
            Pretty.fillSep
                [ "alias"
                , unparse2 sentenceAliasAlias <> arguments2 sentenceAliasSorts
                , ":"
                , unparse2 sentenceAliasResultSort
                , "where"
                , unparse2 sentenceAliasLeftPattern
                , ":="
                , unparse2 sentenceAliasRightPattern
                , unparse2 sentenceAliasAttributes
                ]

-- | 'SentenceSymbol' is the @sentence-symbol@ and syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>.
data SentenceSymbol = SentenceSymbol
    { sentenceSymbolSymbol :: !Symbol
    , sentenceSymbolSorts :: ![Sort]
    , sentenceSymbolResultSort :: !Sort
    , sentenceSymbolAttributes :: !Attributes
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse SentenceSymbol where
    unparse
        SentenceSymbol
            { sentenceSymbolSymbol
            , sentenceSymbolSorts
            , sentenceSymbolResultSort
            , sentenceSymbolAttributes
            } =
            Pretty.fillSep
                [ "symbol"
                , unparse sentenceSymbolSymbol <> arguments sentenceSymbolSorts
                , ":"
                , unparse sentenceSymbolResultSort
                , unparse sentenceSymbolAttributes
                ]

    unparse2
        SentenceSymbol
            { sentenceSymbolSymbol
            , sentenceSymbolSorts
            , sentenceSymbolResultSort
            } =
            Pretty.vsep
                [ Pretty.fillSep ["symbol", unparse2 sentenceSymbolSymbol]
                , Pretty.fillSep
                    [ "axiom \\forall s Sorts"
                    , Pretty.parens
                        ( Pretty.fillSep
                            [ "\\subset"
                            , Pretty.parens
                                ( Pretty.fillSep
                                    [ unparse2 sentenceSymbolSymbol
                                    , unparse2Inhabitant sentenceSymbolSorts
                                    ]
                                )
                            , unparse2 sentenceSymbolResultSort
                            ]
                        )
                    ]
                ]
          where
            unparse2Inhabitant ss =
                case ss of
                    [] -> ""
                    (s : rest) ->
                        Pretty.parens (Pretty.fillSep ["\\inh", unparse2 s])
                            <> unparse2Inhabitant rest

{- | 'SentenceImport' corresponds to the @sentence-import@ syntactic category
from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>.
-}
data SentenceImport = SentenceImport
    { sentenceImportModuleName :: !ModuleName
    , sentenceImportAttributes :: !Attributes
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse SentenceImport where
    unparse
        SentenceImport{sentenceImportModuleName, sentenceImportAttributes} =
            Pretty.fillSep
                [ "import"
                , unparse sentenceImportModuleName
                , unparse sentenceImportAttributes
                ]

    unparse2
        SentenceImport{sentenceImportModuleName, sentenceImportAttributes} =
            Pretty.fillSep
                [ "import"
                , unparse2 sentenceImportModuleName
                , unparse2 sentenceImportAttributes
                ]

{- | 'SentenceSort' corresponds to the @sentence-sort@ syntactic category
from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>.
-}
data SentenceSort = SentenceSort
    { sentenceSortName :: !Id
    , sentenceSortParameters :: ![SortVariable]
    , sentenceSortAttributes :: !Attributes
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse SentenceSort where
    unparse
        SentenceSort
            { sentenceSortName
            , sentenceSortParameters
            , sentenceSortAttributes
            } =
            Pretty.fillSep
                [ "sort"
                , unparse sentenceSortName <> parameters sentenceSortParameters
                , unparse sentenceSortAttributes
                ]

    unparse2
        SentenceSort
            { sentenceSortName
            , sentenceSortParameters
            } =
            Pretty.vsep
                [ Pretty.fillSep ["symbol", unparse2 sentenceSortName, "[sort]"]
                , Pretty.fillSep
                    [ "axiom"
                    , "\\subset"
                    , Pretty.parens
                        ( Pretty.fillSep
                            [ unparse2 sentenceSortName
                            , printLbSortsRb (length sentenceSortParameters)
                            ]
                        )
                    , "Sorts"
                    ]
                ]
          where
            printLbSortsRb n =
                case n of
                    0 -> ""
                    m -> Pretty.fillSep ["(\\inh Sorts)", printLbSortsRb (m - 1)]

-- | 'SentenceAxiom' corresponds to the @sentence-axiom@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>.
data SentenceAxiom (patternType :: Type) = SentenceAxiom
    { sentenceAxiomParameters :: ![SortVariable]
    , sentenceAxiomPattern :: !patternType
    , sentenceAxiomAttributes :: !Attributes
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse patternType => Unparse (SentenceAxiom patternType) where
    unparse = unparseAxiom "axiom"
    unparse2 = unparseAxiom2 "axiom"

instance
    Ord variable =>
    HasFreeVariables (SentenceAxiom (Pattern variable annotation)) variable
    where
    freeVariables =
        foldMap freeVariable
            . freePureVariables
            . sentenceAxiomPattern

unparseAxiom ::
    Unparse patternType =>
    Pretty.Doc ann ->
    SentenceAxiom patternType ->
    Pretty.Doc ann
unparseAxiom
    label
    SentenceAxiom
        { sentenceAxiomParameters
        , sentenceAxiomPattern
        , sentenceAxiomAttributes
        } =
        Pretty.fillSep
            [ label
            , parameters sentenceAxiomParameters
            , unparse sentenceAxiomPattern
            , unparse sentenceAxiomAttributes
            ]

unparseAxiom2 ::
    Unparse patternType =>
    Pretty.Doc ann ->
    SentenceAxiom patternType ->
    Pretty.Doc ann
unparseAxiom2
    label
    SentenceAxiom
        { sentenceAxiomPattern
        , sentenceAxiomAttributes
        } =
        Pretty.fillSep
            [ label
            , unparse2 sentenceAxiomPattern
            , unparse2 sentenceAxiomAttributes
            ]

-- | 'SentenceClaim' corresponds to the @sentence-claim@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>.
newtype SentenceClaim (patternType :: Type) = SentenceClaim {getSentenceClaim :: SentenceAxiom patternType}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving stock (Traversable)
    deriving newtype (Functor, Foldable)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

sentenceClaimAttributes :: SentenceClaim patternType -> Attributes
sentenceClaimAttributes = sentenceAxiomAttributes . getSentenceClaim

instance Unparse patternType => Unparse (SentenceClaim patternType) where
    unparse = unparseAxiom "claim" . getSentenceClaim
    unparse2 = unparseAxiom2 "claim" . getSentenceClaim

instance
    Ord variable =>
    HasFreeVariables (SentenceClaim (Pattern variable annotation)) variable
    where
    freeVariables = freeVariables . getSentenceClaim

{- | 'SentenceHook' corresponds to @sentence-hooked-sort@ and @sentence-hooked-symbol@ syntactic categories from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax#sentences>.

See also: 'SentenceSort', 'SentenceSymbol'
-}
data SentenceHook
    = SentenceHookedSort !SentenceSort
    | SentenceHookedSymbol !SentenceSymbol
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse SentenceHook where
    unparse =
        \case
            SentenceHookedSort a -> "hooked-" <> unparse a
            SentenceHookedSymbol a -> "hooked-" <> unparse a

    unparse2 =
        \case
            SentenceHookedSort a -> "hooked-" <> unparse2 a
            SentenceHookedSymbol a -> "hooked-" <> unparse2 a

-- | 'Sentence' is the @sentence@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax#sentences>.
data Sentence (patternType :: Type)
    = SentenceAliasSentence !(SentenceAlias patternType)
    | SentenceSymbolSentence !SentenceSymbol
    | SentenceImportSentence !SentenceImport
    | SentenceAxiomSentence !(SentenceAxiom patternType)
    | SentenceClaimSentence !(SentenceClaim patternType)
    | SentenceSortSentence !SentenceSort
    | SentenceHookSentence !SentenceHook
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse patternType => Unparse (Sentence patternType) where
    unparse = unparseGeneric
    unparse2 = unparse2Generic

instance Injection (Sentence patternType) (SentenceAlias patternType) where
    inject = SentenceAliasSentence

    retract (SentenceAliasSentence sentenceAlias) = Just sentenceAlias
    retract _ = Nothing

instance Injection (Sentence patternType) SentenceSymbol where
    inject = SentenceSymbolSentence

    retract (SentenceSymbolSentence sentenceSymbol) = Just sentenceSymbol
    retract _ = Nothing

instance Injection (Sentence patternType) SentenceImport where
    inject = SentenceImportSentence

    retract (SentenceImportSentence sentenceImport) = Just sentenceImport
    retract _ = Nothing

instance Injection (Sentence patternType) (SentenceAxiom patternType) where
    inject = SentenceAxiomSentence

    retract (SentenceAxiomSentence sentenceAxiom) = Just sentenceAxiom
    retract _ = Nothing

instance Injection (Sentence patternType) (SentenceClaim patternType) where
    inject = SentenceClaimSentence

    retract (SentenceClaimSentence sentenceClaim) = Just sentenceClaim
    retract _ = Nothing

instance Injection (Sentence patternType) SentenceSort where
    inject = SentenceSortSentence

    retract (SentenceSortSentence sentenceSort) = Just sentenceSort
    retract _ = Nothing

instance Injection (Sentence patternType) SentenceHook where
    inject = SentenceHookSentence

    retract (SentenceHookSentence sentenceHook) = Just sentenceHook
    retract _ = Nothing

projectSentenceImport :: Sentence ParsedPattern -> Maybe SentenceImport
projectSentenceImport = retract

projectSentenceSort :: Sentence ParsedPattern -> Maybe SentenceSort
projectSentenceSort = retract

projectSentenceSymbol :: Sentence ParsedPattern -> Maybe SentenceSymbol
projectSentenceSymbol = retract

projectSentenceHook :: Sentence ParsedPattern -> Maybe SentenceHook
projectSentenceHook = retract

projectSentenceHookedSort :: Sentence ParsedPattern -> Maybe SentenceSort
projectSentenceHookedSort = projectSentenceHook Monad.>=> projectTyped

projectSentenceHookedSymbol :: Sentence ParsedPattern -> Maybe SentenceSymbol
projectSentenceHookedSymbol = projectSentenceHook Monad.>=> projectTyped

projectSentenceAlias ::
    Sentence ParsedPattern ->
    Maybe (SentenceAlias ParsedPattern)
projectSentenceAlias = retract

projectSentenceAxiom ::
    Sentence ParsedPattern ->
    Maybe (SentenceAxiom ParsedPattern)
projectSentenceAxiom = retract

projectSentenceClaim ::
    Sentence ParsedPattern ->
    Maybe (SentenceClaim ParsedPattern)
projectSentenceClaim = retract

{- | The attributes associated with a sentence.

Every sentence type has attributes, so this operation is total.
-}
sentenceAttributes :: Sentence patternType -> Attributes
sentenceAttributes =
    \case
        SentenceAliasSentence
            SentenceAlias{sentenceAliasAttributes} ->
                sentenceAliasAttributes
        SentenceSymbolSentence
            SentenceSymbol{sentenceSymbolAttributes} ->
                sentenceSymbolAttributes
        SentenceImportSentence
            SentenceImport{sentenceImportAttributes} ->
                sentenceImportAttributes
        SentenceAxiomSentence
            SentenceAxiom{sentenceAxiomAttributes} ->
                sentenceAxiomAttributes
        SentenceClaimSentence
            (SentenceClaim SentenceAxiom{sentenceAxiomAttributes}) ->
                sentenceAxiomAttributes
        SentenceSortSentence
            SentenceSort{sentenceSortAttributes} ->
                sentenceSortAttributes
        SentenceHookSentence sentence ->
            case sentence of
                SentenceHookedSort
                    SentenceSort{sentenceSortAttributes} ->
                        sentenceSortAttributes
                SentenceHookedSymbol
                    SentenceSymbol{sentenceSymbolAttributes} ->
                        sentenceSymbolAttributes

-- | Erase the pattern annotations within a 'Sentence'.
eraseSentenceAnnotations ::
    Sentence (Pattern variable erased) ->
    Sentence (Pattern variable Attribute.Null)
eraseSentenceAnnotations sentence = (<$) Attribute.Null <$> sentence

asSentence ::
    forall input patternType.
    Injection (Sentence patternType) input =>
    input ->
    Sentence patternType
asSentence = inject

-- | 'SentenceSymbolOrAlias' corresponds to the @symbol-or-alias@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>.
class SentenceSymbolOrAlias (sentence :: Type) where
    getSentenceSymbolOrAliasConstructor ::
        sentence -> Id
    getSentenceSymbolOrAliasSortParams ::
        sentence -> [SortVariable]
    getSentenceSymbolOrAliasArgumentSorts ::
        sentence -> [Sort]
    getSentenceSymbolOrAliasResultSort ::
        sentence -> Sort
    getSentenceSymbolOrAliasAttributes ::
        sentence -> Attributes
    getSentenceSymbolOrAliasSentenceName ::
        sentence -> String
    getSentenceSymbolOrAliasHead ::
        sentence ->
        [Sort] ->
        SymbolOrAlias
    getSentenceSymbolOrAliasHead sentence sortParameters =
        SymbolOrAlias
            { symbolOrAliasConstructor =
                getSentenceSymbolOrAliasConstructor sentence
            , symbolOrAliasParams = sortParameters
            }

instance SentenceSymbolOrAlias (SentenceAlias patternType) where
    getSentenceSymbolOrAliasConstructor = aliasConstructor . sentenceAliasAlias
    getSentenceSymbolOrAliasSortParams = aliasParams . sentenceAliasAlias
    getSentenceSymbolOrAliasArgumentSorts = sentenceAliasSorts
    getSentenceSymbolOrAliasResultSort = sentenceAliasResultSort
    getSentenceSymbolOrAliasAttributes = sentenceAliasAttributes
    getSentenceSymbolOrAliasSentenceName _ = "alias"

instance SentenceSymbolOrAlias SentenceSymbol where
    getSentenceSymbolOrAliasConstructor =
        symbolConstructor . sentenceSymbolSymbol
    getSentenceSymbolOrAliasSortParams = symbolParams . sentenceSymbolSymbol
    getSentenceSymbolOrAliasArgumentSorts = sentenceSymbolSorts
    getSentenceSymbolOrAliasResultSort = sentenceSymbolResultSort
    getSentenceSymbolOrAliasAttributes = sentenceSymbolAttributes
    getSentenceSymbolOrAliasSentenceName _ = "symbol"

-- | 'PureSentenceAxiom' is the pure (fixed-@level@) version of 'SentenceAxiom'
type PureSentenceAxiom = SentenceAxiom ParsedPattern

-- | 'PureSentenceAlias' is the pure (fixed-@level@) version of 'SentenceAlias'
type PureSentenceAlias = SentenceAlias ParsedPattern

-- | 'PureSentenceSymbol' is the pure (fixed-@level@) version of 'SentenceSymbol'
type PureSentenceSymbol = SentenceSymbol

-- | 'PureSentenceImport' is the pure (fixed-@level@) version of 'SentenceImport'
type PureSentenceImport = SentenceImport

-- | 'PureSentenceHook' is the pure (fixed-@level@) version of 'SentenceHook'.
type PureSentenceHook = SentenceHook

-- | 'PureSentence' is the pure (fixed-@level@) version of 'Sentence'
type PureSentence = Sentence ParsedPattern

-- | 'PureModule' is the pure (fixed-@level@) version of 'Module'
type PureModule = Module PureSentence

type ParsedSentenceSort = SentenceSort

type ParsedSentenceSymbol = SentenceSymbol

type ParsedSentenceAlias = SentenceAlias ParsedPattern

type ParsedSentenceImport = SentenceImport

type ParsedSentenceAxiom = SentenceAxiom ParsedPattern

type ParsedSentenceHook = SentenceHook

type ParsedSentence = Sentence ParsedPattern

type ParsedModule = Module ParsedSentence
