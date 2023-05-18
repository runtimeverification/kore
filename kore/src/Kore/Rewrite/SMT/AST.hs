{- |
Module      : Kore.Rewrite.SMT.AST
Description : Data types involved in declaring and using SMT symbols.
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
-}
module Kore.Rewrite.SMT.AST (
    Declarations (..),
    Encodable (..),
    IndirectSymbolDeclaration (..),
    KoreSortDeclaration (..),
    KoreSymbolDeclaration (..),
    SmtDeclarations,
    SmtKoreSymbolDeclaration,
    SmtSort,
    SmtSymbol,
    Sort (..),
    SortReference (..),
    SortSExprSpec (..),
    Symbol (..),
    SymbolReference (..),
    UnresolvedConstructor,
    UnresolvedConstructorArgument,
    UnresolvedDataTypeDeclaration,
    UnresolvedDeclarations,
    UnresolvedFunctionDeclaration,
    UnresolvedIndirectSymbolDeclaration,
    UnresolvedKoreSortDeclaration,
    UnresolvedKoreSymbolDeclaration,
    UnresolvedSort,
    UnresolvedSortDeclaration,
    UnresolvedSymbol,
    encodable,
    encode,
    mergePreferFirst,
    symbolSmtFromSortArgs,
) where

import Data.Binary (Binary)
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Smtlib.Smthook (
    SExpr (..),
 )
import Kore.Debug
import Kore.Rewrite.SMT.Encoder (
    encodeName,
 )
import Kore.Sort qualified as Kore (
    Sort,
 )
import Kore.Syntax.Id qualified as Kore (
    Id,
    getId,
    pattern Id,
 )
import Prelude.Kore
import SMT.AST qualified as AST

{- | A representation of the Kore Sort type together with its related
declarations (constructors, noJunk axioms), optimized for dealing with the SMT.

The SmtSort type below instantiates Sort with the types used by the SMT.
-}
data Sort sort symbol name = Sort
    { sortData ::
        SortSExprSpec
    -- ^ Produces the SMT representation of a sort. Given a map with
    -- Smt representations for sorts and a list of sort arguments, returns
    -- an S-expression that can be used, say, when declaring symbols of
    -- that sort.
    , sortDeclaration :: !(KoreSortDeclaration sort symbol name)
    -- ^ Information needed for declaring the sort, also listing all
    -- dependencies on other sorts and symbols.
    }
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance
    (Show sort, Show symbol, Show name) =>
    Show (Sort sort symbol name)
    where
    show s@(Sort _ _) =
        case s of
            Sort{sortDeclaration} ->
                "Sort { sortSmtFromSortArgs, sortDeclaration = "
                    ++ show sortDeclaration
                    ++ "}"

{- | A defunctionalized data type containing the information needed to invoke
sortSmtFromSortArgs, which constructs an S-expression of a sort from its sort
arguments.
-}
data SortSExprSpec
    = EmptySortArgsToSmt AST.SExpr
    | ApplyToArgs AST.SExpr
    | ConstSExpr AST.SExpr
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

instance Diff SortSExprSpec where
    diffPrec = diffPrecIgnore

{- | A representation of the Kore SymbolOrAlias type together with symbol
declaration sentences, optimized for dealing with the SMT.

The SmtSymbol type below instantiates Symbol with the types used by the SMT.
-}
data Symbol sort name = Symbol
    { symbolData :: AST.SExpr
    -- ^ Produces the SMT representation of a symbol. Given a map with
    -- Smt representations for sorts and a list of sort arguments, returns
    -- an s-expression that can be used, say, when building assertions
    -- using that symbol.
    , symbolDeclaration :: !(KoreSymbolDeclaration sort name)
    -- ^ Information needed for declaring the symbol, also listing all
    -- dependencies on other sorts and symbols.
    }
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

symbolSmtFromSortArgs :: AST.SExpr -> Map Kore.Id SmtSort -> [Kore.Sort] -> Maybe AST.SExpr
symbolSmtFromSortArgs astName = const . const $ Just astName

instance (Show sort, Show name) => Show (Symbol sort name) where
    show s@(Symbol _ _) =
        case s of
            Symbol{symbolDeclaration} ->
                "Symbol { symbolSmtFromSortArgs, symbolDeclaration = "
                    ++ show symbolDeclaration
                    ++ "}"

{- | Data needed for declaring an SMT sort.

The SmtKoreSortDeclaration type below instantiates Sort with the types used
by the SMT.
-}
data KoreSortDeclaration sort symbol name
    = -- | Constructor-based sort. Assumed to declare its own constructors.
      SortDeclarationDataType !(AST.DataTypeDeclaration sort symbol name)
    | -- | Non-constructor sort.
      SortDeclarationSort !(AST.SortDeclaration name)
    | -- | Sort that we don't need to declare (e.g. builtins like Int) so we just
      -- represent that the SMT already knows about it.
      SortDeclaredIndirectly !name
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

{- | Data needed for declaring an SMT symbol.

The SmtKoreSymbolDeclaration type below instantiates Symbol with the types used
by the SMT.
-}
data KoreSymbolDeclaration sort name
    = -- | Normal symbol declaration
      SymbolDeclaredDirectly !(AST.FunctionDeclaration sort name)
    | -- | Builtins should not be declared to the SMT.
      -- The IndirectSymbolDeclaration value holds dependencies on other sorts
      -- and debug information.
      SymbolBuiltin !(IndirectSymbolDeclaration sort name)
    | -- | Constructors are declared to the SMT when declaring the sort.
      -- The IndirectSymbolDeclaration value holds dependencies on other sorts
      -- and debug information.
      SymbolConstructor !(IndirectSymbolDeclaration sort name)
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- | Holds the sorts on which an already declared symbol depends.
data IndirectSymbolDeclaration sort name = IndirectSymbolDeclaration
    { name :: !name
    , sortDependencies :: ![sort]
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance (Debug name, Debug sort) => Debug (IndirectSymbolDeclaration sort name)

instance
    ( Debug name
    , Diff name
    , Debug sort
    , Diff sort
    ) =>
    Diff (IndirectSymbolDeclaration sort name)

{- | Holds things that we declare to an SMT. When encountered in its
SmtDeclarations instantiation, we usually assume that all dependencies between
the various declarations can be resolved.
-}
data Declarations sort symbol name = Declarations
    { sorts :: Map Kore.Id (Sort sort symbol name)
    , symbols :: Map Kore.Id (Symbol sort name)
    }
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- | Marks a dependency on a given sort.
newtype SortReference = SortReference {getSortReference :: Kore.Sort}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- | Marks a dependency on a given symbol.
newtype SymbolReference = SymbolReference {getSymbolReference :: Kore.Id}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

{- | Data that should be encoded before being used with the SMT.

Use @AlreadyEncoded@ and @encodable@ to create it,
and @encode@ to extract its data
-}
data Encodable
    = AlreadyEncoded !SExpr
    | Encodable !SExpr
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- Type instantiations to be used by the SMT.
type SmtDeclarations = Declarations AST.SExpr Text AST.SExpr
type SmtKoreSymbolDeclaration = KoreSymbolDeclaration AST.SExpr AST.SExpr
type SmtSort = Sort AST.SExpr Text AST.SExpr
type SmtSymbol = Symbol AST.SExpr AST.SExpr

-- Type instantiations with unresolved dependencies, produced directly from the
-- input module.
type UnresolvedConstructorArgument =
    AST.ConstructorArgument SortReference Encodable
type UnresolvedConstructor =
    AST.Constructor SortReference SymbolReference Encodable
type UnresolvedDataTypeDeclaration =
    AST.DataTypeDeclaration SortReference SymbolReference Encodable
type UnresolvedDeclarations =
    Declarations SortReference SymbolReference Encodable
type UnresolvedFunctionDeclaration =
    AST.FunctionDeclaration SortReference Encodable
type UnresolvedIndirectSymbolDeclaration =
    IndirectSymbolDeclaration SortReference Encodable
type UnresolvedKoreSortDeclaration =
    KoreSortDeclaration SortReference SymbolReference Encodable
type UnresolvedKoreSymbolDeclaration =
    KoreSymbolDeclaration SortReference Encodable
type UnresolvedSort =
    Sort SortReference SymbolReference Encodable
type UnresolvedSortDeclaration =
    AST.SortDeclaration Encodable
type UnresolvedSymbol =
    Symbol SortReference Encodable

encodable :: Kore.Id -> Encodable
encodable Kore.Id{getId} = Encodable (Atom getId)

encode :: Encodable -> SExpr
encode (AlreadyEncoded e) = e
encode (Encodable e) = AST.mapSExpr encodeName e

mergePreferFirst ::
    Declarations sort symbol name ->
    Declarations sort symbol name ->
    Declarations sort symbol name
mergePreferFirst
    Declarations{sorts = sorts1, symbols = symbols1}
    Declarations{sorts = sorts2, symbols = symbols2} =
        Declarations
            { sorts = sorts1 `Map.union` sorts2
            , symbols = symbols1 `Map.union` symbols2
            }
