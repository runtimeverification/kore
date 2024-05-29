{- |
Module      : Kore.Rewrite.SMT.Declaration
Description : Declares sorts and symbols to the SMT solver.
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com,
              david.cox@runtimeverification.com
-}
module Kore.Rewrite.SMT.Declaration (
    declareSortsSymbols,
) where

import Data.Map.Strict (elems)
import Kore.Rewrite.SMT.AST (
    Declarations (Declarations, sorts, symbols),
    KoreSortDeclaration (
        SortDeclarationDataType,
        SortDeclarationSort,
        SortDeclaredIndirectly
    ),
    KoreSymbolDeclaration (
        SymbolBuiltin,
        SymbolConstructor,
        SymbolDeclaredDirectly
    ),
    SmtDeclarations,
    Sort (sortDeclaration),
    Symbol (symbolDeclaration),
 )
import Prelude.Kore
import SMT (
    MonadSMT,
    -- declareDatatypes,
    declareFun_,
    declareSort,
 )

-- | Sends all symbols in the given declarations to the SMT.
declareSymbols :: MonadSMT m => SmtDeclarations -> m ()
declareSymbols = traverse_ (declareSymbol . symbolDeclaration) . symbols
  where
    declareSymbol = \case
        SymbolDeclaredDirectly decl -> declareFun_ decl
        SymbolBuiltin _ -> return ()
        SymbolConstructor _ -> return ()

-- | Sends all sorts in the given declarations to the SMT.
declareSorts :: MonadSMT m => SmtDeclarations -> m ()
declareSorts Declarations{sorts} = do
    let (sortDecls, _dataTypeDecls) =
            elems sorts
                & mapMaybe (eitherDecl . sortDeclaration)
                & partitionEithers
    traverse_ declareSort sortDecls
    -- declareDatatypes dataTypeDecls
  where
    eitherDecl = \case
        SortDeclarationSort decl -> Just $ Left decl
        SortDeclarationDataType decl -> Just $ Right decl
        SortDeclaredIndirectly _ -> Nothing

-- | Sends all given declarations to the SMT
declareSortsSymbols :: MonadSMT m => SmtDeclarations -> m ()
declareSortsSymbols decls = declareSorts decls >> declareSymbols decls
