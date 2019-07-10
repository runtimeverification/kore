{- |
Module      : Kore.Builtin.MapSymbols
Description : Tools for handling the symbols involved with map biltins
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.MapSymbols as Map
@
-}

module Kore.Builtin.MapSymbols
    ( -- * Symbols
      lookupSymbolUpdate
    , lookupSymbolLookup
    , lookupSymbolInKeys
    , lookupSymbolKeys
    , lookupSymbolRemove
    , lookupSymbolRemoveAll
    , isSymbolConcat
    , isSymbolElement
    , isSymbolUnit
    , isSymbolRemove
    , isSymbolRemoveAll
      -- * keys
    , concatKey
    , elementKey
    , in_keysKey
    , keysKey
    , lookupKey
    , removeAllKey
    , removeKey
    , unitKey
    , updateKey
    ) where

import           Data.String
                 ( IsString )
import           Kore.Attribute.Hook
                 ( Hook )
import qualified Kore.Attribute.Symbol as Attribute
                 ( Symbol )
import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Error as Kore
                 ( Error )
import           Kore.IndexedModule.IndexedModule
                 ( VerifiedModule )
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import           Kore.Internal.Symbol
                 ( Symbol )
import           Kore.Sort
                 ( Sort )

concatKey :: IsString s => s
concatKey = "MAP.concat"

lookupKey :: IsString s => s
lookupKey = "MAP.lookup"

elementKey :: IsString s => s
elementKey = "MAP.element"

unitKey :: IsString s => s
unitKey = "MAP.unit"

updateKey :: IsString s => s
updateKey = "MAP.update"

in_keysKey :: IsString s => s
in_keysKey = "MAP.in_keys"

keysKey :: IsString s => s
keysKey = "MAP.keys"

removeKey :: IsString s => s
removeKey = "MAP.remove"

removeAllKey :: IsString s => s
removeAllKey = "MAP.removeAll"

{- | Find the symbol hooked to @MAP.update@ in an indexed module.
 -}
lookupSymbolUpdate
    :: Sort
    -> VerifiedModule Attribute.Symbol axiomAttrs
    -> Either (Kore.Error e) Symbol
lookupSymbolUpdate = Builtin.lookupSymbol updateKey

{- | Find the symbol hooked to @MAP.lookup@ in an indexed module.
 -}
lookupSymbolLookup
    :: Sort
    -> VerifiedModule Attribute.Symbol axiomAttrs
    -> Either (Kore.Error e) Symbol
lookupSymbolLookup = Builtin.lookupSymbol lookupKey

{- | Find the symbol hooked to @MAP.in_keys@ in an indexed module.
 -}
lookupSymbolInKeys
    :: Sort
    -> VerifiedModule Attribute.Symbol axiomAttrs
    -> Either (Kore.Error e) Symbol
lookupSymbolInKeys = Builtin.lookupSymbol in_keysKey

{- | Find the symbol hooked to @MAP.keys@ in an indexed module.
 -}
lookupSymbolKeys
    :: Sort
    -> VerifiedModule Attribute.Symbol axiomAttrs
    -> Either (Kore.Error e) Symbol
lookupSymbolKeys = Builtin.lookupSymbol keysKey

{- | Find the symbol hooked to @MAP.remove@ in an indexed module.
 -}
lookupSymbolRemove
    :: Sort
    -> VerifiedModule Attribute.Symbol axiomAttrs
    -> Either (Kore.Error e) Symbol
lookupSymbolRemove = Builtin.lookupSymbol removeKey

{- | Find the symbol hooked to @MAP.removeAll@ in an indexed module.
 -}
lookupSymbolRemoveAll
    :: Sort
    -> VerifiedModule Attribute.Symbol axiomAttrs
    -> Either (Kore.Error e) Symbol
lookupSymbolRemoveAll = Builtin.lookupSymbol removeAllKey

{- | Check if the given symbol is hooked to @MAP.concat@.
 -}
isSymbolConcat
    :: SmtMetadataTools Hook
    -> Symbol
    -> Bool
isSymbolConcat = Builtin.isSymbol concatKey

{- | Check if the given symbol is hooked to @MAP.element@.
 -}
isSymbolElement
    :: SmtMetadataTools Hook
    -> Symbol
    -> Bool
isSymbolElement = Builtin.isSymbol elementKey

{- | Check if the given symbol is hooked to @MAP.unit@.
-}
isSymbolUnit
    :: SmtMetadataTools Hook
    -> Symbol
    -> Bool
isSymbolUnit = Builtin.isSymbol unitKey

{- | Check if the given symbol is hooked to @MAP.remove@.
-}
isSymbolRemove
    :: SmtMetadataTools Hook
    -> Symbol
    -> Bool
isSymbolRemove = Builtin.isSymbol removeKey

{- | Check if the given symbol is hooked to @MAP.removeAll@.
-}
isSymbolRemoveAll
    :: SmtMetadataTools Hook
    -> Symbol
    -> Bool
isSymbolRemoveAll = Builtin.isSymbol removeAllKey
