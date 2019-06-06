{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

-}

module Kore.Internal.Symbol
    ( Symbol (..)
    , toSymbolOrAlias
    , isNonSimplifiable
    , isConstructor
    , isSortInjection
    , isFunctional
    , isFunction
    , isTotal
    , isInjective
    , symbolHook
    ) where

import           Control.DeepSeq
import qualified Data.Function as Function
import           Data.Hashable
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import qualified Kore.Attribute.Symbol.Symbol as Attribute
import           Kore.Debug
import           Kore.Sort
import           Kore.Syntax.Application
                 ( SymbolOrAlias (..) )
import           Kore.Unparser

data Symbol =
    Symbol
        { symbolConstructor :: !Id
        , symbolParams      :: ![Sort]
        , symbolAttributes  :: !Attribute.Symbol
        }
    deriving (GHC.Generic, Show)

instance Eq Symbol where
    (==) a b =
            Function.on (==) symbolConstructor a b
        &&  Function.on (==) symbolParams a b
    {-# INLINE (==) #-}

instance Ord Symbol where
    compare a b =
            Function.on compare symbolConstructor a b
        <>  Function.on compare symbolParams a b

instance Hashable Symbol where
    hashWithSalt salt Symbol { symbolConstructor, symbolParams } =
        salt `hashWithSalt` symbolConstructor `hashWithSalt` symbolParams

instance NFData Symbol

instance SOP.Generic Symbol

instance SOP.HasDatatypeInfo Symbol

instance Debug Symbol

instance Unparse Symbol where
    unparse Symbol { symbolConstructor, symbolParams } =
        unparse symbolConstructor
        <> parameters symbolParams

    unparse2 Symbol { symbolConstructor } =
        unparse2 symbolConstructor

toSymbolOrAlias :: Symbol -> SymbolOrAlias
toSymbolOrAlias symbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = symbolConstructor symbol
        , symbolOrAliasParams = symbolParams symbol
        }

-- | Is a symbol not simplifiable?
--
-- sigma is non-simplifiable if whenever we have the following
-- * Context[y] is not simplifiable to a pattern without y
-- * sigma(..., x, ...) != bottom
-- then Context[sigma(..., x, ...)] cannot be simplified to either x or
-- something that does not contain x as a free variable.
--
-- Note that constructors and sort injection are natural candidates for
-- non-simplifiable patterns. Builtins like 'element' (for sets, lists and maps)
-- are also good candidates for non-simplifiable symbols.
--
-- Builtins like 'concat' need an additional condition, i.e. that the arguments
-- are not .Map.
isNonSimplifiable :: Symbol -> Bool
isNonSimplifiable = Attribute.isNonSimplifiable . symbolAttributes

isConstructor :: Symbol -> Bool
isConstructor =
    Attribute.isConstructor . Attribute.constructor . symbolAttributes

isSortInjection :: Symbol -> Bool
isSortInjection =
    Attribute.isSortInjection . Attribute.sortInjection . symbolAttributes

isInjective :: Symbol -> Bool
isInjective = Attribute.isInjective . symbolAttributes

isFunctional :: Symbol -> Bool
isFunctional = Attribute.isFunctional . symbolAttributes

isFunction :: Symbol -> Bool
isFunction = Attribute.isFunction . symbolAttributes

isTotal :: Symbol -> Bool
isTotal = Attribute.isTotal . symbolAttributes

symbolHook :: Symbol -> Attribute.Hook
symbolHook = Attribute.hook . symbolAttributes
