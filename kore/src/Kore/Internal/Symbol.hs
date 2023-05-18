{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Internal.Symbol (
    Symbol (..),
    toSymbolOrAlias,
    isConstructorLike,
    isConstructor,
    isSortInjection,
    isTotal,
    isFunction,
    isDeclaredFunction,
    isNotBottom,
    isInjective,
    isMemo,
    isAnywhere,
    noEvaluators,
    symbolHook,
    constructor,
    total,
    function,
    injective,
    sortInjection,
    smthook,
    hook,
    klabel,
    symbolKywd,
    coerceSortInjection,

    -- * Re-exports
    module Kore.Internal.ApplicationSorts,
) where

import Control.DeepSeq (
    deepseq,
 )
import Control.Lens qualified as Lens
import Data.Binary (Binary)
import Data.Generics.Product
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.AST.AstWithLocation
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Internal.ApplicationSorts
import Kore.Sort
import Kore.Syntax.Application
import Kore.Unparser
import Prelude.Kore
import Pretty qualified
import SMT.AST (
    SExpr,
 )
import SQL qualified

data Symbol = Symbol
    { symbolConstructor :: !Id
    , symbolParams :: ![Sort]
    , symbolSorts :: !ApplicationSorts
    , symbolAttributes :: !Attribute.Symbol
    }
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Eq Symbol where
    (==) a b =
        on (==) symbolConstructor a b
            && on (==) symbolParams a b
    {-# INLINE (==) #-}

instance Ord Symbol where
    compare a b =
        on compare symbolConstructor a b
            <> on compare symbolParams a b

instance Hashable Symbol where
    hashWithSalt salt Symbol{symbolConstructor, symbolParams} =
        salt `hashWithSalt` symbolConstructor `hashWithSalt` symbolParams

instance Unparse Symbol where
    unparse Symbol{symbolConstructor, symbolParams} =
        unparse symbolConstructor <> parameters symbolParams

    unparse2 Symbol{symbolConstructor} =
        unparse2 symbolConstructor

instance From Symbol SymbolOrAlias where
    from = toSymbolOrAlias

instance
    Ord variable =>
    Synthetic (FreeVariables variable) (Application Symbol)
    where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Sort (Application Symbol) where
    synthetic application =
        resultSort & deepseq (matchSorts operandSorts children)
      where
        Application{applicationSymbolOrAlias = symbol} = application
        Application{applicationChildren = children} = application
        Symbol{symbolSorts} = symbol
        resultSort = applicationSortsResult symbolSorts
        operandSorts = applicationSortsOperands symbolSorts

instance SQL.Column Symbol where
    defineColumn = SQL.defineTextColumn
    toColumn = SQL.toColumn . Pretty.renderText . Pretty.layoutOneLine . unparse

instance AstWithLocation Symbol where
    locationFromAst = locationFromAst . symbolConstructor

toSymbolOrAlias :: Symbol -> SymbolOrAlias
toSymbolOrAlias symbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = symbolConstructor symbol
        , symbolOrAliasParams = symbolParams symbol
        }

{- | Is a symbol constructor-like?

 A symbol @sigma@ is constructor-like if whenever we have the following
 * Context[y] is not simplifiable to a pattern without y
 * sigma(..., x, ...) != bottom
 then Context[sigma(..., x, ...)] cannot be simplified to either x or
 something that does not contain x as a free variable.

 Note that constructors and sort injection are natural candidates for
 constructor-like patterns. Builtins like 'element' (for sets, lists and maps)
 are also good candidates for constructor-like symbols.

 Builtins like 'concat' need an additional condition, i.e. that the arguments
 are not .Map.
-}
isConstructorLike :: Symbol -> Bool
isConstructorLike = Attribute.isConstructorLike . symbolAttributes

isConstructor :: Symbol -> Bool
isConstructor =
    Attribute.isConstructor . Attribute.constructor . symbolAttributes

isSortInjection :: Symbol -> Bool
isSortInjection =
    Attribute.isSortInjection . Attribute.sortInjection . symbolAttributes

isInjective :: Symbol -> Bool
isInjective = Attribute.isInjective . symbolAttributes

isTotal :: Symbol -> Bool
isTotal = Attribute.isTotal . symbolAttributes

isFunction :: Symbol -> Bool
isFunction = Attribute.isFunction . symbolAttributes

isDeclaredFunction :: Symbol -> Bool
isDeclaredFunction =
    Attribute.isDeclaredFunction . Attribute.function . symbolAttributes

isNotBottom :: Symbol -> Bool
isNotBottom = Attribute.isNotBottom . symbolAttributes

isMemo :: Symbol -> Bool
isMemo = Attribute.isMemo . Attribute.memo . symbolAttributes

isAnywhere :: Symbol -> Bool
isAnywhere = Attribute.isAnywhere . Attribute.anywhere . symbolAttributes

noEvaluators :: Symbol -> Bool
noEvaluators =
    Attribute.hasNoEvaluators . Attribute.noEvaluators . symbolAttributes

symbolHook :: Symbol -> Attribute.Hook
symbolHook = Attribute.hook . symbolAttributes

constructor :: Symbol -> Symbol
constructor =
    Lens.set
        (typed @Attribute.Symbol . typed @Attribute.Constructor)
        Attribute.Constructor{isConstructor = True}

total :: Symbol -> Symbol
total =
    Lens.set
        (typed @Attribute.Symbol . typed @Attribute.Total)
        Attribute.Total{isDeclaredTotal = True}

function :: Symbol -> Symbol
function =
    Lens.set
        (typed @Attribute.Symbol . typed @Attribute.Function)
        Attribute.Function{isDeclaredFunction = True}

injective :: Symbol -> Symbol
injective =
    Lens.set
        (typed @Attribute.Symbol . typed @Attribute.Injective)
        Attribute.Injective{isDeclaredInjective = True}

sortInjection :: Symbol -> Symbol
sortInjection =
    Lens.set
        (typed @Attribute.Symbol . typed @Attribute.SortInjection)
        Attribute.SortInjection{isSortInjection = True}

smthook :: SExpr -> Symbol -> Symbol
smthook sExpr =
    Lens.set
        (typed @Attribute.Symbol . typed @Attribute.Smthook)
        Attribute.Smthook{getSmthook = Just sExpr}

hook :: Text -> Symbol -> Symbol
hook name =
    Lens.set
        (typed @Attribute.Symbol . typed @Attribute.Hook)
        Attribute.Hook{getHook = Just name}

klabel :: Text -> Symbol -> Symbol
klabel name =
    Lens.set
        (typed @Attribute.Symbol . typed @Attribute.Klabel)
        Attribute.Klabel{getKlabel = Just name}

symbolKywd :: Symbol -> Symbol
symbolKywd =
    Lens.set
        (typed @Attribute.Symbol . typed @Attribute.SymbolKywd)
        Attribute.SymbolKywd{isSymbolKywd = True}

{- | Coerce a sort injection symbol's source and target sorts.

Use @coerceSortInjection@ to update the internal representation of a sort
injection 'Symbol' when evaluating or simplifying sort injections.
-}
coerceSortInjection ::
    -- | Original sort injection symbol
    Symbol ->
    -- | New source sort
    Sort ->
    -- | New target sort
    Sort ->
    Symbol
coerceSortInjection injectionSymbol sourceSort targetSort =
    injectionSymbol
        { symbolParams = [sourceSort, targetSort]
        , symbolSorts = applicationSorts [sourceSort] targetSort
        }
