{- |
Description : Symbol declaration attributes
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
This module is intended to be imported qualified:
@
import Kore.Attribute.Symbol qualified as Attribute
@
-}
module Kore.Attribute.Symbol (
    Symbol (..),
    StepperAttributes,
    defaultSymbolAttributes,

    -- * Function symbols
    Function (..),
    functionAttribute,

    -- * Total symbols
    Total (..),
    totalAttribute,

    -- * Constructor symbols
    Constructor (..),
    constructorAttribute,

    -- * Injective symbols
    Injective (..),
    injectiveAttribute,

    -- * Anywhere symbols
    Anywhere (..),
    anywhereAttribute,

    -- * Sort injection symbols
    SortInjection (..),
    sortInjectionAttribute,

    -- * Hooked symbols
    Hook (..),
    hookAttribute,

    -- * SMT symbols
    Smthook (..),
    smthookAttribute,
    Smtlib (..),
    smtlibAttribute,

    -- * Memoized functions
    Memo (..),
    memoAttribute,

    -- * K labels
    Klabel (..),
    klabelAttribute,

    -- * Symbols
    SymbolKywd (..),
    symbolKywdAttribute,

    -- * Functions with no evaluators
    NoEvaluators (..),
    noEvaluatorsAttribute,

    -- * Macro symbols
    Macro (..),
    macroAttribute,

    -- * Alias symbols
    AliasKywd (..),
    aliasKywdAttribute,

    -- * Derived attributes
    isConstructorLike,
    isTotal,
    isFunction,
    isNotBottom,
    isInjective,
) where

import Control.Lens qualified as Lens
import Control.Monad (
    (>=>),
 )
import Data.Binary (Binary)
import Data.Default
import Data.Generics.Product
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Constructor
import Kore.Attribute.Function
import Kore.Attribute.Hook
import Kore.Attribute.Injective
import Kore.Attribute.Parser (
    Attributes,
    ParseAttributes (..),
 )
import Kore.Attribute.Smthook
import Kore.Attribute.Smtlib
import Kore.Attribute.SortInjection
import Kore.Attribute.SourceLocation
import Kore.Attribute.Symbol.AliasKywd
import Kore.Attribute.Symbol.Anywhere
import Kore.Attribute.Symbol.Klabel
import Kore.Attribute.Symbol.Macro
import Kore.Attribute.Symbol.Memo
import Kore.Attribute.Symbol.NoEvaluators
import Kore.Attribute.Symbol.SymbolKywd
import Kore.Attribute.Total
import Kore.Debug
import Prelude.Kore

{- | Symbol attributes used during Kore execution.

@Symbol@ records the declared attributes of a Kore symbol, but the effective
attributes can be different; for example, constructors and sort injections are
injective, even if their declaration is not given the @injective@ attribute. To
view the effective attributes, use the functions defined in this module.
-}
data Symbol = Symbol
    { function :: !Function
    -- ^ Whether a symbol represents a function
    , total :: !Total
    -- ^ Whether a symbol is total
    , constructor :: !Constructor
    -- ^ Whether a symbol represents a constructor
    , injective :: !Injective
    -- ^ Whether a symbol represents an injective function
    , sortInjection :: !SortInjection
    -- ^ Whether a symbol is a sort injection
    , anywhere :: !Anywhere
    , hook :: !Hook
    -- ^ The builtin sort or symbol hooked to a sort or symbol
    , smtlib :: !Smtlib
    , smthook :: !Smthook
    , memo :: !Memo
    , klabel :: !Klabel
    , symbolKywd :: !SymbolKywd
    , noEvaluators :: !NoEvaluators
    , sourceLocation :: !SourceLocation
    -- ^ Location in the original (source) file.
    , macro :: !Macro
    , aliasKywd :: !AliasKywd
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Debug Symbol where
    debugPrecBrief _ _ = "_"

instance Diff Symbol

instance ParseAttributes Symbol where
    parseAttribute attr =
        typed @Function (parseAttribute attr)
            >=> typed @Total (parseAttribute attr)
            >=> typed @Constructor (parseAttribute attr)
            >=> typed @SortInjection (parseAttribute attr)
            >=> typed @Injective (parseAttribute attr)
            >=> typed @Anywhere (parseAttribute attr)
            >=> typed @Hook (parseAttribute attr)
            >=> typed @Smtlib (parseAttribute attr)
            >=> typed @Smthook (parseAttribute attr)
            >=> typed @Memo (parseAttribute attr)
            >=> typed @Klabel (parseAttribute attr)
            >=> typed @SymbolKywd (parseAttribute attr)
            >=> typed @NoEvaluators (parseAttribute attr)
            >=> typed @SourceLocation (parseAttribute attr)
            >=> typed @Macro (parseAttribute attr)
            >=> typed @AliasKywd (parseAttribute attr)

instance From Symbol Attributes where
    from =
        mconcat
            . sequence
                [ from . function
                , from . total
                , from . constructor
                , from . injective
                , from . sortInjection
                , from . anywhere
                , from . hook
                , from . smtlib
                , from . smthook
                , from . memo
                , from . klabel
                , from . symbolKywd
                , from . noEvaluators
                , from . sourceLocation
                , from . macro
                , from . aliasKywd
                ]

type StepperAttributes = Symbol

defaultSymbolAttributes :: Symbol
defaultSymbolAttributes =
    Symbol
        { function = def
        , total = def
        , constructor = def
        , injective = def
        , sortInjection = def
        , anywhere = def
        , hook = def
        , smtlib = def
        , smthook = def
        , memo = def
        , klabel = def
        , symbolKywd = def
        , noEvaluators = def
        , sourceLocation = def
        , macro = def
        , aliasKywd = def
        }

-- | See also: 'defaultSymbolAttributes'
instance Default Symbol where
    def = defaultSymbolAttributes

-- | Is a symbol constructor-like?
isConstructorLike :: StepperAttributes -> Bool
isConstructorLike = do
    Constructor isConstructor' <- constructor
    SortInjection isSortInjection' <- sortInjection
    return (isSortInjection' || isConstructor')

{- | Is the symbol a function?

A symbol is a function if it is given the @function@ attribute or if it is
total.

See also: 'functionAttribute', 'isTotal'
-}
isFunction :: StepperAttributes -> Bool
isFunction = do
    Function isFunction' <- Lens.view typed
    isTotal' <- isTotal
    return (isFunction' || isTotal')

{- | Is the symbol total?

A symbol is total if it is given the @total@ attribute or the
@sortInjection@ attribute.

See also: 'totalAttribute', 'sortInjectionAttribute'
-}
isTotal :: StepperAttributes -> Bool
isTotal = do
    Total total' <- total
    SortInjection isSortInjection' <- sortInjection
    return (total' || isSortInjection')

-- | Is a symbol not bottom (non-@\\bottom@)?
isNotBottom :: StepperAttributes -> Bool
isNotBottom = do
    isTotal' <- isTotal
    -- TODO (thomas.tuegel): Constructors are not total.
    Constructor isConstructor' <- Lens.view typed
    return (isTotal' || isConstructor')

{- | Is the symbol injective?

A symbol is injective if it is given the @injective@ attribute, the
@constructor@ attribute, or the @sortInjection@ attribute.

See also: 'injectiveAttribute', 'constructorAttribute', 'sortInjectionAttribute'
-}
isInjective :: StepperAttributes -> Bool
isInjective =
    or
        . sequence
            [ isDeclaredInjective . injective
            , isConstructor . constructor
            , isSortInjection . sortInjection
            ]
