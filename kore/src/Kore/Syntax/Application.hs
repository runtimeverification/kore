{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause

This module includes all the data structures necessary for representing
the syntactic categories of a Kore definition that do not need unified
constructs.

Unified constructs are those that represent both meta and object versions of
an AST term in a single data type (e.g. 'SymbolOrAlias' that can be either
'Kore.Syntax.Sentence.Symbol' or 'Kore.Syntax.Sentence.Alias')

Please refer to
<http://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md kore-syntax.md>.
-}
module Kore.Syntax.Application (
    SymbolOrAlias (..),
    Application (..),
    mapHead,
) where

import Data.Bifunctor (Bifunctor (..))
import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Debug
import Kore.Sort
import Kore.TopBottom
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

{- | 'SymbolOrAlias' corresponds to the @symbol-or-alias@ syntactic category from
<https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#sentences kore-syntax.md#sentences>
-}
data SymbolOrAlias = SymbolOrAlias
    { symbolOrAliasConstructor :: !Id
    , symbolOrAliasParams :: ![Sort]
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse SymbolOrAlias where
    unparse
        SymbolOrAlias
            { symbolOrAliasConstructor
            , symbolOrAliasParams
            } =
            unparse symbolOrAliasConstructor <> parameters symbolOrAliasParams
    unparse2 SymbolOrAlias{symbolOrAliasConstructor} =
        Pretty.parens $ Pretty.fillSep [unparse2 symbolOrAliasConstructor]

instance From SymbolOrAlias SymbolOrAlias where
    from = id

{- | 'Application' corresponds to the @application-pattern@ branch of the
@pattern@ syntactic category from
<https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#patterns kore-syntax.md#patterns>.

This represents the @σ(φ1, ..., φn)@ symbol patterns in Matching Logic.
-}
data Application head child = Application
    { applicationSymbolOrAlias :: !head
    , applicationChildren :: ![child]
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Bifunctor Application where
    bimap f g (Application alias children) =
        Application (f alias) (fmap g children)

instance (Debug head, Debug child) => Debug (Application head child)

instance
    ( Debug head
    , Diff head
    , Debug child
    , Diff child
    ) =>
    Diff (Application head child)

instance (Unparse head, Unparse child) => Unparse (Application head child) where
    unparse Application{applicationSymbolOrAlias, applicationChildren} =
        unparse applicationSymbolOrAlias <> arguments applicationChildren

    unparse2 Application{applicationSymbolOrAlias, applicationChildren} =
        case applicationChildren of
            [] ->
                Pretty.parens (unparse2 applicationSymbolOrAlias)
            children ->
                Pretty.parens
                    ( Pretty.fillSep
                        [ unparse2 applicationSymbolOrAlias
                        , arguments2 children
                        ]
                    )

instance TopBottom child => TopBottom (Application head child) where
    isTop _ = False
    isBottom = any isBottom

mapHead ::
    (head1 -> head2) ->
    Application head1 child ->
    Application head2 child
mapHead mapping app =
    app{applicationSymbolOrAlias = mapping (applicationSymbolOrAlias app)}
