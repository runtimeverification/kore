{- |
Module      : Kore.Attribute.Smtlib.Smtlib
Description : SMT-LIB translation attribute
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.Smtlib.Smtlib (
    Smtlib (..),
    smtlibId,
    smtlibSymbol,
    smtlibAttribute,
) where

import Data.Binary (Binary)
import Data.Default (
    Default (..),
 )
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Attributes
import Kore.Debug
import Kore.Syntax.Application (
    SymbolOrAlias (..),
 )
import Kore.Syntax.Id (
    Id,
 )
import Prelude.Kore
import SMT.SimpleSMT (
    SExpr,
    showSExpr,
 )

{- | The @smtlib@ attribute for symbols.

The @smtlib@ attribute allows a Kore symbol and its arguments to be translated
for an external SMT solver.

Kore syntax: @smtlib{}(\"≪S-expression≫\")@, where @≪S-expression≫@ is an
S-expression defined by the SMT-LIB 2 standard. If @≪S-expression≫@ is an atom,
that atom is used as the head of a list expression and all the arguments of the
hooked Kore symbol are passed as arguments of the list. If @≪S-expression≫@ is a
list, that list is passed to the SMT solver; in this case, the special
meta-variable symbols @#≪n≫@ (where @≪n≫@ is a positive integer) are replaced by
the positional parameters of the hooked Kore symbol. Note that the
meta-variable symbols are only valid in the @smtlib@ attribute; they are /not/
valid SMT-LIB S-expressions.
-}
newtype Smtlib = Smtlib {getSmtlib :: Maybe SExpr}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Smtlib where
    def = Smtlib Nothing

instance From Smtlib Attributes where
    from =
        Attributes
            . maybe [] ((: []) . smtlibAttribute . Text.pack . showSExpr)
            . getSmtlib

-- | Kore identifier representing the @smtlib@ attribute symbol.
smtlibId :: Id
smtlibId = "smtlib"

-- | Kore symbol representing the @smtlib@ attribute.
smtlibSymbol :: SymbolOrAlias
smtlibSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = smtlibId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @smtlib@ attribute.
smtlibAttribute :: Text -> AttributePattern
smtlibAttribute syntax =
    attributePattern smtlibSymbol [attributeString syntax]
