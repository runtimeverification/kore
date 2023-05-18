{- |
Module      : Kore.Attribute.Smtlib.Smtlib
Description : SMT-HOOK translation attribute
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : traian.serbanuta@runtimeverification.com
-}
module Kore.Attribute.Smtlib.Smthook (
    Smthook (..),
    SExpr (..),
    smthookId,
    smthookSymbol,
    smthookAttribute,
) where

import Data.Binary (Binary)
import Data.Text (
    Text,
 )
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser
import Kore.Debug
import Prelude.Kore
import SMT.SimpleSMT (
    SExpr (..),
    showSExpr,
 )

{- | The @smthook@ attribute for symbols.

The @smthook@ attribute allows a Kore symbol and its arguments to be translated
for an external SMT solver. @smthook@ is meant to be used similarly to how
@smtlib@ is used, with the exception that it is meant for encoding into
builtin operations provided by the SMT solver and, as such, the symbol
used for encoding needs not be declared.

See 'Kore.Attribute.Smtlib.Smtlib'
-}
newtype Smthook = Smthook {getSmthook :: Maybe SExpr}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Smthook where
    def = Smthook Nothing

-- | Kore identifier representing the @smthook@ attribute symbol.
smthookId :: Id
smthookId = "smt-hook"

-- | Kore symbol representing the @smthook@ attribute.
smthookSymbol :: SymbolOrAlias
smthookSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = smthookId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @smthook@ attribute.
smthookAttribute :: Text -> AttributePattern
smthookAttribute syntax =
    attributePattern smthookSymbol [attributeString syntax]

instance ParseAttributes Smthook where
    parseAttribute =
        withApplication' $ \params args Smthook{getSmthook} -> do
            getZeroParams params
            arg <- getOneArgument args
            StringLiteral syntax <- getStringLiteral arg
            sExpr <- parseSExpr syntax
            unless (isNothing getSmthook) failDuplicate'
            return Smthook{getSmthook = Just sExpr}
      where
        withApplication' = withApplication smthookId
        failDuplicate' = failDuplicate smthookId

instance From Smthook Attributes where
    from =
        maybe def toAttribute . getSmthook
      where
        toAttribute =
            from @AttributePattern . smthookAttribute . Text.pack . showSExpr
