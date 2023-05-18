{- |
Module      : Data.Attribute.Hook
Description : Representation and parser for hook attributes
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.Hook (
    Hook (..),
    emptyHook,
    hookId,
    hookSymbol,
    hookAttribute,
    getHookAttribute,
) where

import Data.Binary (Binary)
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Kore.Error
import Prelude.Kore

newtype Hook = Hook {getHook :: Maybe Text}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Hook where
    def = emptyHook

{- | Parse the @hook@ Kore attribute, if present.

  It is a parse error if the @hook@ attribute is not given exactly one literal
  string argument.

  See also: 'hookAttribute'
-}
instance ParseAttributes Hook where
    parseAttribute =
        withApplication' $ \params args (Hook hook) -> do
            getZeroParams params
            arg <- getOneArgument args
            StringLiteral name <- getStringLiteral arg
            unless (isNothing hook) failDuplicate'
            return Hook{getHook = Just name}
      where
        withApplication' = withApplication hookId
        failDuplicate' = failDuplicate hookId

instance From Hook Attributes where
    from = maybe def (from @AttributePattern . hookAttribute) . getHook

-- | The missing @hook@ attribute.
emptyHook :: Hook
emptyHook = Hook Nothing

-- | Kore identifier representing a @hook@ attribute symbol.
hookId :: Id
hookId = "hook"

{- | Kore symbol representing the head of a @hook@ attribute.

Kore syntax: @hook{}@
-}
hookSymbol :: SymbolOrAlias
hookSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = hookId
        , symbolOrAliasParams = []
        }

{- | Kore pattern representing a @hook@ attribute

Kore syntax: @hook{}("HOOKED.function")@
@"HOOKED.function"@ is a literal string referring to a known builtin
function.
-}
hookAttribute ::
    -- | hooked function name
    Text ->
    AttributePattern
hookAttribute builtin = attributePattern hookSymbol [attributeString builtin]

{- | Look up a required @hook{}()@ attribute from the given attributes.

    It is an error if the attribute is missing.
-}
getHookAttribute ::
    MonadError (Error e) m =>
    Attributes ->
    m Text
getHookAttribute attributes = do
    let parser = Parser.parseAttributes attributes
    hook <- Parser.liftParser parser
    maybe (koreFail "missing hook attribute") return (getHook hook)
