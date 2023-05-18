{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause

The simplification attribute identifies axioms that are useful for
simplifying configurations, without being part of the main semantics.

Kore syntax: @simplification{}()@

Informal example of an axiom that would use the simplification attribute:

(x +Int y) +Int z = (x +Int z) +Int y
    if concrete(x) and concrete(z) and not concrete(y)
-}
module Kore.Attribute.Simplification (
    Simplification (..),
    simplificationId,
    simplificationSymbol,
    simplificationAttribute,
    defaultSimplificationPriority,
) where

import Data.Binary (Binary)
import Data.Maybe (
    maybeToList,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

type SimplificationPriority = Maybe Integer

{- | @Simplification@ represents the @simplification@ attribute for axioms.
    It takes an optional integer argument which represents the rule's priority.
    This allows the possibility of ordering the application of simplification rules.
-}
data Simplification
    = IsSimplification !SimplificationPriority
    | NotSimplification
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Simplification where
    def = NotSimplification

-- | Kore identifier representing the @simplification@ attribute symbol.
simplificationId :: Id
simplificationId = "simplification"

-- | Kore symbol representing the @simplification@ attribute.
simplificationSymbol :: SymbolOrAlias
simplificationSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = simplificationId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @simplification@ attribute.
simplificationAttribute :: Maybe Integer -> AttributePattern
simplificationAttribute priority =
    attributePattern
        simplificationSymbol
        (fmap attributeInteger (maybeToList priority))

defaultSimplificationPriority :: Integer
defaultSimplificationPriority = 50

instance ParseAttributes Simplification where
    parseAttribute =
        withApplication' parseSimplification
      where
        parseSimplification params args NotSimplification = do
            Parser.getZeroParams params
            arg <- Parser.getZeroOrOneArguments args
            case arg of
                Just arg' ->
                    Parser.getStringLiteral arg'
                        >>= Parser.parseStringLiteral readPriority
                Nothing -> pure (IsSimplification Nothing)
        parseSimplification _ _ _ =
            failDuplicate'

        readPriority str
            | null str = pure (IsSimplification Nothing, "")
            | otherwise = do
                (integer, rest) <- reads str
                pure (IsSimplification (Just integer), rest)

        withApplication' = Parser.withApplication simplificationId
        failDuplicate' = Parser.failDuplicate simplificationId

instance From Simplification Attributes where
    from NotSimplification = def
    from (IsSimplification maybePriority) =
        from @AttributePattern (simplificationAttribute maybePriority)
