{-|
Module      : Kore.Attribute.Simplification
Description : Function simplification axiom attribute
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com


The simplification attribute identifies axioms that are useful for
simplifying configurations, without being part of the main semantics.

Kore syntax: @simplification{}()@

Informal example of an axiom that would use the simplification attribute:

(x +Int y) +Int z = (x +Int z) +Int y
    if concrete(x) and concrete(z) and not concrete(y)
-}
module Kore.Attribute.Simplification
    ( Simplification (..)
    , simplificationId, simplificationSymbol, simplificationAttribute
    ) where

import Prelude.Kore

import Data.Maybe
    ( maybeToList
    )
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Parser as Parser
import Kore.Debug

type SimplificationPriority = Maybe Integer

-- TODO: update docs
{- | @Simplification@ represents the @simplification@ attribute for axioms.
 -}
data Simplification
    = IsSimplification SimplificationPriority
    | NotSimplification
    deriving (Eq, Ord, Show, GHC.Generic)

instance SOP.Generic Simplification

instance SOP.HasDatatypeInfo Simplification

instance Debug Simplification

instance Diff Simplification

instance NFData Simplification

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
    attributePattern simplificationSymbol (fmap attributeInteger (maybeToList priority))

instance ParseAttributes Simplification where
    parseAttribute =
        withApplication' $ \params args simplification ->
            case simplification of
                NotSimplification -> do
                    Parser.getZeroParams params
                    arg <- Parser.getZeroOrOneArguments args
                    case arg of
                        Just arg' -> do
                            stringLiteral <- Parser.getStringLiteral arg'
                            integer <- Parser.parseInteger stringLiteral
                            return (IsSimplification (Just integer))
                        Nothing ->
                            return (IsSimplification Nothing)
                _ -> failDuplicate'
      where
        withApplication' = Parser.withApplication simplificationId
        failDuplicate' = Parser.failDuplicate simplificationId

instance From Simplification Attributes where
    from NotSimplification = def
    from (IsSimplification maybePriority) =
        from @AttributePattern (simplificationAttribute maybePriority)
