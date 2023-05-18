{- |
Module      : Kore.Attribute.Source
Description : Source file attribute
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : vladimir.ciobanu@runtimeverification.com
-}
module Kore.Attribute.Source (
    Source (..),
) where

import Data.Binary (Binary)
import Data.Default
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as AttributeParser
import Kore.Debug
import Kore.Error qualified
import Prelude.Kore
import Text.Megaparsec (
    Parsec,
    noneOf,
    parseMaybe,
 )
import Text.Megaparsec.Char

newtype Source = Source {unSource :: Maybe String}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Source where
    def = Source Nothing

-- | Kore identifier representing the @location@ attribute symbol.
sourceId :: Id
sourceId = "org'Stop'kframework'Stop'attributes'Stop'Source"

instance ParseAttributes Source where
    parseAttribute = AttributeParser.withApplication sourceId parseApplication
      where
        parseApplication ::
            [Sort] ->
            [AttributePattern] ->
            Source ->
            AttributeParser.Parser Source
        parseApplication params args s@(Source Nothing) = do
            AttributeParser.getZeroParams params
            case args of
                [] -> pure s
                [_] -> do
                    arg <- AttributeParser.getOneArgument args
                    StringLiteral str <- AttributeParser.getStringLiteral arg
                    pure
                        . fromMaybe def
                        . parseMaybe sourceParser
                        $ Text.unpack str
                _ ->
                    Kore.Error.koreFail
                        ("expected one argument, found " ++ show (length args))
        parseApplication _ _ _ =
            AttributeParser.failDuplicate sourceId

instance From Source Attributes where
    -- TODO (thomas.tuegel): Implement
    from _ = def

{- | This parser is used to parse the inner representation of the attribute.
 The expected format is "Source(path)" where path is a FilePath.
-}
type StringParser = Parsec String String

sourceParser :: StringParser Source
sourceParser = Source <$> (Just <$> sourceParser0)
  where
    sourceParser0 :: StringParser String
    sourceParser0 =
        string "Source(" *> many (noneOf excludedChars) <* string ")"

    excludedChars :: String
    excludedChars = ")"
