{- |
Module      : Kore.Attribute.Location
Description : Line/column location attribute
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : vladimir.ciobanu@runtimeverification.com
-}
module Kore.Attribute.Location (
    Location (..),
    LineColumn (..),
) where

import Data.Binary (Binary)
import Data.Text qualified as Text
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as AttributeParser
import Kore.Debug
import Kore.Error qualified
import Prelude.Kore
import Text.Megaparsec (
    Parsec,
    parseMaybe,
 )
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (
    decimal,
 )

data LineColumn = LineColumn
    { line :: !Int
    , column :: !Int
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

data Location = Location
    { start :: Maybe LineColumn
    , end :: Maybe LineColumn
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Location where
    def = Location Nothing Nothing

-- | Kore identifier representing the @location@ attribute symbol.
locationId :: Id
locationId = "org'Stop'kframework'Stop'attributes'Stop'Location"

instance ParseAttributes Location where
    parseAttribute = AttributeParser.withApplication locationId parseApplication
      where
        parseApplication ::
            [Sort] ->
            [AttributePattern] ->
            Location ->
            AttributeParser.Parser Location
        parseApplication params args l@(Location Nothing Nothing) = do
            AttributeParser.getZeroParams params
            case args of
                [] -> pure l
                [_] -> do
                    arg <- AttributeParser.getOneArgument args
                    StringLiteral str <- AttributeParser.getStringLiteral arg
                    pure
                        . fromMaybe def
                        . parseMaybe locationParser
                        $ Text.unpack str
                _ ->
                    Kore.Error.koreFail
                        ("expected one argument, found " ++ show (length args))
        parseApplication _ _ _ =
            AttributeParser.failDuplicate locationId

instance From Location Attributes where
    -- TODO (thomas.tuegel): Implement
    from = def

{- | This parser is used to parse the inner representation of the attribute.
 The expected format is "Location(sl,sc,el,ec)" where sc, sc, el, and ec are
 all numbers.
-}
type StringParser = Parsec String String

locationParser :: StringParser Location
locationParser =
    Location
        <$> (Just <$> parseStart)
        <*> (Just <$> parseEnd)
  where
    parseStart :: StringParser LineColumn
    parseStart =
        LineColumn
            <$> (string "Location(" *> decimal)
            <*> (string "," *> decimal)

    parseEnd :: StringParser LineColumn
    parseEnd =
        LineColumn
            <$> (string "," *> decimal)
            <*> (string "," *> decimal <* ")")
