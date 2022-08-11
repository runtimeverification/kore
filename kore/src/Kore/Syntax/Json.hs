{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2022
License     : BSD-3-Clause
-}
module Kore.Syntax.Json (
    -- API
    KoreJson (..),
    KorePattern,
    JsonParseError,
    encodePattern,
    encodeKoreJson,
    decodePattern,
    decodeKoreJson,
    toParsedPattern,
    fromPattern,
    fromTermLike,
    fromPredicate,
) where

import Data.Aeson as Json
import Data.Aeson.Encode.Pretty as Json
import Data.ByteString.Lazy (ByteString)
import Data.Either.Extra hiding (Left, Right)
import Kore.Attribute.Attributes (ParsedPattern)
import Kore.Internal.Predicate (Predicate)
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.TermLike qualified as TermLike
import Kore.Internal.TermLike.TermLike ()
import Kore.Syntax qualified as Kore
import Kore.Syntax.Json.Internal
import Kore.Syntax.Variable (VariableName (..))
import Prelude.Kore

-- reading

{- | read text into KorePattern, then check for consistency and
 construct a ParsedPattern
-}
decodePattern :: ByteString -> Either JsonParseError ParsedPattern
decodePattern bs =
    toParsedPattern . term <$> mapLeft JsonParseError (decodeKoreJson bs)

-- | low-level: read text into KorePattern (wrapped in KoreJson)
decodeKoreJson :: ByteString -> Either String KoreJson
decodeKoreJson = Json.eitherDecode'

-- | Errors relating to the json codec
newtype JsonParseError
    = JsonParseError String
    deriving stock (Eq, Show)

------------------------------------------------------------
-- writing

-- | Write a Pattern to a json byte string.
encodePattern :: Kore.Pattern VariableName ann -> ByteString
encodePattern = encodeKoreJson . addHeader . fromPattern

addHeader :: KorePattern -> KoreJson
addHeader = KoreJson KORE KJ1

encodeKoreJson :: KoreJson -> ByteString
encodeKoreJson = Json.encodePretty' prettyJsonOpts

prettyJsonOpts :: Json.Config
prettyJsonOpts =
    defConfig
        { confIndent = Spaces 2
        , confCompare =
            keyOrder -- retains the field order in all constructors
                [ "format"
                , "version"
                , "term"
                , "tag"
                , "assoc"
                , "name"
                , "symbol"
                , "argSort"
                , "sort"
                , "sorts"
                , "var"
                , "varSort"
                , "arg"
                , "args"
                , "argss"
                , "source"
                , "dest"
                , "value"
                ]
        }

------------------------------------------------------------
-- convenience converters

fromTermLike :: TermLike.TermLike VariableName -> KoreJson
fromTermLike =
    addHeader
        . fromPattern
        . from @_ @(Kore.Pattern _ (TermLike.TermAttributes VariableName))

fromPredicate :: Kore.Sort -> Predicate VariableName -> KoreJson
fromPredicate s = fromTermLike . Predicate.fromPredicate s
