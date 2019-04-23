{-|
Module      : Kore.Parser.Parser
Description : Parser for the Kore language
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX

This is a parser for the Kore language. Sample usage:

@
import Kore.Parser.Parser

import           Kore.Parser.ParserUtils (parseOnly)
import           System.Environment (getArgs)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    print (parseKoreDefinition contents)
    -- or --
    print (parse koreParser fileName contents)
    -- or --
    print (parseOnly koreParser fileName contents)
@

-}
module Kore.Parser.Parser
    ( parseKoreDefinition
    , parseKorePattern
    , koreParser
    , korePatternParser
    , CommonKorePattern
    ) where

import           Kore.AST.Kore
import           Kore.AST.Sentence
import           Kore.Parser.Lexeme
                 ( skipWhitespace )
import qualified Kore.Parser.ParserImpl as KoreParser
import           Kore.Parser.ParserUtils

{-|'koreParser' is a parser for Kore.
Data.Kore.AST.Kore
The input must contain a full valid Kore defininition and nothing else.
-}
koreParser :: Parser ParsedDefinition
koreParser = skipWhitespace *> KoreParser.koreDefinitionParser <* endOfInput

{-|'korePatternParser' is a parser for Kore patterns.

The input must contain a full valid Kore pattern and nothing else.
-}
korePatternParser :: Parser CommonKorePattern
korePatternParser = KoreParser.korePatternParser

{- | Parse a string representing a Kore definition.

@parseKoreDefinition@ returns a 'KoreDefinition' upon success, or an parse error
message otherwise. The input must contain a valid Kore definition and nothing
else.

 -}
parseKoreDefinition
    :: FilePath  -- ^ Filename used for error messages
    -> String  -- ^ The concrete syntax of a valid Kore definition
    -> Either String ParsedDefinition
parseKoreDefinition = parseOnly koreParser

{- | Parse a string representing a Kore pattern.

@parseKorePattern@ returns a 'CommonKorePattern' upon success, or an parse error
message otherwise. The input must contain a valid Kore pattern and nothing else.

 -}
parseKorePattern
    :: FilePath  -- ^ Filename used for error messages
    -> String  -- ^ The concrete syntax of a valid Kore pattern
    -> Either String CommonKorePattern
parseKorePattern = parseOnly korePatternParser
