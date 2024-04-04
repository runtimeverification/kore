module Test.Kore.Syntax.Json.Roundtrips (
    test_ParserKoreFiles,
    makeGold,
    patternToText,
) where

import Control.Monad (filterM)
import Data.Aeson qualified as Json
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Text (Text)
import Data.Text.IO qualified as T
import Kore.Parser (
    ParsedDefinition,
    ParsedPattern,
    parseKoreDefinition,
 )
import Kore.Syntax qualified as Kore
import Kore.Syntax.Definition qualified as Kore
import Kore.Syntax.Json
import Kore.Unparser (unparse)
import Prelude.Kore
import Pretty qualified
import System.FilePath
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden
import Test.Tasty.HUnit (testCase, (@?=))

-- | extract a pattern from a textual kore file (containing a module)
patternsFrom :: FilePath -> IO [ParsedPattern]
patternsFrom path = do
    contents <- T.readFile path
    pure $
        either (const []) getPatterns $
            parseKoreDefinition path contents

getPatterns :: ParsedDefinition -> [ParsedPattern]
getPatterns =
    concatMap (mapMaybe fromSentence . Kore.moduleSentences) . Kore.definitionModules

fromSentence :: Kore.Sentence ParsedPattern -> Maybe ParsedPattern
fromSentence (Kore.SentenceAliasSentence alias) =
    Just $ Kore.sentenceAliasRightPattern alias
fromSentence (Kore.SentenceAxiomSentence axiom) =
    Just $ Kore.sentenceAxiomPattern axiom
fromSentence (Kore.SentenceClaimSentence claim) =
    Just . Kore.sentenceAxiomPattern $ Kore.getSentenceClaim claim
fromSentence _ = Nothing

-- | pretty-print a pattern to textual form
patternToText :: Kore.Pattern Kore.VariableName ann -> Text
patternToText =
    Pretty.renderText
        . Pretty.layoutPretty Pretty.defaultLayoutOptions
        . unparse

-- | render a list of json bytestrings into a json array
jsonArray :: [BS.ByteString] -> BS.ByteString
jsonArray bss =
    BS.unwords
        [ BS.pack "[\n"
        , BS.intercalate ",\n" bss
        , BS.pack "\n]"
        ]

----------------------------------------

{- | Constructs a golden test from textual kore to json

 First file is the "golden" json output, second file the textual
 kore input. The input file should contain patterns that can be
 extracted using @patternsFrom@, otherwise the result is empty.
-}
toJsonGolden :: FilePath -> FilePath -> TestTree
toJsonGolden jsonFile koreFile =
    goldenVsString testName jsonFile encodeFile
  where
    testName = unwords ["Converting", takeFileName koreFile, "to JSON"]
    encodeFile = do
        ps <- patternsFrom koreFile
        pure . jsonArray $ map encodePattern ps

----------------------------------------
-- set up tests for all golden json files

jsonLocation, koreLocation :: FilePath
jsonLocation = foldl1 (</>) ["..", "test", "parser", "json"]
koreLocation = takeDirectory jsonLocation

-- repl helper to produce golden files for all usable kore files
makeGold :: IO ()
makeGold = do
    files <- findByExtension [".kore"] koreLocation
    goodFiles <- filterM (fmap (not . null) . patternsFrom) files
    putStrLn $ "Good files: " <> show goodFiles
    forM_ goodFiles $ \file -> do
        putStr $ takeBaseName file
        ps <- patternsFrom file
        putStr "..."
        let json = jsonArray $ map encodePattern ps
            target = jsonLocation </> takeBaseName file <.> "json"
        BS.writeFile target json
        putStrLn "...done"

{- | parse json file to KorePattern, parse/convert textual to
 KorePattern, compare
-}
comparePatternsFrom :: FilePath -> TestTree
comparePatternsFrom jsonFile =
    testCase ("Comparing results from " <> baseName) $
        do
            fromJson <- parseJson jsonFile
            fromKore <- map fromPattern <$> patternsFrom koreFile
            fromJson @?= fromKore
  where
    baseName = takeBaseName jsonFile
    koreFile = koreLocation </> baseName <.> "kore"
    parseJson =
        fmap (either (error . show) (map term))
            . Json.eitherDecodeFileStrict @[KoreJson]

test_ParserKoreFiles :: IO TestTree
test_ParserKoreFiles = do
    jsonFiles <- findByExtension [".json"] jsonLocation
    pure $
        testGroup
            "JSON <-> textual kore conversion tests"
            [ testGroup
                ("Encode to golden JSON files in " <> jsonLocation)
                [ toJsonGolden json (koreLocation </> takeBaseName json <.> "kore")
                | json <- jsonFiles
                ]
            , testGroup
                ("Patterns from JSON files in " <> jsonLocation)
                [ comparePatternsFrom json
                | json <- jsonFiles
                ]
            ]
