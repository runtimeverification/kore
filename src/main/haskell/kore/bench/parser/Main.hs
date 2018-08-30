module Main where

import Criterion.Main

import Data.Proxy
       ( Proxy (Proxy) )
import System.FilePath
       ( takeFileName )

import           Kore.ASTVerifier.DefinitionVerifier
                 ( defaultAttributesVerification, verifyDefinition )
import qualified Kore.Builtin as Builtin
import           Kore.Parser.Parser
                 ( fromKore )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )

import qualified Paths

main :: IO ()
main =
    defaultMain
    [ bgroup "Parse" (map parse koreFiles)
    , bgroup "Read and parse" (map readAndParse koreFiles)
      -- 'kore.kore' cannot be verified
    , bgroup "Verify" (map verify (tail koreFiles))
    ]

{- | List of Kore files

The benchmarks in this module test parsing the following list of files.
-}
koreFiles :: [FilePath]
koreFiles =
    map Paths.dataFileName
    [ "../../kore/kore.kore"
    , "../../../test/resources/bool.kore"
    , "../../../test/resources/imp.kore"
    , "../../../test/resources/imp2.kore"
    , "../../../test/resources/lambda.kore"
    , "../../../test/resources/list.kore"
    , "../../../test/resources/nat.kore"
    , "../../../test/resources/user-meta-nat.kore"
    ]

{- | Declare a parser benchmark

The benchmark will parse the contents of the file. The file is read only once
before the benchmark is run because Criterion may repeat a benchmark many times
to gather timing statistics.
-}
parse
    :: FilePath  -- ^ name of file to parse
    -> Benchmark
parse filename =
    env (readFile filename)  -- Read Kore definition once before benchmark
    (bench name . nf (fromKore filename))  -- Benchmark parsing step only
  where
    name = takeFileName filename


{- | Declare a parser benchmark

The benchmark will parse the contents of the file. This benchmark includes the
overhead of reading the file, in contrast to 'parse' above.
-}
readAndParse
    :: FilePath  -- ^ name of file to parse
    -> Benchmark
readAndParse filename =
    bench name $ nfIO (fromKore filename <$> readFile filename)
  where
    name = takeFileName filename

{- | Declare a verifier benchmark

The benchmark will verify the contents of the file. The file is read and parsed
only once before the benchmark is run because Criterion may repeat a benchmark
many times to gather timing statistics.
-}
verify
    :: FilePath
    -> Benchmark
verify filename =
    env parse1 (bench name . nf verify1)
  where
    name = takeFileName filename
    -- | Read and parse the file once before the benchmark
    parse1 = do
        parsed <- fromKore filename <$> readFile filename
        case parsed of
            Left err -> error err
            Right defn -> pure defn
    -- | Verify the Kore definition.
    -- Runs once per benchmark iteration.
    verify1 defn =
        case
            verifyDefinition
                attributesVerification
                Builtin.koreBuiltins
                defn
          of
            Left err -> error (show err)
            Right _ -> ()
      where
        attributesVerification =
            defaultAttributesVerification
            (Proxy @StepperAttributes)
