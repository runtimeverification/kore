{- | Stand-alone parser executable for testing and profiling

Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Main (
    main,
) where
import Data.Aeson (ToJSON(toJSON), decode)
import Data.Aeson.Encode.Pretty
import Types
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment (getArgs)
import Data.List (foldl')
import Control.Monad (foldM, forM_)
import Data.Text (unpack)
import Data.Map qualified as Map


{- | Tests textual kore parser with given arguments and reports
   internalisation results.

   * Files given as arguments are parsed and internalised.  When a
   * directory is given as an argument, it is (recursively) searched
   * for files named "*.kore", which are parsed and internalised.
-}
main :: IO ()
main = getArgs >>= \case
  ["tree", file] -> do
    nested <- foldl' (foldl' toNested) (Nested mempty) . map decode . BS.lines <$> BS.readFile file
    BS.putStrLn $ encodePretty' defConfig{confIndent = Spaces 2} $ toJSON nested
  "aborts":files -> do
    let countContexts m f = foldl' (foldl' countAborts) m . map ((context <$>) . decode) . BS.lines <$> BS.readFile f
    counts <- foldM countContexts mempty files
    forM_ (Map.toList counts) $ \(k, v) -> putStrLn $ unpack k <> " " <> show v
  _ -> error "invalid option"


    

