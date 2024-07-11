{-# LANGUAGE PatternSynonyms #-}

{- | Pretty printer for JSON KORE terms

Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Main (
    main,
) where

import Booster.Pattern.Pretty
import Booster.Prettyprinter (renderDefault)
import Booster.Syntax.Json (KoreJson (..))
import Booster.Syntax.Json.Internalise (
    InternalisedPredicates (..),
    PatternError (..),
    internalisePattern,
    internalisePredicates,
    pattern CheckSubsorts,
    pattern DisallowAlias,
 )
import Booster.Syntax.ParsedKore (internalise, parseKoreDefinition)
import Control.Monad.Trans.Except
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BS
import Data.Text.IO qualified as Text
import Prettyprinter
import System.Environment (getArgs)

main :: IO ()
main = do
    [def, json] <- getArgs
    parsedDef <-
        either (error . renderDefault . pretty) id . parseKoreDefinition def <$> Text.readFile def
    let internalDef = either (error . renderDefault . pretty' @'[Decoded]) id $ internalise Nothing parsedDef

    fileContent <- BS.readFile json
    case eitherDecode fileContent of
        Left err -> putStrLn $ "Error: " ++ err
        Right KoreJson{term} -> do
            case runExcept $ internalisePattern DisallowAlias CheckSubsorts Nothing internalDef term of
                Right (trm, _subst, _unsupported) -> do
                    putStrLn "Pretty-printing pattern: "
                    putStrLn $ renderDefault $ pretty' @'[Decoded] trm
                Left (NoTermFound _) ->
                    case runExcept $ internalisePredicates DisallowAlias CheckSubsorts Nothing internalDef [term] of
                        Left es -> error (show es)
                        Right ts -> do
                            putStrLn "Pretty-printing predicates: "
                            putStrLn "Bool predicates: "
                            mapM_ (putStrLn . renderDefault . pretty' @'[Decoded]) ts.boolPredicates
                            putStrLn "Ceil predicates: "
                            mapM_ (putStrLn . renderDefault . pretty' @'[Decoded]) ts.ceilPredicates
                            putStrLn "Substitution: "
                            mapM_ (putStrLn . renderDefault . pretty' @'[Decoded]) ts.substitution
                            putStrLn "Unsupported predicates: "
                            mapM_ print ts.unsupported
                Left err -> error (show err)
