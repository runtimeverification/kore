{-# LANGUAGE FlexibleContexts #-}

{- | Utilities for (internalised) definitions and other things

Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Definition.Util (
    -- * summarising a definition
    Summary (..),
    mkSummary,
    prettySummary,
    sourceRefText,
) where

import Control.DeepSeq (NFData (..))
import Data.Bifunctor
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List.Extra (sortOn)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter (Doc, Pretty, pretty, (<+>))
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.String qualified as Pretty (renderString)

import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.Definition.Ceil (ComputeCeilSummary (..))
import Booster.Pattern.Base
import Booster.Pattern.Index (CellIndex (..), TermIndex (..))
import Booster.Pattern.Pretty
import Booster.Prettyprinter
import Booster.Util

data Summary = Summary
    { file :: FilePath
    , modNames, sortNames, symbolNames :: [ByteString]
    , subSorts :: Map.Map ByteString [ByteString]
    , axiomCount, preserveDefinednessCount, containAcSymbolsCount, partialSymbolsCount :: Int
    , functionRuleCount, simplificationCount, predicateSimplificationCount :: Int
    , rewriteRules :: Map.Map TermIndex [SourceRef]
    , functionRules :: Map.Map TermIndex [SourceRef]
    , simplifications :: Map.Map TermIndex [SourceRef]
    , ceils :: Map.Map TermIndex [RewriteRule "Ceil"]
    , computeCeilsSummary :: [ComputeCeilSummary]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

mkSummary :: FilePath -> KoreDefinition -> [ComputeCeilSummary] -> Summary
mkSummary file def computeCeilsSummary =
    Summary
        { file
        , modNames = Map.keys def.modules
        , sortNames = Map.keys def.sorts
        , symbolNames = Map.keys def.symbols
        , subSorts = Map.map (Set.toList . snd) def.sorts
        , axiomCount = length $ concat $ concatMap Map.elems (Map.elems def.rewriteTheory)
        , preserveDefinednessCount =
            length $
                filter (\rule -> null rule.computedAttributes.notPreservesDefinednessReasons) $
                    concat $
                        concatMap Map.elems (Map.elems def.rewriteTheory)
        , containAcSymbolsCount =
            length $
                filter (\rule -> rule.computedAttributes.containsAcSymbols) $
                    concat $
                        concatMap Map.elems (Map.elems def.rewriteTheory)
        , partialSymbolsCount =
            length $
                filter (\sym -> sym.attributes.symbolType == Function Partial) $
                    Map.elems def.symbols
        , functionRuleCount =
            length $ concat $ concatMap Map.elems (Map.elems def.functionEquations)
        , simplificationCount =
            length $ concat $ concatMap Map.elems (Map.elems def.functionEquations)
        , predicateSimplificationCount =
            length $ concat $ concatMap Map.elems (Map.elems def.functionEquations)
        , rewriteRules = Map.map sourceRefs def.rewriteTheory
        , functionRules = Map.map sourceRefs def.functionEquations
        , simplifications = Map.map sourceRefs def.simplifications
        , ceils = Map.map (concat . Map.elems) def.ceils
        , computeCeilsSummary
        }
  where
    sourceRefs :: HasSourceRef x => Map.Map k [x] -> [SourceRef]
    sourceRefs = map sourceRef . concat . Map.elems

prettySummary :: Bool -> Summary -> String
prettySummary veryVerbose summary@Summary{computeCeilsSummary} =
    Pretty.renderString . layoutPrettyUnbounded $
        Pretty.vcat $
            pretty summary
                : if veryVerbose then map (pretty' @['Decoded, 'Truncated] $) computeCeilsSummary else []

instance Pretty Summary where
    pretty summary =
        Pretty.vsep $
            [ list prettyLabel "Modules" summary.modNames
            , list prettyLabel "Sorts" summary.sortNames
            , list prettyLabel "Symbols" summary.symbolNames
            , "Partial function symbols: " <> pretty summary.partialSymbolsCount
            , "Axioms: " <> pretty summary.axiomCount
            , "Axioms preserving definedness: " <> pretty summary.preserveDefinednessCount
            , "Axioms containing AC symbols: " <> pretty summary.containAcSymbolsCount
            ]
                <> ( "Subsorts:"
                        : tableView prettyLabel prettyLabel summary.subSorts
                   )
                <> ( "Rewrite rules by term index:"
                        : tableView prettyTermIndex pretty summary.rewriteRules
                   )
                <> ( "Function equations by term index:"
                        : tableView prettyTermIndex pretty summary.functionRules
                   )
                <> ( "Simplifications by term index:"
                        : tableView prettyTermIndex pretty summary.simplifications
                   )
                <> ( "Ceils:"
                        : tableView prettyTermIndex prettyCeilRule summary.ceils
                   )
                <> [mempty]
      where
        tableView :: (k -> Doc a) -> (elem -> Doc a) -> Map.Map k [elem] -> [Doc a]
        tableView keyShower elemShower =
            map (uncurry (list elemShower))
                . sortOn (show . fst)
                . map (first (("- " <>) . keyShower))
                . Map.assocs

        list :: (elem -> Doc a) -> Doc a -> [elem] -> Doc a
        list _ header [] = header <> ": -"
        list f header [x] = header <> ": " <> f x
        list f header xs =
            Pretty.hang 2 . Pretty.vsep $
                (header <> ": " <> pretty (length xs)) : map (("- " <>) . f) xs

        prettyLabel = either error (pretty . BS.unpack) . decodeLabel

        prettyTermIndex :: TermIndex -> Doc a
        prettyTermIndex (TermIndex ixs) = Pretty.sep $ map prettyCellIndex ixs

        prettyCellIndex Anything = "Anything"
        prettyCellIndex (TopSymbol sym) = prettyLabel sym
        prettyCellIndex None = "None"

        prettyCeilRule :: RewriteRule r -> Doc a
        prettyCeilRule RewriteRule{lhs, rhs} =
            "#Ceil(" <+> pretty' @['Decoded, 'Truncated] lhs <+> ") =>" <+> pretty' @['Decoded, 'Truncated] rhs

sourceRefText :: HasSourceRef a => a -> Text
sourceRefText = renderOneLineText . pretty . sourceRef
