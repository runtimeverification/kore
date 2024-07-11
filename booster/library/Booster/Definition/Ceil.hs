{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Booster.Definition.Ceil (
    module Booster.Definition.Ceil,
) where

import Booster.Definition.Base hiding (RewriteRule (..))
import Booster.Definition.Base qualified as RewriteRule (RewriteRule (..))

import Booster.Definition.Attributes.Base
import Booster.Pattern.ApplyEquations
import Booster.Pattern.Base

import Booster.LLVM as LLVM (API, simplifyBool)
import Booster.Log
import Booster.Pattern.Bool
import Booster.Pattern.Pretty
import Booster.Pattern.Util (isConcrete, sortOfTerm)
import Booster.Util (Flag (..))
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (runWriterT, tell)
import Data.ByteString.Char8 (isPrefixOf)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import GHC.Generics qualified as GHC
import Prettyprinter (Pretty (..))
import Prettyprinter qualified as Pretty

data ComputeCeilSummary = ComputeCeilSummary
    { rule :: RewriteRule.RewriteRule "Rewrite"
    , ceils :: Set.Set (Either Predicate Term)
    , newRule :: Maybe (RewriteRule.RewriteRule "Rewrite")
    }
    deriving stock (Eq, Ord, Show, GHC.Generic)
    deriving anyclass (NFData)

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods ComputeCeilSummary) where
    pretty (PrettyWithModifiers ComputeCeilSummary{rule, ceils}) =
        Pretty.vsep $
            [ "\n\n----------------------------\n"
            , pretty $ sourceRef rule
            , pretty' @mods rule.lhs
            , "=>"
            , pretty' @mods rule.rhs
            ]
                <> ( if null rule.requires
                        then []
                        else
                            [ "requires"
                            , Pretty.indent 2 . Pretty.vsep $ map (pretty' @mods) $ Set.toList rule.requires
                            ]
                   )
                <> [ Pretty.line
                   , "partial symbols found:"
                   , Pretty.indent 2 . Pretty.vsep $ map pretty rule.computedAttributes.notPreservesDefinednessReasons
                   ]
                <> if null ceils
                    then [Pretty.line, "discharged all ceils, rule preserves definedness"]
                    else
                        [ Pretty.line
                        , "computed ceils:"
                        , Pretty.indent 2 . Pretty.vsep $
                            map
                                (either (pretty' @mods) (\t -> "#Ceil(" Pretty.<+> pretty' @mods t Pretty.<+> ")"))
                                (Set.toList ceils)
                        ]

computeCeilsDefinition ::
    LoggerMIO io =>
    Maybe LLVM.API ->
    KoreDefinition ->
    io (KoreDefinition, [ComputeCeilSummary])
computeCeilsDefinition mllvm def@KoreDefinition{rewriteTheory} = do
    (rewriteTheory', ceilSummaries) <-
        runWriterT $
            let ceilComputation r = do
                    lift (computeCeilRule mllvm def r) >>= \case
                        Nothing -> pure r
                        Just summary@ComputeCeilSummary{newRule} -> do
                            tell (Seq.singleton summary)
                            pure $ fromMaybe r newRule
             in mapM (mapM (mapM ceilComputation)) rewriteTheory
    pure (def{rewriteTheory = rewriteTheory'}, toList ceilSummaries)

computeCeilRule ::
    LoggerMIO io =>
    Maybe LLVM.API ->
    KoreDefinition ->
    RewriteRule.RewriteRule "Rewrite" ->
    io (Maybe ComputeCeilSummary)
computeCeilRule mllvm def r@RewriteRule.RewriteRule{lhs, requires, rhs, attributes, computedAttributes}
    | null computedAttributes.notPreservesDefinednessReasons = pure Nothing
    | otherwise = do
        (res, _) <- runEquationT def mllvm Nothing mempty mempty $ do
            lhsCeils <- Set.fromList <$> computeCeil lhs
            requiresCeils <- Set.fromList <$> concatMapM (computeCeil . coerce) (Set.toList requires)
            let subtractLHSAndRequiresCeils = (Set.\\ (lhsCeils `Set.union` requiresCeils)) . Set.fromList
            rhsCeils <- simplifyCeils =<< (subtractLHSAndRequiresCeils <$> computeCeil rhs)

            pure $
                Just $
                    ComputeCeilSummary
                        { rule = r
                        , ceils = requiresCeils <> rhsCeils
                        , newRule =
                            if null requiresCeils && null rhsCeils
                                then
                                    Just
                                        r
                                            { RewriteRule.attributes = attributes{preserving = Flag True}
                                            , RewriteRule.computedAttributes = computedAttributes{notPreservesDefinednessReasons = []}
                                            }
                                else -- we could add a case when ceils are fully resolved into predicates, which we would then
                                -- add to the requires clause of a rule
                                    Nothing
                        }

        case res of
            Left err -> do
                liftIO $ print err
                pure Nothing
            Right r' -> pure r'
  where
    simplifyCeils ceils =
        (.llvmApi) <$> getConfig >>= \case
            Nothing -> pure ceils
            Just api -> foldM (\ceils' c -> maybe ceils' (`Set.insert` ceils') <$> simplifyCeil api c) mempty ceils

    simplifyCeil api p@(Left (Predicate t@(Term TermAttributes{canBeEvaluated} _)))
        | isConcrete t && canBeEvaluated = do
            simplifyBool api t >>= \case
                Left{} -> pure $ Just p
                Right res ->
                    if res
                        then pure Nothing
                        else error "ceil simplified to bottom"
        | otherwise = pure $ Just p
    simplifyCeil _ other = pure $ Just other

computeCeil :: LoggerMIO io => Term -> EquationT io [Either Predicate Term]
computeCeil term@(SymbolApplication symbol _ args)
    | symbol.attributes.symbolType
        /= Booster.Definition.Attributes.Base.Function Booster.Definition.Attributes.Base.Partial =
        concatMapM computeCeil args
    | otherwise = do
        ceils <- (.definition.ceils) <$> getConfig
        simplified <- applyEquations ceils handleSimplificationEquation term
        -- liftIO $ putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ "original ceil:" <+> pretty (InternalCeil term)
        -- when (simplified /= (InternalCeil term)) $ liftIO $ putStrLn $ Pretty.renderString . layoutPrettyUnbounded $ "applied ceil:" <+> pretty simplified
        if simplified == term
            then pure [Right term]
            else do
                argCeils <- concatMapM computeCeil args
                pure $ (map Left $ splitBoolPredicates $ Predicate simplified) <> argCeils
computeCeil (AndTerm l r) = concatMapM computeCeil [l, r]
computeCeil (Injection _ _ t) = computeCeil t
computeCeil (KMap _ keyVals rest) = do
    recArgs <- concatMapM computeCeil $ concat [[k, v] | (k, v) <- keyVals] <> maybeToList rest
    symbols <- mkInKeysMap . (.definition.symbols) <$> getConfig
    pure $
        [Left $ Predicate $ mkNeq a b | a <- map fst keyVals, b <- map fst keyVals, a /= b]
            <> [ Left $ Predicate $ NotBool (mkInKeys symbols a rest') | a <- map fst keyVals, rest' <- maybeToList rest
               ]
            <> recArgs
computeCeil (KList _ heads rest) = concatMapM computeCeil $ heads <> maybe [] (uncurry (:)) rest
computeCeil (KSet _ elems rest) = do
    recArgs <- concatMapM computeCeil $ elems <> maybeToList rest
    -- forall a b in elems. a =/=K b and forall a in elems. a \not\in rest
    pure $
        [Left $ Predicate $ mkNeq a b | a <- elems, b <- elems, a /= b]
            <> [Left $ Predicate $ NotBool (SetIn a rest') | a <- elems, rest' <- maybeToList rest]
            <> recArgs
computeCeil DomainValue{} = pure []
computeCeil v@(Var Variable{variableName})
    -- this feels a little hacky... we should be distinguishing existentials in a better way
    -- I presume it's ok to make the assumption that a newly introduced existential will be defined?
    | "Ex#" `isPrefixOf` variableName = pure []
    | otherwise = pure [Right v]

mkNeq :: Term -> Term -> Term
mkNeq a b
    | sortOfTerm a == SortKItem = NEqualsK a b
    | sortOfTerm a == SortInt = NEqualsInt a b
    | otherwise = NEqualsK (KSeq (sortOfTerm a) a) (KSeq (sortOfTerm b) b)

mkInKeysMap :: Map.Map SymbolName Symbol -> Map.Map (Sort, Sort) Symbol
mkInKeysMap symbols =
    Map.fromList
        [ (sorts, sym)
        | sym@Symbol{argSorts, attributes = SymbolAttributes{hook}} <- Map.elems symbols
        , hook == Just "MAP.in_keys"
        , sorts <- mTuple argSorts
        ]
  where
    mTuple [x, y] = [(x, y)]
    mTuple _ = []

mkInKeys :: Map.Map (Sort, Sort) Symbol -> Term -> Term -> Term
mkInKeys inKeysSymbols k m =
    case Map.lookup (sortOfTerm k, sortOfTerm m) inKeysSymbols of
        Just inKeysSymbol -> SymbolApplication inKeysSymbol [] [k, m]
        Nothing ->
            error $
                "in_keys for key sort '"
                    <> show (pretty $ PrettyWithModifiers @'[Decoded, Truncated] $ sortOfTerm k)
                    <> "' and map sort '"
                    <> show (pretty $ PrettyWithModifiers @'[Decoded, Truncated] $ sortOfTerm m)
                    <> "' does not exist."
