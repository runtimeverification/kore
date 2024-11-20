{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Booster.Pattern.Implies (runImplies) where

import Control.Monad (unless)
import Control.Monad.Extra (void)
import Control.Monad.Trans.Except (runExcept)
import Data.Coerce (coerce)
import Data.Data (Proxy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Network.JSONRPC (ErrorObj)
import Prettyprinter (comma, hsep, punctuate, (<+>))

import Booster.Definition.Base (KoreDefinition)
import Booster.LLVM qualified
import Booster.Log (getPrettyModifiers)
import Booster.Log qualified
import Booster.Pattern.ApplyEquations qualified as ApplyEquations
import Booster.Pattern.Base (Pattern (..), Predicate (..))
import Booster.Pattern.Bool (pattern TrueBool)
import Booster.Pattern.Match (FailReason (..), MatchResult (..), MatchType (Implies), matchTerms)
import Booster.Pattern.Pretty (FromModifiersT, ModifiersRep (..), pretty')
import Booster.Pattern.Substitution (asEquations)
import Booster.Pattern.Util (freeVariables, sortOfPattern)
import Booster.Prettyprinter (renderDefault)
import Booster.SMT.Interface qualified as SMT
import Booster.Syntax.Json (addHeader, prettyPattern)
import Booster.Syntax.Json.Externalise (
    externaliseExistTerm,
    externaliseSort,
    externaliseSubstitution,
 )
import Booster.Syntax.Json.Internalise (
    PatternOrTopOrBottom (..),
    internalisePatternOrTopOrBottom,
    logPatternError,
    patternErrorToRpcError,
    pattern CheckSubsorts,
    pattern DisallowAlias,
 )
import Booster.Syntax.ParsedKore.Internalise (extractExistentials)
import Booster.Util (constructorName)

import Kore.JsonRpc.Error qualified as RpcError
import Kore.JsonRpc.Types qualified as RpcTypes
import Kore.Syntax.Json.Types qualified as Kore.Syntax

runImplies ::
    Booster.Log.LoggerMIO m =>
    KoreDefinition ->
    Maybe Booster.LLVM.API ->
    Maybe SMT.SMTOptions ->
    Kore.Syntax.KoreJson ->
    Kore.Syntax.KoreJson ->
    m (Either ErrorObj (RpcTypes.API 'RpcTypes.Res))
runImplies def mLlvmLibrary mSMTOptions antecedent consequent =
    getPrettyModifiers >>= \case
        ModifiersRep (_ :: FromModifiersT mods => Proxy mods) -> Booster.Log.withContext Booster.Log.CtxImplies $ do
            solver <- maybe (SMT.noSolver) (SMT.initSolver def) mSMTOptions
            -- internalise given constrained term
            let internalised korePat' =
                    let (korePat, existentials) = extractExistentials korePat'
                     in runExcept $
                            internalisePatternOrTopOrBottom DisallowAlias CheckSubsorts Nothing def existentials korePat

                checkImplies patL unsupportedL existsL patR unsupportedR existsR = do
                    let substitutionL = patL.substitution
                        substitutionR = patR.substitution
                        freeVarsL =
                            ( freeVariables patL.term
                                <> (Set.unions $ Set.map (freeVariables . coerce) patL.constraints)
                                <> (Set.fromList $ Map.keys substitutionL)
                            )
                                Set.\\ Set.fromList existsL
                        freeVarsR =
                            ( freeVariables patR.term
                                <> (Set.unions $ Set.map (freeVariables . coerce) patR.constraints)
                                <> (Set.fromList $ Map.keys substitutionR)
                            )
                                Set.\\ Set.fromList existsR
                        freeVarsRminusL = freeVarsR Set.\\ freeVarsL
                    if
                        | not $ null freeVarsRminusL ->
                            pure . Left . RpcError.backendError . RpcError.ImplicationCheckError $
                                RpcError.ErrorWithContext "The RHS must not have free variables not present in the LHS" $
                                    map (pack . renderDefault . pretty' @mods) $
                                        Set.toList freeVarsRminusL
                        | not (null unsupportedL) || not (null unsupportedR) -> do
                            Booster.Log.logMessage
                                ("aborting due to unsupported predicate parts" :: Text)
                            unless (null unsupportedL) $
                                Booster.Log.withContext Booster.Log.CtxDetail $
                                    Booster.Log.logMessage
                                        (Text.unwords $ map prettyPattern unsupportedL)
                            unless (null unsupportedR) $
                                Booster.Log.withContext Booster.Log.CtxDetail $
                                    Booster.Log.logMessage
                                        (Text.unwords $ map prettyPattern unsupportedR)
                            pure . Left . RpcError.backendError . RpcError.ImplicationCheckError $
                                RpcError.ErrorWithContext "Could not internalise part of the configuration" $
                                    map (pack . show) $
                                        unsupportedL <> unsupportedR
                        | otherwise -> do
                            SMT.isSat solver (Set.toList patL.constraints) patL.substitution >>= \case
                                SMT.IsUnsat ->
                                    let sort = externaliseSort $ sortOfPattern patL
                                     in implies' (Kore.Syntax.KJBottom sort) sort antecedent.term consequent.term mempty
                                _ -> checkImpliesMatchTerms existsL patL existsR patR

                checkImpliesMatchTerms existsL patL existsR patR =
                    case matchTerms Booster.Pattern.Match.Implies def patR.term patL.term of
                        MatchFailed (SubsortingError sortError) ->
                            pure . Left . RpcError.backendError . RpcError.ImplicationCheckError . RpcError.ErrorOnly . pack $
                                show sortError
                        MatchFailed{} ->
                            doesNotImply
                                (sortOfPattern patL)
                                (externaliseExistTerm existsL patL.term)
                                (externaliseExistTerm existsR patR.term)
                        MatchIndeterminate remainder ->
                            ApplyEquations.evaluatePattern def mLlvmLibrary solver mempty patL >>= \case
                                (Right simplifedSubstPatL, _) ->
                                    if patL == simplifedSubstPatL
                                        then -- we are being conservative here for now and returning an error.
                                        -- since we have already simplified the LHS, we may want to eventually return implise, but the condition
                                        -- will contain the remainder as an equality contraint, predicating the implication on that equality being true.

                                            pure . Left . RpcError.backendError . RpcError.ImplicationCheckError . RpcError.ErrorOnly . pack $
                                                "match remainder: "
                                                    <> renderDefault
                                                        ( hsep $
                                                            punctuate comma $
                                                                map (\(t1, t2) -> pretty' @mods t1 <+> "==" <+> pretty' @mods t2) $
                                                                    NonEmpty.toList remainder
                                                        )
                                        else checkImpliesMatchTerms existsL simplifedSubstPatL existsR patR
                                (Left err, _) ->
                                    pure . Left . RpcError.backendError $ RpcError.Aborted (Text.pack . constructorName $ err)
                        MatchSuccess subst -> do
                            let filteredConsequentPreds =
                                    (patR.constraints <> (Set.fromList $ asEquations patR.substitution))
                                        `Set.difference` (patL.constraints <> (Set.fromList $ asEquations patL.substitution))

                            if null filteredConsequentPreds
                                then
                                    implies
                                        (sortOfPattern patL)
                                        (externaliseExistTerm existsL patL.term)
                                        (externaliseExistTerm existsR patR.term)
                                        subst
                                else -- FIXME This is incomplete because patL.constraints are not assumed in the check.

                                    ApplyEquations.evaluateConstraints def mLlvmLibrary solver mempty filteredConsequentPreds >>= \case
                                        (Right newPreds, _) ->
                                            if all (== Predicate TrueBool) newPreds
                                                then
                                                    implies
                                                        (sortOfPattern patL)
                                                        (externaliseExistTerm existsL patL.term)
                                                        (externaliseExistTerm existsR patR.term)
                                                        subst
                                                else do
                                                    -- here we conservatively abort (incomplete)
                                                    -- print all unclear predicates
                                                    let unclear = Set.toList $ Set.filter (/= Predicate TrueBool) newPreds
                                                    Booster.Log.withContext Booster.Log.CtxDetail
                                                        . Booster.Log.logMessage
                                                        . renderDefault
                                                        . hsep
                                                        . punctuate comma
                                                        $ map (pretty' @mods) unclear
                                                    pure . Left . RpcError.backendError $ RpcError.Aborted "unknown constraints"
                                        (Left other, _) ->
                                            pure . Left . RpcError.backendError $ RpcError.Aborted (Text.pack . constructorName $ other)

            case (internalised antecedent.term, internalised consequent.term) of
                (Left patternError, _) -> do
                    void $ Booster.Log.withContext Booster.Log.CtxInternalise $ logPatternError patternError
                    pure $
                        Left $
                            RpcError.backendError $
                                RpcError.CouldNotVerifyPattern
                                    [ patternErrorToRpcError patternError
                                    ]
                (_, Left patternError) -> do
                    void $ Booster.Log.withContext Booster.Log.CtxInternalise $ logPatternError patternError
                    pure $
                        Left $
                            RpcError.backendError $
                                RpcError.CouldNotVerifyPattern
                                    [ patternErrorToRpcError patternError
                                    ]
                (Right (IsBottom sort), Right _) ->
                    implies' (Kore.Syntax.KJBottom sort) sort antecedent.term consequent.term mempty
                (Right IsTop{}, _) ->
                    pure . Left . RpcError.backendError . RpcError.ImplicationCheckError . RpcError.ErrorOnly $
                        "The check implication step expects the antecedent term to be function-like."
                ( Right (IsPattern (existsL, patL, unsupportedL))
                    , Right (IsPattern (existsR, patR, unsupportedR))
                    ) ->
                        checkImplies patL unsupportedL existsL patR unsupportedR existsR
                (Right IsPattern{}, Right (IsTop sort)) ->
                    implies' (Kore.Syntax.KJTop sort) sort antecedent.term consequent.term mempty
                (Right IsPattern{}, Right (IsBottom sort)) ->
                    doesNotImply'
                        sort
                        ( Just $
                            RpcTypes.Condition
                                { predicate = addHeader $ Kore.Syntax.KJBottom sort
                                , substitution = addHeader $ Kore.Syntax.KJTop sort
                                }
                        )
                        antecedent.term
                        consequent.term
  where
    doesNotImply' s condition l r =
        pure $
            Right $
                RpcTypes.Implies
                    RpcTypes.ImpliesResult
                        { implication = addHeader $ Kore.Syntax.KJImplies s l r
                        , valid = False
                        , condition
                        , logs = Nothing
                        }

    doesNotImply s' = let s = externaliseSort s' in doesNotImply' s Nothing
    implies' predicate s l r subst =
        pure $
            Right $
                RpcTypes.Implies
                    RpcTypes.ImpliesResult
                        { implication = addHeader $ Kore.Syntax.KJImplies s l r
                        , valid = True
                        , condition =
                            Just
                                RpcTypes.Condition
                                    { predicate = addHeader predicate
                                    , substitution =
                                        addHeader
                                            $ ( \case
                                                    [] -> Kore.Syntax.KJTop s
                                                    [x] -> x
                                                    xs -> Kore.Syntax.KJAnd s xs
                                              )
                                                . map (uncurry $ externaliseSubstitution s)
                                                . Map.toList
                                            $ subst
                                    }
                        , logs = Nothing
                        }
    implies s' = let s = externaliseSort s' in implies' (Kore.Syntax.KJTop s) s
