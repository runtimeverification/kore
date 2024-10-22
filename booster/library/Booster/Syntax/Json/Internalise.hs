{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Syntax.Json.Internalise (
    internalisePattern,
    internalisePatternOrTopOrBottom,
    internaliseTermOrPredicate,
    internaliseTerm,
    internalisePredicates,
    lookupInternalSort,
    PatternError (..),
    patternErrorToRpcError,
    internaliseSort,
    SortError (..),
    renderSortError,
    ----------------
    explodeAnd,
    isTermM,
    textToBS,
    trm,
    handleBS,
    TermOrPredicates (..),
    InternalisedPredicates (..),
    PatternOrTopOrBottom (..),
    retractPattern,
    pattern IsQQ,
    pattern IsNotQQ,
    pattern AllowAlias,
    pattern DisallowAlias,
    pattern CheckSubsorts,
    pattern IgnoreSubsorts,
    logPatternError,
    -- substitution mining
    extractSubsitution,
    -- for test only
    InternalisedPredicate (..),
) where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.ByteString.Char8 (ByteString, isPrefixOf)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isLower)
import Data.Coerce (coerce)
import Data.Foldable ()
import Data.Generics (extQ)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (foldl', foldl1', nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text as Text (Text, intercalate, pack, unpack)
import Data.Text.Encoding (decodeLatin1)
import Language.Haskell.TH (ExpQ, Lit (..), appE, litE, mkName, varE)
import Language.Haskell.TH.Quote
import Prettyprinter (Pretty (..), pretty)
import Prettyprinter qualified as Pretty

import Booster.Definition.Attributes.Base
import Booster.Definition.Attributes.Base qualified as Internal
import Booster.Definition.Base (KoreDefinition (..), emptyKoreDefinition)
import Booster.Log (LoggerMIO, logMessage, withKorePatternContext)
import Booster.Pattern.Base qualified as Internal
import Booster.Pattern.Bool qualified as Internal
import Booster.Pattern.Pretty
import Booster.Pattern.Substitution qualified as Internal
import Booster.Pattern.Util (freeVariables, sortOfTerm, substituteInSort)
import Booster.Prettyprinter (renderDefault)
import Booster.Syntax.Json (addHeader)
import Booster.Syntax.Json.Externalise (externaliseSort)
import Booster.Syntax.ParsedKore.Parser (parsePattern)
import Booster.Util (Flag (..))
import Kore.JsonRpc.Error qualified as RpcError
import Kore.Syntax.Json.Types qualified as Syntax

pattern IsQQ, IsNotQQ :: Flag "qq"
pattern IsQQ = Flag True
pattern IsNotQQ = Flag False

{-# COMPLETE IsQQ, IsNotQQ #-}

pattern AllowAlias, DisallowAlias :: Flag "alias"
pattern AllowAlias = Flag True
pattern DisallowAlias = Flag False

{-# COMPLETE AllowAlias, DisallowAlias #-}

pattern CheckSubsorts, IgnoreSubsorts :: Flag "subsorts"
pattern CheckSubsorts = Flag True
pattern IgnoreSubsorts = Flag False

{-# COMPLETE CheckSubsorts, IgnoreSubsorts #-}

-- helper types for predicate and pattern internalisation
data InternalisedPredicate
    = BoolPred Internal.Predicate
    | CeilPred Internal.Ceil
    | SubstitutionPred Internal.Variable Internal.Term
    | UnsupportedPred Syntax.KorePattern
    deriving stock (Eq, Show)

data InternalisedPredicates = InternalisedPredicates
    { boolPredicates :: [Internal.Predicate]
    , ceilPredicates :: [Internal.Ceil]
    , substitution :: Map Internal.Variable Internal.Term
    , unsupported :: [Syntax.KorePattern]
    }
    deriving stock (Eq, Show)

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods InternalisedPredicates) where
    pretty (PrettyWithModifiers ps) =
        Pretty.vsep $
            ("Bool predicates: " : map (pretty' @mods) ps.boolPredicates)
                <> ("Ceil predicates: " : map (pretty' @mods) ps.ceilPredicates)
                <> ( "Substitution: "
                        : map
                            (\(v, t) -> pretty' @mods v Pretty.<+> "->" Pretty.<+> pretty' @mods t)
                            (Map.assocs ps.substitution)
                   )
                <> ("Unsupported predicates: " : map (pretty . show) ps.unsupported)

data TermOrPredicates -- = Either Predicate Pattern
    = Predicates InternalisedPredicates
    | TermAndPredicates
        Internal.Pattern
        (Map Internal.Variable Internal.Term)
        [Syntax.KorePattern]
    deriving stock (Eq, Show)

retractPattern :: TermOrPredicates -> Maybe Internal.Pattern
retractPattern (TermAndPredicates patt _ _) = Just patt
retractPattern _ = Nothing

-- main interface functions
internalisePattern ::
    Flag "alias" ->
    Flag "subsorts" ->
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    Syntax.KorePattern ->
    Except
        PatternError
        ( Internal.Term
        , [Internal.Predicate]
        , [Internal.Ceil]
        , Map Internal.Variable Internal.Term
        , [Syntax.KorePattern]
        )
internalisePattern allowAlias checkSubsorts sortVars definition p = do
    (terms, predicates) <- partitionM isTermM $ explodeAnd p

    when (null terms) $ throwE $ NoTermFound p

    -- construct an AndTerm from all terms (checking sort consistency)
    term <- andTerm p =<< mapM (internaliseTerm allowAlias checkSubsorts sortVars definition) terms
    -- internalise all predicates
    internalPs <-
        internalisePredicates allowAlias checkSubsorts sortVars definition predicates
    pure
        ( term
        , internalPs.boolPredicates
        , internalPs.ceilPredicates
        , internalPs.substitution
        , internalPs.unsupported
        )
  where
    andTerm :: Syntax.KorePattern -> [Internal.Term] -> Except PatternError Internal.Term
    andTerm _ [] = error "BUG: andTerm called with empty term list"
    andTerm pat ts = do
        let sortList = nub $ map sortOfTerm ts
            resultTerm = foldl1' Internal.AndTerm ts
        -- check resulting terms for consistency and sorts
        -- TODO needs to consider sub-sorts instead (set must be
        -- consistent) if this code becomes order-sorted
        unless (length sortList == 1) $
            throwE $
                PatternSortError pat (IncompatibleSorts $ map externaliseSort sortList)
        pure resultTerm

data PatternOrTopOrBottom a
    = IsTop Syntax.Sort
    | IsBottom Syntax.Sort
    | IsPattern a
    deriving (Functor)

-- main interface functions
internalisePatternOrTopOrBottom ::
    Flag "alias" ->
    Flag "subsorts" ->
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    [(Syntax.Id, Syntax.Sort)] ->
    Syntax.KorePattern ->
    Except
        PatternError
        ( PatternOrTopOrBottom
            ([Internal.Variable], Internal.Pattern, Map Internal.Variable Internal.Term, [Syntax.KorePattern])
        )
internalisePatternOrTopOrBottom allowAlias checkSubsorts sortVars definition existentials p = do
    let exploded = explodeAnd p

    case isTop exploded of
        Just t -> pure t
        Nothing -> case isBottom exploded of
            Just b -> pure b
            Nothing -> do
                existentialVars <- forM existentials $ \(var, sort) -> do
                    variableSort <- lookupInternalSort sortVars definition.sorts p sort
                    let variableName = textToBS var.getId
                    pure $ Internal.Variable{variableSort, variableName}
                (term, preds, ceilConditions, subst, unknown) <-
                    internalisePattern allowAlias checkSubsorts sortVars definition p
                pure $
                    IsPattern
                        ( existentialVars
                        , Internal.Pattern{term, constraints = Set.fromList preds, ceilConditions, substitution = mempty} -- this is the ensures-substitution, leave empty
                        , subst
                        , unknown
                        )
  where
    isTop = \case
        [Syntax.KJTop{sort}] -> Just $ IsTop sort
        Syntax.KJTop{} : xs -> isTop xs
        _ -> Nothing

    isBottom = \case
        [] -> Nothing
        Syntax.KJBottom{sort} : _ -> Just $ IsBottom sort
        _ : xs -> isBottom xs

internaliseTermOrPredicate ::
    Flag "alias" ->
    Flag "subsorts" ->
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    Syntax.KorePattern ->
    Except [PatternError] TermOrPredicates
internaliseTermOrPredicate allowAlias checkSubsorts sortVars definition syntaxPatt =
    ( withExcept (: []) . fmap Predicates $
        internalisePredicates allowAlias checkSubsorts sortVars definition [syntaxPatt]
    )
        <|> ( withExcept (: []) $ do
                (term, constrs, ceilConditions, substitution, unsupported) <-
                    internalisePattern allowAlias checkSubsorts sortVars definition syntaxPatt
                pure $
                    TermAndPredicates
                        Internal.Pattern
                            { term
                            , constraints = Set.fromList constrs
                            , ceilConditions
                            , substitution = mempty -- this is the ensures-substitution, leave empty
                            }
                        substitution
                        unsupported
            )

lookupInternalSort ::
    Maybe [Syntax.Id] ->
    Map Internal.SortName (SortAttributes, Set Internal.SortName) ->
    Syntax.KorePattern ->
    Syntax.Sort ->
    Except PatternError Internal.Sort
lookupInternalSort sortVars sorts pat =
    let knownVarSet = maybe Set.empty (Set.fromList . map Syntax.getId) sortVars
     in mapExcept (first $ PatternSortError pat) . internaliseSort knownVarSet sorts

-- Throws errors when a predicate is encountered. The 'And' case
-- should be analysed before, this function produces an 'AndTerm'.
internaliseTermRaw ::
    Flag "qq" ->
    Flag "alias" ->
    Flag "subsorts" ->
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    Syntax.KorePattern ->
    Except PatternError Internal.Term
internaliseTermRaw qq allowAlias checkSubsorts sortVars definition@KoreDefinition{sorts, symbols} pat =
    case pat of
        Syntax.KJEVar{name, sort} -> do
            variableSort <- lookupInternalSort' sort
            let variableName = textToBS name.getId
            pure $ Internal.Var Internal.Variable{variableSort, variableName}
        Syntax.KJSVar{name, sort} -> do
            variableSort <- lookupInternalSort' sort
            let variableName = textToBS name.getId
            pure $ Internal.Var Internal.Variable{variableSort, variableName}
        Syntax.KJApp{name, sorts = [from, to], args = [arg]}
            | Just Internal.injectionSymbol == Map.lookup (textToBS name.getId) symbols -> do
                from' <- lookupInternalSort' from
                to' <- lookupInternalSort' to
                arg' <- recursion arg
                unless (not (coerce checkSubsorts) || from' `isSubsort` to') $
                    throwE $
                        PatternSortError pat $
                            NotSubsort from' to'
                pure $ Internal.Injection from' to' arg'
        symPatt@Syntax.KJApp{name, sorts = appSorts, args} -> do
            symbol <-
                if coerce qq
                    then
                        pure $
                            Internal.Symbol
                                (textToBS name.getId)
                                []
                                []
                                (Internal.SortApp "QQ" [])
                                Internal.SymbolAttributes
                                    { symbolType = Internal.Constructor
                                    , isIdem = Internal.IsNotIdem
                                    , isAssoc = Internal.IsNotAssoc
                                    , isMacroOrAlias = Internal.IsNotMacroOrAlias
                                    , hasEvaluators = Flag False
                                    , collectionMetadata = Nothing
                                    , smt = Nothing
                                    , hook = Nothing
                                    }
                    else
                        maybe (throwE $ UnknownSymbol name symPatt) pure $
                            Map.lookup (textToBS name.getId) symbols
            -- Internalise sort variable instantiation (appSorts)
            -- Length must match sort variables in symbol declaration.
            unless (coerce qq || length appSorts == length symbol.sortVars) $
                throwE $
                    PatternSortError pat $
                        GeneralError
                            "wrong sort argument count for symbol"
            when (not (coerce allowAlias) && coerce symbol.attributes.isMacroOrAlias) $
                throwE $
                    MacroOrAliasSymbolNotAllowed name symPatt
            unless (coerce qq || length symbol.argSorts == length args) $
                throwE $
                    IncorrectSymbolArity pat name (length symbol.argSorts) (length args)
            args' <- mapM recursion args
            appSorts' <- mapM lookupInternalSort' appSorts
            let sub = Map.fromList $ zip symbol.sortVars appSorts'
            unless (coerce qq) $
                forM_ (zip args $ zip (map (substituteInSort sub) symbol.argSorts) $ map sortOfTerm args') $ \(t, (expected, got)) ->
                    unless (expected == got) $
                        throwE $
                            PatternSortError t $
                                IncorrectSort expected got
            pure $ Internal.SymbolApplication symbol appSorts' args'
        Syntax.KJString{value} ->
            pure $ Internal.DomainValue (Internal.SortApp "SortString" []) $ textToBS value
        Syntax.KJTop{} -> predicate
        Syntax.KJBottom{} -> predicate
        Syntax.KJNot{} -> predicate
        Syntax.KJAnd{patterns} -> do
            -- analysed beforehand, expecting this to operate on terms
            args <- mapM recursion patterns
            -- TODO check that both a and b are of sort "resultSort"
            -- Which is a unification problem if this involves variables.
            pure $ foldr1 Internal.AndTerm args
        Syntax.KJOr{} -> predicate
        Syntax.KJImplies{} -> predicate
        Syntax.KJIff{} -> predicate
        Syntax.KJForall{} -> predicate
        Syntax.KJExists{} -> predicate
        Syntax.KJMu{} -> predicate
        Syntax.KJNu{} -> predicate
        Syntax.KJCeil{} -> predicate
        Syntax.KJFloor{} -> predicate
        Syntax.KJEquals{} -> predicate
        Syntax.KJIn{} -> predicate
        Syntax.KJNext{} -> predicate
        Syntax.KJRewrites{} -> predicate
        Syntax.KJDV{sort, value} ->
            Internal.DomainValue
                <$> lookupInternalSort' sort
                <*> pure (textToBS value)
        Syntax.KJMultiOr{} -> predicate
        Syntax.KJLeftAssoc{symbol, sorts = argSorts, argss} ->
            recursion $ foldl1 (mkF symbol argSorts) argss
        Syntax.KJRightAssoc{symbol, sorts = argSorts, argss} ->
            recursion $ foldr1 (mkF symbol argSorts) argss
  where
    predicate = throwE $ TermExpected pat

    lookupInternalSort' sort =
        if coerce qq
            then pure $ case sort of
                Syntax.SortApp{name = Syntax.Id n} -> Internal.SortApp (textToBS n) []
                Syntax.SortVar{name = Syntax.Id n} -> Internal.SortVar $ textToBS n
            else lookupInternalSort sortVars sorts pat sort

    s `isSubsort` t = case (s, t) of
        (Internal.SortApp ns _, Internal.SortApp nt _) ->
            case Map.lookup nt sorts of
                Just (_, subs) -> ns `Set.member` subs
                -- this case should be unreachable since we have already internalised these sorts and therefore know they exist
                Nothing -> False
        _ -> False

    recursion = internaliseTermRaw qq allowAlias checkSubsorts sortVars definition

internaliseTerm ::
    Flag "alias" ->
    Flag "subsorts" ->
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    Syntax.KorePattern ->
    Except PatternError Internal.Term
internaliseTerm = internaliseTermRaw IsNotQQ

{- | Internalises an And-ed set of predicates, classifying them into
  BoolPred, CeilPred, SubstitutionPred, and UnsupportedPred.
  The substitution (as a whole) is analysed after internalisation, to
  ensure nothing is circular or ambiguous.

  Throws a PatternError when any of the top-level patterns is a term
  or an And with a term (must be analysed beforehand).
-}
internalisePredicates ::
    Flag "alias" ->
    Flag "subsorts" ->
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    [Syntax.KorePattern] ->
    Except PatternError InternalisedPredicates
internalisePredicates allowAlias checkSubsorts sortVars definition ps = do
    internalised <-
        concatMapM (internalisePred allowAlias checkSubsorts sortVars definition) $
            concatMap explodeAnd ps

    let (substitution, moreEquations) =
            mkSubstitution [s | s@SubstitutionPred{} <- internalised]

    pure
        InternalisedPredicates
            { boolPredicates = [p | BoolPred p <- internalised] <> moreEquations
            , ceilPredicates = [p | CeilPred p <- internalised]
            , substitution
            , unsupported = [p | UnsupportedPred p <- internalised]
            }

{- | Vet a given set of substitution predicates (Var -> Term), returning
   a checked consistent substitution (Map) and bool equations for
   everything that could not be part of this substitution

1. ambiguous substitutions (several substitutions for the same
   variable) are turned into BoolPredicate equations

2. The resulting substitution (with unique replacement for each
   variable) is checked for cycles. For each cycle, one of the
   substitution equations is turned into a BoolPredicate equation,
   iteratively until the substitution is acyclic.

Also see https://gist.github.com/jberthold/984a5a8d87c6ce9c0b5b97ddd3a2e9f2
-}
mkSubstitution ::
    [InternalisedPredicate] -> (Map Internal.Variable Internal.Term, [Internal.Predicate])
mkSubstitution initialSubst =
    let substMap, duplicates :: Map Internal.Variable [Internal.Term]
        (substMap, duplicates) =
            Map.partition ((== 1) . length) $
                Map.fromListWith (<>) [(v, [t]) | SubstitutionPred v t <- initialSubst]
        equations =
            [Internal.mkEq v t | (v, ts) <- Map.assocs duplicates, t <- ts]
     in execState breakCycles (Map.map head substMap, equations)
  where
    breakCycles :: State (Map Internal.Variable Internal.Term, [Internal.Predicate]) ()
    breakCycles = do
        assocs <- gets (Map.assocs . fst)
        let sccs =
                stronglyConnComp [(v, v, Set.toList (freeVariables t)) | (v, t) <- assocs]
            cycleNodes = Set.fromList [v | CyclicSCC (v : _vs) <- sccs]
        if null cycleNodes
            then pure ()
            else do
                modify $ \(m, eqs) ->
                    ( m `Map.withoutKeys` cycleNodes
                    , eqs <> (map (uncurry Internal.mkEq) $ Map.assocs $ m `Map.restrictKeys` cycleNodes)
                    )
                breakCycles

-- | Given a equality @'Term'@, decide if it can be interpreted as substitution.
mbSubstitution ::
    Internal.Term -> InternalisedPredicate
mbSubstitution = \case
    eq@(Internal.EqualsInt (Internal.Var x) e)
        | x `Set.member` freeVariables e -> boolPred eq
        | otherwise -> SubstitutionPred x e
    eq@(Internal.EqualsInt e (Internal.Var x))
        | x `Set.member` freeVariables e -> boolPred eq
        | otherwise -> SubstitutionPred x e
    eq@(Internal.EqualsK (Internal.KSeq _sortL (Internal.Var x)) (Internal.KSeq _sortR e)) ->
        do
            -- NB sorts do not have to agree! (could be subsorts)
            if (x `Set.member` freeVariables e)
                then boolPred eq
                else SubstitutionPred x e
    eq@(Internal.EqualsK (Internal.KSeq _sortL e) (Internal.KSeq _sortR (Internal.Var x))) ->
        do
            -- NB sorts do not have to agree! (could be subsorts)
            if (x `Set.member` freeVariables e)
                then boolPred eq
                else SubstitutionPred x e
    other ->
        boolPred other
  where
    boolPred = BoolPred . Internal.Predicate

extractSubsitution ::
    [Internal.Predicate] -> (Map Internal.Variable Internal.Term, Set Internal.Predicate)
extractSubsitution ps =
    let (potentialSubstituion, otherPreds) = partitionSubstitutionPreds . map mbSubstitution . coerce $ ps
        (newSubstitution, leftoverPreds) = mkSubstitution potentialSubstituion
     in (newSubstitution, Set.fromList $ leftoverPreds <> map unsafeFromBoolPred otherPreds)
  where
    partitionSubstitutionPreds = foldl' accumulate ([], [])
      where
        accumulate (accSubst, accOther) = \case
            pSubst@SubstitutionPred{} -> (pSubst : accSubst, accOther)
            pOther -> (accSubst, pOther : accOther)

    -- this conversion is safe withing this function since we pass Internal.Predicate as input
    unsafeFromBoolPred :: InternalisedPredicate -> Internal.Predicate
    unsafeFromBoolPred = \case
        BoolPred p -> coerce p
        _ -> error "extractSubsitution.unsafeFromBoolPred: impossible case"

internalisePred ::
    Flag "alias" ->
    Flag "subsorts" ->
    Maybe [Syntax.Id] ->
    KoreDefinition ->
    Syntax.KorePattern ->
    Except PatternError [InternalisedPredicate]
internalisePred allowAlias checkSubsorts sortVars definition@KoreDefinition{sorts} p = case p of
    Syntax.KJEVar{} -> term
    Syntax.KJSVar{} -> term
    Syntax.KJApp{} -> term
    Syntax.KJString{} -> term
    Syntax.KJTop{} -> pure [BoolPred $ Internal.Predicate Internal.TrueBool]
    Syntax.KJBottom{} -> notSupported
    Syntax.KJNot{arg} -> do
        recursion arg >>= \case
            [BoolPred (Internal.Predicate p')] ->
                pure [BoolPred $ Internal.Predicate $ Internal.NotBool p']
            [SubstitutionPred k v] ->
                if "@" `isPrefixOf` k.variableName
                    then notSupported -- @ variables are set variables, the negation of which we do not support internalising
                    else case sortOfTerm v of
                        Internal.SortInt ->
                            pure [BoolPred $ Internal.Predicate $ Internal.NotBool $ Internal.EqualsInt (Internal.Var k) v]
                        otherSort ->
                            pure
                                [ BoolPred $
                                    Internal.Predicate $
                                        Internal.NotBool $
                                            Internal.EqualsK (Internal.KSeq otherSort $ Internal.Var k) (Internal.KSeq otherSort v)
                                ]
            _ -> notSupported
    Syntax.KJAnd{patterns = []} -> notSupported
    Syntax.KJAnd{patterns = [p']} -> recursion p'
    Syntax.KJAnd{patterns = ps'} -> concatMapM recursion ps'
    Syntax.KJOr{} -> notSupported
    Syntax.KJImplies{} -> notSupported
    Syntax.KJIff{} -> notSupported
    Syntax.KJForall{} -> notSupported
    Syntax.KJExists{} -> notSupported
    Syntax.KJMu{} -> notSupported
    Syntax.KJNu{} -> notSupported
    Syntax.KJCeil{arg} ->
        (: []) . CeilPred . Internal.Ceil
            <$> internaliseTerm allowAlias checkSubsorts sortVars definition arg
    Syntax.KJFloor{} -> notSupported
    Syntax.KJEquals{argSort, first = arg1, second = arg2} -> do
        -- distinguish term and predicate equality
        is1Term <- isTermM arg1
        is2Term <- isTermM arg2
        case (is1Term, is2Term) of
            (True, True) -> do
                a <- internaliseTerm allowAlias checkSubsorts sortVars definition arg1
                b <- internaliseTerm allowAlias checkSubsorts sortVars definition arg2
                argS <- lookupInternalSort' argSort
                -- check that argS and sorts of a and b "agree"
                ensureEqualSorts (sortOfTerm a) argS
                ensureEqualSorts (sortOfTerm b) argS
                case (argS, a, b) of
                    -- for "true" #Equals P, check whether P is in fact a substitution
                    (Internal.SortBool, Internal.TrueBool, x) ->
                        pure [mbSubstitution x]
                    (Internal.SortBool, x, Internal.TrueBool) ->
                        pure [mbSubstitution x]
                    -- we could also detect NotBool (NEquals _) in "false" #Equals P
                    (Internal.SortBool, Internal.FalseBool, x) ->
                        pure [BoolPred $ Internal.Predicate $ Internal.NotBool x]
                    (Internal.SortBool, x, Internal.FalseBool) ->
                        pure [BoolPred $ Internal.Predicate $ Internal.NotBool x]
                    (_, Internal.Var x, t)
                        | x `Set.member` freeVariables t ->
                            pure [BoolPred $ Internal.mkEq x t]
                        | otherwise ->
                            pure [SubstitutionPred x t]
                    (_, t, Internal.Var x)
                        | x `Set.member` freeVariables t ->
                            pure [BoolPred $ Internal.mkEq x t]
                        | otherwise ->
                            pure [SubstitutionPred x t]
                    (Internal.SortInt, _, _) ->
                        pure [BoolPred $ Internal.Predicate $ Internal.EqualsInt a b]
                    (otherSort, _, _) ->
                        pure
                            [ BoolPred $
                                Internal.Predicate $
                                    Internal.EqualsK (Internal.KSeq otherSort a) (Internal.KSeq otherSort b)
                            ]
            (False, False) -> notSupported
            _other ->
                throwE $ InconsistentPattern p
    Syntax.KJIn{} -> notSupported
    Syntax.KJNext{} -> notSupported
    Syntax.KJRewrites{} -> notSupported -- should only occur in claims!
    Syntax.KJDV{} -> term
    Syntax.KJMultiOr{} -> notSupported
    Syntax.KJLeftAssoc{} -> term
    Syntax.KJRightAssoc{} -> term
  where
    notSupported = pure [UnsupportedPred p]
    term = throwE $ PredicateExpected p
    lookupInternalSort' = lookupInternalSort sortVars sorts p

    recursion = internalisePred allowAlias checkSubsorts sortVars definition

    -- Recursively check that two (internal) sorts are the same.
    -- Sort variables must be eliminated (instantiated) before checking.
    ensureEqualSorts :: Internal.Sort -> Internal.Sort -> Except PatternError ()
    ensureEqualSorts s s' = mapExcept (first $ PatternSortError p) $ go s s'
      where
        go :: Internal.Sort -> Internal.Sort -> Except SortError ()
        go s1 s2 =
            case (s1, s2) of
                (Internal.SortVar n, _) ->
                    throwE $ GeneralError ("ensureSortsAgree found variable " <> decodeLatin1 n)
                (_, Internal.SortVar n) ->
                    throwE $ GeneralError ("ensureSortsAgree found variable " <> decodeLatin1 n)
                (Internal.SortApp name1 args1, Internal.SortApp name2 args2) -> do
                    unless (name1 == name2) $
                        throwE (IncompatibleSorts (map externaliseSort [s1, s2]))
                    zipWithM_ go args1 args2

----------------------------------------

-- for use with withAssoc
mkF ::
    Syntax.Id ->
    [Syntax.Sort] ->
    Syntax.KorePattern ->
    Syntax.KorePattern ->
    Syntax.KorePattern
mkF symbol argSorts a b = Syntax.KJApp symbol argSorts [a, b]

-- primitive solution ignoring text encoding
textToBS :: Text -> ByteString
textToBS = BS.pack . Text.unpack

----------------------------------------

{- | Given a set of sort variable names and a sort attribute map, checks
   a given syntactic @Sort@ and converts to an internal Sort
-}
internaliseSort ::
    Set Text ->
    Map Internal.SortName (SortAttributes, Set Internal.SortName) ->
    Syntax.Sort ->
    Except SortError Internal.Sort
internaliseSort knownVars sortMap = check'
  where
    check' :: Syntax.Sort -> Except SortError Internal.Sort
    check' var@Syntax.SortVar{name = Syntax.Id n} = do
        unless (n `Set.member` knownVars) $
            throwE (UnknownSort var)
        pure $ Internal.SortVar $ textToBS n
    check' app@Syntax.SortApp{name = Syntax.Id n, args} =
        do
            let name = textToBS n
            maybe
                (throwE $ UnknownSort app)
                ( \(SortAttributes{argCount}, _) ->
                    unless (length args == argCount) $
                        throwE (WrongSortArgCount app argCount)
                )
                (Map.lookup name sortMap)
            internalArgs <- mapM check' args
            pure $ Internal.SortApp name internalArgs

isTermM :: Syntax.KorePattern -> Except PatternError Bool
isTermM pat = case pat of
    Syntax.KJEVar{} -> pure True
    Syntax.KJSVar{} -> pure True
    Syntax.KJApp{} -> pure True
    Syntax.KJString{} -> pure True
    Syntax.KJTop{} -> pure False
    Syntax.KJBottom{} -> pure False
    Syntax.KJNot{} -> pure False
    Syntax.KJAnd{patterns} -> do
        terms <- mapM isTermM patterns
        when (length (nub terms) /= 1) $ throwE (InconsistentPattern pat)
        pure $ head terms
    Syntax.KJOr{} -> pure False
    Syntax.KJImplies{} -> pure False
    Syntax.KJIff{} -> pure False
    Syntax.KJForall{} -> pure False
    Syntax.KJExists{} -> pure False
    Syntax.KJMu{} -> notSupported
    Syntax.KJNu{} -> notSupported
    Syntax.KJCeil{} -> pure False
    Syntax.KJFloor{} -> pure False
    Syntax.KJEquals{} -> pure False
    Syntax.KJIn{} -> pure False
    Syntax.KJNext{} -> notSupported
    Syntax.KJRewrites{} -> notSupported -- should only occur in claims
    Syntax.KJDV{} -> pure True
    Syntax.KJMultiOr{} -> pure False
    Syntax.KJLeftAssoc{} -> pure True
    Syntax.KJRightAssoc{} -> pure True
  where
    notSupported = throwE $ NotSupported pat

{- | unpacks the top level of 'And' nodes of a 'KorePattern', returning
   the arguments as a list. The top-level sorts of the 'And' nodes are
   ignored, no checks are performed.
-}
explodeAnd :: Syntax.KorePattern -> [Syntax.KorePattern]
explodeAnd Syntax.KJAnd{patterns} =
    concatMap explodeAnd patterns
explodeAnd other = [other]

----------------------------------------

data PatternError
    = NotSupported Syntax.KorePattern
    | NoTermFound Syntax.KorePattern
    | PatternSortError Syntax.KorePattern SortError
    | InconsistentPattern Syntax.KorePattern
    | TermExpected Syntax.KorePattern
    | PredicateExpected Syntax.KorePattern
    | UnknownSymbol Syntax.Id Syntax.KorePattern
    | MacroOrAliasSymbolNotAllowed Syntax.Id Syntax.KorePattern
    | SubstitutionNotAllowed
    | PredicateNotAllowed
    | CeilNotAllowed
    | IncorrectSymbolArity Syntax.KorePattern Syntax.Id Int Int
    deriving stock (Eq, Show)

{- | ToJson instance (user-facing):

Renders an error string as 'error' and providing the pattern or Id in
a 'context' list.

The JSON-RPC server's error responses contain both of these fields in
a 'data' object.

If the error is a sort error, the message will contain its information
while the context provides the pattern where the error occurred.
-}
patternErrorToRpcError :: PatternError -> RpcError.ErrorWithTermAndContext
patternErrorToRpcError = \case
    NotSupported p ->
        wrap "Pattern not supported" p
    NoTermFound p ->
        wrap "Pattern must contain at least one term" p
    PatternSortError p sortErr ->
        wrap (renderSortError (PrettyWithModifiers @['Decoded, 'Truncated] sortErr)) p
    InconsistentPattern p ->
        wrap "Inconsistent pattern" p
    TermExpected p ->
        wrap "Expected a term but found a predicate" p
    PredicateExpected p ->
        wrap "Expected a predicate but found a term" p
    UnknownSymbol sym p ->
        wrap ("Unknown symbol '" <> Syntax.getId sym <> "'") p
    MacroOrAliasSymbolNotAllowed sym p ->
        wrap ("Symbol '" <> Syntax.getId sym <> "' is a macro/alias") p
    SubstitutionNotAllowed -> RpcError.ErrorOnly "Substitution predicates are not allowed here"
    PredicateNotAllowed -> RpcError.ErrorOnly "Predicates are not allowed here"
    CeilNotAllowed -> RpcError.ErrorOnly "Ceil predicates are not allowed here"
    IncorrectSymbolArity p s expected got ->
        wrap
            ( "Inconsistent pattern. Symbol '"
                <> Syntax.getId s
                <> "' expected "
                <> (pack $ show expected)
                <> " arguments but got "
                <> (pack $ show got)
            )
            p
  where
    wrap :: Text -> Syntax.KorePattern -> RpcError.ErrorWithTermAndContext
    wrap err p = RpcError.ErrorWithTerm err $ addHeader p

logPatternError :: LoggerMIO m => PatternError -> m ()
logPatternError = \case
    NotSupported p -> withKorePatternContext p $ logMessage ("Pattern not supported" :: Text)
    NoTermFound p -> withKorePatternContext p $ logMessage ("Pattern must contain at least one term" :: Text)
    PatternSortError p sortErr -> withKorePatternContext p $ logMessage $ pack $ show sortErr
    InconsistentPattern p -> withKorePatternContext p $ logMessage ("Inconsistent pattern" :: Text)
    TermExpected p -> withKorePatternContext p $ logMessage ("Expected a term but found a predicate" :: Text)
    PredicateExpected p -> withKorePatternContext p $ logMessage ("Expected a predicate but found a term" :: Text)
    UnknownSymbol sym p -> withKorePatternContext p $ logMessage $ "Unknown symbol '" <> Syntax.getId sym <> "'"
    MacroOrAliasSymbolNotAllowed sym p -> withKorePatternContext p $ logMessage $ "Symbol '" <> Syntax.getId sym <> "' is a macro/alias"
    SubstitutionNotAllowed -> logMessage ("Substitution predicates are not allowed here" :: Text)
    PredicateNotAllowed -> logMessage ("Predicates are not allowed here" :: Text)
    CeilNotAllowed -> logMessage ("Ceil predicates are not allowed here" :: Text)
    IncorrectSymbolArity p s expected got ->
        withKorePatternContext p $
            logMessage $
                "Inconsistent pattern. Symbol '"
                    <> Syntax.getId s
                    <> "' expected "
                    <> (pack $ show expected)
                    <> " arguments but got "
                    <> (pack $ show got)

data SortError
    = UnknownSort Syntax.Sort
    | WrongSortArgCount Syntax.Sort Int
    | IncompatibleSorts [Syntax.Sort]
    | NotSubsort Internal.Sort Internal.Sort
    | GeneralError Text
    | IncorrectSort Internal.Sort Internal.Sort
    deriving stock (Eq, Show)

renderSortError :: forall mods. FromModifiersT mods => PrettyWithModifiers mods SortError -> Text
renderSortError (PrettyWithModifiers e) = case e of
    UnknownSort sort ->
        "Unknown " <> render sort
    WrongSortArgCount sort expected ->
        "Wrong argument count: expected "
            <> pack (show expected)
            <> " in "
            <> render sort
    IncompatibleSorts sorts ->
        "Incompatible sorts: " <> intercalate ", " (map render sorts)
    NotSubsort source target ->
        prettyText source <> " is not a subsort of " <> prettyText target
    GeneralError msg ->
        msg
    IncorrectSort expected got ->
        "Incorrect sort: expected "
            <> prettyText expected
            <> " but got "
            <> prettyText got
  where
    prettyText = pack . renderDefault . pretty' @mods

    render = \case
        Syntax.SortVar (Syntax.Id n) ->
            "sort variable " <> n
        Syntax.SortApp (Syntax.Id n) args ->
            "sort " <> n <> "(" <> intercalate ", " (map render args) <> ")"

recomputeTermAttributes :: Internal.Term -> Internal.Term
recomputeTermAttributes = \case
    Internal.AndTerm t1 t2 -> Internal.AndTerm (recomputeTermAttributes t1) (recomputeTermAttributes t2)
    Internal.SymbolApplication sym sorts args -> Internal.SymbolApplication sym sorts $ map recomputeTermAttributes args
    Internal.DomainValue sort dv -> Internal.DomainValue sort dv
    Internal.Var v -> Internal.Var v
    Internal.Injection from to t -> Internal.Injection from to $ recomputeTermAttributes t
    Internal.KMap def keyVals rest ->
        Internal.KMap
            def
            (map (bimap recomputeTermAttributes recomputeTermAttributes) keyVals)
            (fmap recomputeTermAttributes rest)
    Internal.KList def heads rest ->
        Internal.KList
            def
            (map recomputeTermAttributes heads)
            (fmap (bimap recomputeTermAttributes (map recomputeTermAttributes)) rest)
    Internal.KSet def heads rest ->
        Internal.KSet
            def
            (map recomputeTermAttributes heads)
            (fmap recomputeTermAttributes rest)

trm :: QuasiQuoter
trm =
    QuasiQuoter
        { quoteExp = do
            res <-
                dataToExpQ (const Nothing `extQ` handleBS `extQ` metaSymb `extQ` metaTerm)
                    . either (error . show) id
                    . runExcept
                    . internaliseTermRaw
                        IsQQ
                        DisallowAlias
                        IgnoreSubsorts
                        Nothing
                        (emptyKoreDefinition defaultDefAttributes)
                    . either error id
                    . parsePattern "INLINE"
                    . pack
            pure [|recomputeTermAttributes $(res)|]
        , quotePat = undefined
        , quoteType = undefined
        , quoteDec = undefined
        }
  where
    metaSymb :: Internal.Symbol -> Maybe ExpQ
    metaSymb (Internal.Symbol{name}) =
        Just [|$(varE (mkName $ BS.unpack name))|]

    metaTerm :: Internal.Term -> Maybe ExpQ
    metaTerm (Internal.Var Internal.Variable{variableName})
        | not (BS.null variableName) && isLower (BS.head variableName) =
            Just [|$(varE (mkName $ BS.unpack variableName))|]
    metaTerm _ = Nothing

handleBS :: ByteString -> Maybe ExpQ
handleBS x =
    -- convert the byte string to a string literal
    -- and wrap it back with BS.pack
    Just $ appE (varE 'BS.pack) $ litE $ StringL $ BS.unpack x
