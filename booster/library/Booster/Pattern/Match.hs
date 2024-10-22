{-# LANGUAGE PatternSynonyms #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Pattern.Match (
    MatchResult (..),
    MatchType (..),
    FailReason (..),
    matchTerms,
    checkSubsort,
    SortError (..),
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.ByteString (ByteString)
import Data.Either.Extra
import Data.List (partition)
import Data.List.NonEmpty as NE (NonEmpty, fromList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Sequence (Seq, (><), pattern (:<|), pattern (:|>))
import Data.Sequence qualified as Seq

import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter

import Booster.Definition.Attributes.Base (KListDefinition, KMapDefinition, KSetDefinition)
import Booster.Definition.Base
import Booster.Pattern.Base
import Booster.Pattern.Pretty
import Booster.Pattern.Substitution qualified as Substitution
import Booster.Pattern.Util (
    checkSymbolIsAc,
    freeVariables,
    isConstructorSymbol,
    sortOfTerm,
 )

-- | Result of matching a pattern to a subject (a substitution or an indication of what went wrong)
data MatchResult
    = -- | equal structure (constructors) after substitution (substitution goes both ways)
      MatchSuccess Substitution
    | -- | different constructors or domain values, or sort mismatch
      MatchFailed FailReason
    | -- | (other) cases that are unresolved (offending case in head position).
      MatchIndeterminate (NonEmpty (Term, Term))
    deriving stock (Eq, Show)

data MatchType = Rewrite | Eval | Implies deriving (Eq)

-- | Additional information to explain why matching has failed
data FailReason
    = -- | (Domain) values differ
      DifferentValues Term Term
    | -- | Symbols differ
      DifferentSymbols Term Term
    | -- | The unificands have different sorts
      DifferentSorts Term Term
    | -- | Variable would refer to itself. This should not happen
      -- because we rename rule variables to avoid it.
      VariableRecursion Variable Term
    | -- | Variable reassigned
      VariableConflict Variable Term Term
    | -- | Key not found in map
      KeyNotFound Term Term
    | -- | Key not found in map
      DuplicateKeys Term Term
    | -- | Shared variables between matching terms
      SharedVariables (Set Variable)
    | -- | sort error (using variables or unknown sorts)
      SubsortingError SortError
    | -- | The two terms have differing argument lengths
      ArgLengthsDiffer Term Term
    | -- | Not a matching substitution
      SubjectVariableMatch Term Variable
    deriving stock (Eq, Show)

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods FailReason) where
    pretty (PrettyWithModifiers f) = case f of
        DifferentValues t1 t2 ->
            "Values differ:" <> align (sep [pretty' @mods t1, pretty' @mods t2])
        DifferentSymbols t1 t2 ->
            hsep ["Symbols differ:", pretty' @mods t1, "=/=", pretty' @mods t2] -- shorten?
        DifferentSorts t1 t2 ->
            hsep ["Sorts differ:", pretty' @mods t1, "=/=", pretty' @mods t2] -- shorten?
        VariableRecursion v t ->
            "Variable recursion found: " <> pretty' @mods v <> " in " <> pretty' @mods t
        VariableConflict v t1 t2 ->
            hsep
                [ "Variable conflict for " <> pretty' @mods v
                , pretty' @mods t1
                , pretty' @mods t2
                ]
        KeyNotFound k m ->
            hsep
                [ "Key " <> pretty' @mods k <> " not found in map"
                , pretty' @mods m
                ]
        DuplicateKeys k m ->
            hsep
                [ "Key " <> pretty' @mods k <> " appears more than once in map"
                , pretty' @mods m
                ]
        SharedVariables vs ->
            "Shared variables:" <+> hsep (map (pretty' @mods) $ Set.toList vs)
        SubsortingError err ->
            pretty $ show err
        ArgLengthsDiffer t1 t2 ->
            hsep ["Argument length differ", pretty' @mods t1, pretty' @mods t2]
        SubjectVariableMatch t v ->
            hsep ["Cannot match variable in subject:", pretty' @mods v, pretty' @mods t]

{- | Attempts to find a simple unifying substitution for the given
   terms.

   The returned substitution is oriented towards 'term1', i.e.,
   prefers to replace its variables if given a choice.

   TODO: This should not be the case and we need to re-factor the code further
   to ensure that we always produce a matching substitution without having to check
   after running the matcher
-}
matchTerms :: MatchType -> KoreDefinition -> Term -> Term -> MatchResult
matchTerms matchType KoreDefinition{sorts} term1 term2 =
    let runMatch :: MatchState -> MatchResult
        runMatch =
            fromEither
                . runExcept
                . fmap (MatchSuccess . mSubstitution)
                . execStateT (match matchType)
        freeVars1 = freeVariables term1
        freeVars2 = freeVariables term2
        sharedVars = freeVars1 `Set.intersection` freeVars2
     in if matchType /= Implies && (not $ Set.null sharedVars)
            then case matchType of
                Rewrite ->
                    MatchIndeterminate $
                        NE.fromList
                            [(Var v, Var v) | v <- Set.toList sharedVars]
                Eval -> MatchFailed $ SharedVariables sharedVars
                Implies -> error "unreachable"
            else
                runMatch
                    State
                        { mSubstitution = Map.empty
                        , mQueue = Seq.singleton (term1, term2) -- PriorityQueue.singleton (term1, term2) RegularTerm ()
                        , mMapQueue = mempty
                        , mIndeterminate = []
                        , mSubsorts = Map.map snd sorts
                        }

data MatchState = State
    { mSubstitution :: Substitution
    , mQueue :: Seq (Term, Term)
    , mMapQueue :: Seq (Term, Term)
    , mIndeterminate :: [(Term, Term)] -- list of postponed indeterminate terms (function results)
    , mSubsorts :: SortTable
    }

type SortTable = Map SortName (Set SortName)

match :: MatchType -> StateT MatchState (Except MatchResult) ()
match matchType = do
    queue <- gets mQueue
    mapQueue <- gets mMapQueue
    case queue of
        Seq.Empty -> case mapQueue of
            Seq.Empty -> checkIndeterminate -- done
            (term1, term2) :<| rest -> do
                modify $ \s -> s{mMapQueue = rest}
                match1 matchType term1 term2
                match matchType
        (term1, term2) :<| rest -> do
            modify $ \s -> s{mQueue = rest}
            match1 matchType term1 term2
            match matchType

checkIndeterminate :: StateT MatchState (Except MatchResult) ()
checkIndeterminate = do
    indeterminate <- gets mIndeterminate
    unless (null indeterminate) . lift $
        throwE (MatchIndeterminate $ NE.fromList indeterminate)
match1 ::
    MatchType ->
    Term ->
    Term ->
    StateT MatchState (Except MatchResult) ()
{- FOURMOLU_DISABLE -}
match1 Implies t1                                         t2                                         | t1 == t2 = pure ()
match1 Eval    t1@AndTerm{}                               t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _       (AndTerm t1a t1b)                          t2@AndTerm{}                               = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _       (AndTerm t1a t1b)                          t2@DomainValue{}                           = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _       (AndTerm t1a t1b)                          t2@Injection{}                             = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _       (AndTerm t1a t1b)                          t2@KMap{}                                  = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _       (AndTerm t1a t1b)                          t2@KList{}                                 = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _       (AndTerm t1a t1b)                          t2@KSet{}                                  = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _       (AndTerm t1a t1b)                          t2@ConsApplication{}                       = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 _       (AndTerm t1a t1b)                          t2@FunctionApplication{}                   = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 Eval    t1@AndTerm{}                               t2@Var{}                                   = addIndeterminate t1 t2
match1 _       (AndTerm t1a t1b)                          t2@Var{}                                   = enqueueRegularProblem t1a t2 >> enqueueRegularProblem t1b t2
match1 Eval    t1@DomainValue{}                           t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _       t1@DomainValue{}                           (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 _       (DomainValue s1 t1)                        (DomainValue s2 t2)                        = matchDV s1 t1 s2 t2
match1 _       t1@DomainValue{}                           t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 _       t1@DomainValue{}                           t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@DomainValue{}                           t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _       t1@DomainValue{}                           t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@DomainValue{}                           t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _       t1@DomainValue{}                           t2@FunctionApplication{}                   = addIndeterminate t1 t2
-- match with var on the RHS must be indeterminate when evaluating functions. see: https://github.com/runtimeverification/hs-backend-booster/issues/231
match1 Rewrite t1@DomainValue{}                           (Var t2)                                   = failWith $ SubjectVariableMatch t1 t2
match1 Implies t1@DomainValue{}                           (Var t2)                                   = failWith $ SubjectVariableMatch t1 t2
match1 Eval    t1@DomainValue{}                           t2@Var{}                                   = addIndeterminate t1 t2
match1 Eval    t1@Injection{}                             t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _       t1@Injection{}                             (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 _       t1@Injection{}                             t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 matchTy (Injection source1 target1 trm1)           (Injection source2 target2 trm2)           = matchInj matchTy source1 target1 trm1 source2 target2 trm2
match1 _       t1@Injection{}                             t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@Injection{}                             t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _       t1@Injection{}                             t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@Injection{}                             t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _       t1@Injection{}                             t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 _       t1@Injection{}                             t2@Var{}                                   = addIndeterminate t1 t2
match1 Eval    t1@KMap{}                                  t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _       t1@KMap{}                                  (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 _       t1@KMap{}                                  t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 Eval    t1@KMap{}                                  t2@Injection{}                             = addIndeterminate t1 t2
match1 _       t1@KMap{}                                  t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 _       t1@(KMap def1 patKeyVals patRest)          t2@(KMap def2 subjKeyVals subjRest)        = if def1 == def2 then matchMaps def1 patKeyVals patRest subjKeyVals subjRest else failWith $ DifferentSorts t1 t2
match1 _       t1@KMap{}                                  t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _       t1@KMap{}                                  t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@KMap{}                                  t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _       t1@KMap{}                                  t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Rewrite t1@KMap{}                                  (Var t2)                                   = failWith $ SubjectVariableMatch t1 t2
match1 _       t1@KMap{}                                  t2@Var{}                                   = addIndeterminate t1 t2
match1 Eval    t1@KList{}                                 t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _       t1@KList{}                                 (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 _       t1@KList{}                                 t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 Eval    t1@KList{}                                 t2@Injection{}                             = addIndeterminate t1 t2
match1 _       t1@KList{}                                 t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 _       t1@KList{}                                 t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 Eval    t1@KList{}                                 t2@KList{}                                 = addIndeterminate t1 t2
match1 _       t1@(KList def1 heads1 rest1)               t2@(KList def2 heads2 rest2)               = if def1 == def2 then matchLists def1 heads1 rest1 heads2 rest2 else failWith $ DifferentSorts t1 t2
match1 _       t1@KList{}                                 t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@KList{}                                 t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _       t1@KList{}                                 t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Rewrite t1@KList{}                                 (Var t2)                                   = failWith $ SubjectVariableMatch t1 t2
match1 _       t1@KList{}                                 t2@Var{}                                   = addIndeterminate t1 t2
match1 Eval    t1@KSet{}                                  t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _       t1@KSet{}                                  (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 _       t1@KSet{}                                  t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 Eval    t1@KSet{}                                  t2@Injection{}                             = addIndeterminate t1 t2
match1 _       t1@KSet{}                                  t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 _       t1@KSet{}                                  t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@KSet{}                                  t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _       t1@(KSet def1 patElements patRest)         t2@(KSet def2 subjElements subjRest)       = if def1 == def2 then matchSets def1 patElements patRest subjElements subjRest else failWith $ DifferentSorts t1 t2
match1 _       t1@KSet{}                                  t2@ConsApplication{}                       = failWith $ DifferentSymbols t1 t2
match1 _       t1@KSet{}                                  t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Rewrite t1@KSet{}                                  (Var t2)                                   = failWith $ SubjectVariableMatch t1 t2
match1 _       t1@KSet{}                                  t2@Var{}                                   = addIndeterminate t1 t2
match1 Eval    t1@ConsApplication{}                       t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _       t1@ConsApplication{}                       (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 _       t1@ConsApplication{}                       t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 _       t1@ConsApplication{}                       t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 _       t1@ConsApplication{}                       t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@ConsApplication{}                       t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _       t1@ConsApplication{}                       t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 matchTy (ConsApplication symbol1 sorts1 args1)     (ConsApplication symbol2 sorts2 args2)     = matchSymbolAplications matchTy symbol1 sorts1 args1 symbol2 sorts2 args2
match1 Eval    (ConsApplication symbol1 sorts1 args1)     (FunctionApplication symbol2 sorts2 args2) = matchSymbolAplications Eval symbol1 sorts1 args1 symbol2 sorts2 args2
match1 _       t1@ConsApplication{}                       t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Rewrite t1@ConsApplication{}                       (Var t2)                                   = failWith $ SubjectVariableMatch t1 t2
match1 _       t1@ConsApplication{}                       t2@Var{}                                   = addIndeterminate t1 t2
match1 Eval    t1@FunctionApplication{}                   t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _       t1@FunctionApplication{}                   (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 Eval    t1@FunctionApplication{}                   t2@DomainValue{}                           = failWith $ DifferentSymbols t1 t2
match1 _       t1@FunctionApplication{}                   t2@DomainValue{}                           = addIndeterminate t1 t2
match1 Eval    t1@FunctionApplication{}                   t2@Injection{}                             = failWith $ DifferentSymbols t1 t2
match1 _       t1@FunctionApplication{}                   t2@Injection{}                             = addIndeterminate t1 t2
match1 Eval    t1@FunctionApplication{}                   t2@KMap{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@FunctionApplication{}                   t2@KMap{}                                  = addIndeterminate t1 t2
match1 Eval    t1@FunctionApplication{}                   t2@KList{}                                 = failWith $ DifferentSymbols t1 t2
match1 _       t1@FunctionApplication{}                   t2@KList{}                                 = addIndeterminate t1 t2
match1 Eval    t1@FunctionApplication{}                   t2@KSet{}                                  = failWith $ DifferentSymbols t1 t2
match1 _       t1@FunctionApplication{}                   t2@KSet{}                                  = addIndeterminate t1 t2
match1 Eval    (FunctionApplication symbol1 sorts1 args1) (ConsApplication symbol2 sorts2 args2)     = matchSymbolAplications Eval symbol1 sorts1 args1 symbol2 sorts2 args2
match1 _       t1@FunctionApplication{}                   t2@ConsApplication{}                       = addIndeterminate t1 t2
match1 Eval    (FunctionApplication symbol1 sorts1 args1) (FunctionApplication symbol2 sorts2 args2) = matchSymbolAplications Eval symbol1 sorts1 args1 symbol2 sorts2 args2
match1 _       t1@FunctionApplication{}                   t2@FunctionApplication{}                   = addIndeterminate t1 t2
match1 Rewrite t1@FunctionApplication{}                   (Var t2)                                   = failWith $ SubjectVariableMatch t1 t2
match1 _       t1@FunctionApplication{}                   t2@Var{}                                   = addIndeterminate t1 t2
match1 Eval    t1@Var{}                                   t2@AndTerm{}                               = addIndeterminate t1 t2
match1 _       t1@Var{}                                   (AndTerm t2a t2b)                          = enqueueRegularProblem t1 t2a >> enqueueRegularProblem t1 t2b
match1 matchTy (Var var1)                                 t2@DomainValue{}                           = matchVar matchTy var1 t2
match1 matchTy (Var var1)                                 t2@Injection{}                             = matchVar matchTy var1 t2
match1 matchTy (Var var1)                                 t2@KMap{}                                  = matchVar matchTy var1 t2
match1 matchTy (Var var1)                                 t2@KList{}                                 = matchVar matchTy var1 t2
match1 matchTy (Var var1)                                 t2@KSet{}                                  = matchVar matchTy var1 t2
match1 matchTy (Var var1)                                 t2@ConsApplication{}                       = matchVar matchTy var1 t2
match1 matchTy (Var var1)                                 t2@FunctionApplication{}                   = matchVar matchTy var1 t2
match1 matchTy (Var var1)                                 t2@Var{}                                   = matchVar matchTy var1 t2
{- FOURMOLU_ENABLE -}

matchDV :: Sort -> ByteString -> Sort -> ByteString -> StateT s (Except MatchResult) ()
matchDV s1 t1 s2 t2 =
    do
        unless (t1 == t2) $
            failWith (DifferentValues (DomainValue s1 t1) (DomainValue s2 t2))
        unless (s1 == s2) $ -- sorts must be exactly the same for DVs
            failWith (DifferentSorts (DomainValue s1 t1) (DomainValue s2 t2))
{-# INLINE matchDV #-}

----- Injections
-- two injections. Try to match the contained terms if the sorts
-- agree. Target sorts must be the same, source sorts may differ if
-- the contained pattern term is just a variable, otherwise they need
-- to be identical.
matchInj ::
    MatchType ->
    Sort ->
    Sort ->
    Term ->
    Sort ->
    Sort ->
    Term ->
    StateT MatchState (Except MatchResult) ()
matchInj
    matchType
    source1
    target1
    trm1
    source2
    target2
    trm2
        | target1 /= target2 = do
            failWith (DifferentSorts (Injection source1 target1 trm1) (Injection source2 target2 trm2))
        | source1 == source2 = do
            enqueueRegularProblem trm1 trm2
        | Var v <- trm1 = do
            -- variable in pattern, check source sorts and bind
            subsorts <- gets mSubsorts
            isSubsort <-
                lift . withExcept (MatchFailed . SubsortingError) $
                    checkSubsort subsorts source2 source1
            if isSubsort
                then bindVariable matchType v (Injection source2 source1 trm2)
                else failWith (DifferentSorts trm1 trm2)
        | otherwise =
            failWith (DifferentSorts (Injection source1 target1 trm1) (Injection source2 target2 trm2))
{-# INLINE matchInj #-}

----- Symbol Applications
matchSymbolAplications ::
    MatchType ->
    Symbol ->
    [Sort] ->
    [Term] ->
    Symbol ->
    [Sort] ->
    [Term] ->
    StateT MatchState (Except MatchResult) ()
matchSymbolAplications
    Rewrite
    symbol1
    sorts1
    args1
    symbol2
    sorts2
    args2
        | symbol1.name /= symbol2.name =
            failWith
                ( DifferentSymbols (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
                )
        | length args1 /= length args2 =
            failWith $
                ArgLengthsDiffer (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
        | sorts1 /= sorts2 =
            failWith
                (DifferentSorts (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2))
        | otherwise =
            enqueueRegularProblems $ Seq.fromList $ zip args1 args2
matchSymbolAplications
    _
    symbol1
    sorts1
    args1
    symbol2
    sorts2
    args2
        | symbol1.name /= symbol2.name =
            if isConstructorSymbol symbol1 && isConstructorSymbol symbol2
                then
                    failWith
                        (DifferentSymbols (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2))
                else addIndeterminate (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
        | length args1 /= length args2 =
            failWith $
                ArgLengthsDiffer (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
        | sorts1 /= sorts2 =
            failWith
                (DifferentSorts (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2))
        -- If the symbol is non-free (AC symbol), return indeterminate
        | checkSymbolIsAc symbol1 =
            addIndeterminate (SymbolApplication symbol1 sorts1 args1) (SymbolApplication symbol2 sorts2 args2)
        | otherwise =
            enqueueRegularProblems $ Seq.fromList $ zip args1 args2

----- Variables

matchVar :: MatchType -> Variable -> Term -> StateT MatchState (Except MatchResult) ()
matchVar
    Implies
    -- twice the exact same variable: verify sorts are equal
    var1
    (Var var2)
        | var1 == var2 = pure ()
matchVar
    _
    -- twice the exact same variable: verify sorts are equal
    var1@(Variable varSort1 varName1)
    (Var var2@(Variable varSort2 varName2))
        | varName1 == varName2 && varSort1 /= varSort2 =
            -- sorts differ, names equal: error!
            failWith $ VariableConflict var1 (Var var1) (Var var2)
matchVar
    -- term1 variable (target): introduce a new binding
    matchType
    var@Variable{variableSort}
    term2 =
        do
            let termSort = sortOfTerm term2
            subsorts <- gets mSubsorts
            isSubsort <-
                lift . withExcept (MatchFailed . SubsortingError) $
                    checkSubsort subsorts termSort variableSort
            if isSubsort
                then
                    bindVariable matchType var $
                        if termSort == variableSort
                            then term2
                            else Injection termSort variableSort term2
                else failWith $ DifferentSorts (Var var) term2

{- | pair up the argument lists and return the pairs in the first argument. If the lists
are of equal length, return Nothing in second, else return the remaining
terms in the longer list, tagged with their origin).
-}
mkPairs :: [a] -> [a] -> ([(a, a)], Maybe (Either [a] [a]))
mkPairs ts1 ts2
    | l1 == l2 =
        (zip ts1 ts2, Nothing)
    | l1 > l2 =
        let (ts1', rest1) = splitAt l2 ts1
         in (zip ts1' ts2, Just $ Left rest1)
    | otherwise -- l1 < l2
        =
        let (ts2', rest2) = splitAt l1 ts2
         in (zip ts1 ts2', Just $ Right rest2)
  where
    l1 = length ts1
    l2 = length ts2

{- | pair up the tails of argument lists by first reversing the lists and pairing them up using `mkPairs` and
then reversing the resulting paired up list as well as reversing any potential remainders.
-}
mkTailPairs :: [a] -> [a] -> ([(a, a)], Maybe (Either [a] [a]))
mkTailPairs ts1 ts2 =
    let (matchedTailReversed, remainderReversed) = mkPairs (reverse ts1) (reverse ts2)
     in (reverse matchedTailReversed, bimap reverse reverse <$> remainderReversed)

matchLists ::
    KListDefinition ->
    [Term] ->
    Maybe (Term, [Term]) ->
    [Term] ->
    Maybe (Term, [Term]) ->
    StateT MatchState (Except MatchResult) ()
matchLists
    def
    heads1
    rest1
    heads2
    rest2 =
        do
            let (matchedConcrete, remainderHeads) = mkPairs heads1 heads2
            rest <- matchListRests rest1 rest2 remainderHeads
            enqueueRegularProblems $ (Seq.fromList matchedConcrete) >< rest
      where
        emptyList = KList def [] Nothing

        noRemainderList xs = KList def xs Nothing

        matchListRests ::
            Maybe (Term, [Term]) ->
            Maybe (Term, [Term]) ->
            Maybe (Either [Term] [Term]) ->
            StateT MatchState (Except MatchResult) (Seq (Term, Term))
        matchListRests Nothing Nothing = \case
            -- match [] with []
            Nothing -> pure mempty
            -- match [X, Y, ...] with []
            Just (Left hs1) -> failWith $ DifferentValues (KList def hs1 Nothing) emptyList
            -- match [] with [X, Y, ...]
            Just (Right hs2) -> failWith $ DifferentValues emptyList (KList def hs2 Nothing)
        matchListRests Nothing t2@Just{} = \headRemainders ->
            -- match [] with [...REST, X, Y, ...]
            failWith $ uncurry DifferentValues $ case headRemainders of
                Nothing -> (emptyList, KList def [] t2)
                Just (Left hs1) -> (KList def hs1 Nothing, KList def [] t2)
                Just (Right hs2) -> (emptyList, KList def hs2 t2)
        matchListRests t1@(Just (mid, ts1)) Nothing = \case
            Nothing -> case ts1 of
                -- match [...REST] with []
                [] -> pure $ Seq.singleton (mid, emptyList)
                -- match [...REST, X, Y, ...] with []
                _ -> failWith $ DifferentValues (KList def [] t1) emptyList
            Just (Left hs1) ->
                -- match [X, Y, ..., ...REST, ...] with []
                failWith $ DifferentValues (KList def hs1 t1) emptyList
            Just (Right hs2) ->
                -- match [...REST, ...] with [X', Y', ...]
                let (zippedBack, remainderBack) = mkTailPairs ts1 hs2
                 in case remainderBack of
                        -- match [...REST, X1, ..., Xn] with [X'1, ..., X'n]
                        -- succeed matching [...REST] with [] and [X1, ..., Xn] with [X'1, ..., X'n]
                        Nothing -> pure $ Seq.fromList $ (mid, emptyList) : zippedBack
                        -- match [...REST, X'1, ..., X'm, X1, ..., Xn] with [X'1, ..., X'n]
                        -- fail matching [...REST, X'1, ..., X'm] with []
                        Just (Left ts1') -> failWith $ DifferentValues (KList def [] (Just (mid, ts1'))) emptyList
                        -- match [...REST, X1, ..., Xn] with [X'1, ..., X'm, X'1, ..., X'n]
                        -- succeed matching [...REST] with [X'1, ..., X'm] and [X1, ..., Xn] with [X'1, ..., X'n]
                        Just (Right ts2') -> pure $ Seq.fromList $ (mid, noRemainderList ts2') : zippedBack
        matchListRests t1@(Just (mid1, ts1)) t2@(Just (mid2, ts2)) = \case
            Nothing ->
                -- match [...REST, ...] with [...REST', ...]
                let (zippedBack, remainderBack) = mkTailPairs ts1 ts2
                 in case remainderBack of
                        -- match [...REST, X1, ..., Xn] with [...REST', X'1, ..., X'n]
                        -- succeed matching [...REST] with [...REST'] and [X1, ..., Xn] with [X'1, ..., X'n]
                        Nothing ->
                            pure $ Seq.fromList $ (mid1, mid2) : zippedBack
                        -- match [...REST, X'1, ..., X'm, X1, ..., Xn] with [...REST', X'1, ..., X'n]
                        -- indeterminate matching [...REST, X'1, ..., X'm] with [...REST']
                        Just (Left ts1') ->
                            addIndeterminate (KList def [] (Just (mid1, ts1'))) (KList def [] (Just (mid2, []))) >> pure mempty
                        -- match [...REST, X1, ..., Xn] with [...REST', X'1, ..., X'm, X'1, ..., X'n]
                        -- succeed matching [...REST] with [...REST', X'1, ..., X'm] and [X1, ..., Xn] with [X'1, ..., X'n]
                        Just (Right ts2') ->
                            pure $ Seq.fromList $ (mid1, KList def [] (Just (mid2, ts2'))) : zippedBack
            Just (Left hs1) -> do
                -- match [X1,...,Xk,...REST, ...] with [...REST', ...]
                -- indeterminate
                addIndeterminate (KList def hs1 t1) (KList def [] t2) >> pure mempty
            Just (Right hs2) ->
                -- match [...REST, ...] with [X'1,...,X'k,...REST', ...]
                let (zippedBack, remainderBack) = mkTailPairs ts1 ts2
                 in case remainderBack of
                        -- match [...REST, X1, ..., Xn] with [X'1,...,X'k,...REST', X'1, ..., X'n]
                        -- succeed matching [...REST] with [X'1,...,X'k,...REST'] and [X1, ..., Xn] with [X'1, ..., X'n]
                        Nothing ->
                            pure $ Seq.fromList $ (mid1, KList def hs2 (Just (mid2, []))) : zippedBack
                        -- match [...REST, X'1, ..., X'm, X1, ..., Xn] with [X'1,...,X'k,...REST', X'1, ..., X'n]
                        -- indeterminate matching [...REST, X'1, ..., X'm] with [X'1,...,X'k,...REST']
                        Just (Left ts1') ->
                            addIndeterminate (KList def [] (Just (mid1, ts1'))) (KList def hs2 (Just (mid2, []))) >> pure mempty
                        -- match [...REST, X1, ..., Xn] with [X'1,...,X'k,...REST', X'1, ..., X'm, X'1, ..., X'n]
                        -- succeed matching [...REST] with [X'1,...,X'k,...REST', X'1, ..., X'm] and [X1, ..., Xn] with [X'1, ..., X'n]
                        Just (Right ts2') ->
                            pure $ Seq.fromList $ (mid1, KList def hs2 (Just (mid2, ts2'))) : zippedBack
{-# INLINE matchLists #-}

------ Internalised Maps
data RemainderMap
    = ConstructorKey Term Term RemainderMap
    | Rest RestRemainderMap

pattern SingleConstructorKey :: Term -> Term -> RemainderMap
pattern SingleConstructorKey k v = ConstructorKey k v (Rest EmptyMap)

pattern SingleOtherKey :: Term -> Term -> RemainderMap
pattern SingleOtherKey k v = Rest (OtherKey k v EmptyMap)

data RestRemainderMap
    = OtherKey Term Term RestRemainderMap
    | EmptyMap
    | Remainder Term

toRemainderMap :: [(Term, Term)] -> Maybe Term -> RemainderMap
toRemainderMap kvs rest =
    let (conc, sym) = partitionConcreteKeys kvs
     in foldr
            (uncurry ConstructorKey)
            (Rest $ foldr (uncurry OtherKey) (maybe EmptyMap Remainder rest) sym)
            conc
  where
    partitionConcreteKeys :: [(Term, Term)] -> ([(Term, Term)], [(Term, Term)])
    partitionConcreteKeys = partition (\(Term attrs _, _) -> attrs.isConstructorLike)

fromRemainderMap :: KMapDefinition -> RemainderMap -> Term
fromRemainderMap def ml = uncurry (KMap def) $ recurse ml
  where
    recurse (ConstructorKey k v rest) = first ((k, v) :) $ recurse rest
    recurse (Rest s) = recurseS s

    recurseS (OtherKey k v rest) = first ((k, v) :) $ recurseS rest
    recurseS EmptyMap = ([], Nothing)
    recurseS (Remainder t) = ([], Just t)

hasNoRemainder :: RestRemainderMap -> Bool
hasNoRemainder = \case
    OtherKey _ _ r -> hasNoRemainder r
    EmptyMap -> True
    Remainder{} -> False

containsOtherKeys :: RemainderMap -> Bool
containsOtherKeys = \case
    ConstructorKey _ _ rest -> containsOtherKeys rest
    Rest OtherKey{} -> True
    Rest _ -> False

------ Internalised Sets
matchSets ::
    KSetDefinition ->
    [Term] ->
    Maybe Term ->
    [Term] ->
    Maybe Term ->
    StateT MatchState (Except MatchResult) ()
matchSets
    def
    patElements
    patRest
    subjElements
    subjRest = do
        -- match only empty sets, indeterminate otherwise
        if null patElements && null subjElements && isNothing patRest && isNothing subjRest
            then pure ()
            else addIndeterminate (KSet def patElements patRest) (KSet def subjElements subjRest)

------ Internalised Maps
matchMaps ::
    KMapDefinition ->
    [(Term, Term)] ->
    Maybe Term ->
    [(Term, Term)] ->
    Maybe Term ->
    StateT MatchState (Except MatchResult) ()
matchMaps
    def
    patKeyVals
    patRest
    subjKeyVals
    subjRest = do
        st <- get
        if not (Seq.null st.mQueue)
            then -- delay matching 'KMap's until there are no regular
            -- problems left, to obtain a maximal prior substitution
            -- before matching map keys.
                enqueueMapProblem (KMap def patKeyVals patRest) (KMap def subjKeyVals subjRest)
            else do
                let patternKeyVals = map (first (Substitution.substituteInTerm st.mSubstitution)) patKeyVals
                -- check for duplicate keys
                checkDuplicateKeys patternKeyVals patRest
                checkDuplicateKeys subjKeyVals subjRest

                let patMap = Map.fromList patternKeyVals
                    subjMap = Map.fromList subjKeyVals
                    -- handles syntactically identical keys in pattern and subject
                    commonMap = Map.intersectionWith (,) patMap subjMap
                    patExtra = patMap `Map.withoutKeys` Map.keysSet commonMap
                    subjExtra = subjMap `Map.withoutKeys` Map.keysSet commonMap

                rest <-
                    matchRemainderMaps
                        (toRemainderMap (Map.toList patExtra) patRest)
                        (toRemainderMap (Map.toList subjExtra) subjRest)
                enqueueRegularProblems $ (Seq.fromList $ Map.elems commonMap) >< rest
      where
        checkDuplicateKeys assocs rest =
            let duplicates =
                    Map.filter (> (1 :: Int)) $ foldr (flip (Map.insertWith (+)) 1 . fst) mempty assocs
             in case Map.assocs duplicates of
                    [] -> pure ()
                    (k, _) : _ -> failWith $ DuplicateKeys k $ KMap def assocs rest

        -- This function takes the remaining keys and a potential "...REST" term (usually a variable or a function) of the two maps being matched,
        -- after the common keys have been identified.
        -- The remainder maps are encoded in the `RemainderMap` type, which is a list encoding two types of keys, either a `ConstructorKey k ...`,
        -- where `k` is a constructorLike (made up of only domain values or constructors) or `OtherKey k`, where `k` is e.g. a function symbol or a variable.
        -- the key/value pairs are ordered so that the constructor-like keys come before all other keys and the "...REST" term.
        matchRemainderMaps ::
            RemainderMap -> RemainderMap -> StateT MatchState (Except MatchResult) (Seq (Term, Term))
        -- match {K -> V, ...} with {...} where K is constructor-like
        -- if `{...}` does not contain `OtherKeys`, fail because we already matched all concrete keys, so we know `K` does not appear in {...}
        -- otherwise, one of the other keys could potentially match `K`, if `{...}` contains some OtherKey `f()` which evaluates to `K`
        matchRemainderMaps pat@(ConstructorKey patKey _ _) subj
            | not (containsOtherKeys subj) = failWith $ KeyNotFound patKey (fromRemainderMap def subj)
            | otherwise = do
                addIndeterminate (fromRemainderMap def pat) (fromRemainderMap def subj) >> pure mempty
        -- match {} with {}
        -- succeeds
        matchRemainderMaps (Rest EmptyMap) (Rest EmptyMap) = pure mempty
        -- match {} with {...REST} or {K -> V, ...}
        -- fails as the size of the maps is different and there is no substitution `subst`, s.t.
        -- subst({}) = {...REST} or {K -> V, ...}
        matchRemainderMaps (Rest EmptyMap) subj =
            failWith $
                DifferentSymbols (fromRemainderMap def (Rest EmptyMap)) (fromRemainderMap def subj)
        -- match {K -> V} with {K' -> V'} where K' is constructor-like and K is not (i.e. a variable or function)
        -- we can proceed matching because each map only has one element, so the two key/value pairs must match
        matchRemainderMaps (SingleOtherKey patKey patVal) (SingleConstructorKey subjKey subjVal) =
            pure $ Seq.fromList [(patKey, subjKey), (patVal, subjVal)]
        -- match {K -> V} with {K' -> V'} where K and K' are both non-constructor like (i.e. a variable or function)
        -- same arguemnt as above
        matchRemainderMaps (SingleOtherKey patKey patVal) (SingleOtherKey subjKey subjVal) =
            pure $ Seq.fromList [(patKey, subjKey), (patVal, subjVal)]
        -- match {K -> V, ...} with {}
        -- fails, maps are different sizes
        matchRemainderMaps pat@(Rest OtherKey{}) subj@(Rest EmptyMap) =
            failWith $ DifferentSymbols (fromRemainderMap def pat) (fromRemainderMap def subj)
        -- match {K -> V, ...} with {...REST} where {K -> V, ...} has no remainder and ...REST is a map variable
        -- fail because there is no substitution `sub` such that sub({K -> V, ...}) = {...REST}
        matchRemainderMaps (Rest pat@OtherKey{}) subj@(Rest (Remainder Var{}))
            | hasNoRemainder pat =
                failWith $ DifferentSymbols (fromRemainderMap def (Rest pat)) (fromRemainderMap def subj)
        -- match {K -> V, ...} with {...} where K is a function/variable
        -- all other cases are indeterminate
        matchRemainderMaps pat@(Rest OtherKey{}) subj = do
            addIndeterminate (fromRemainderMap def pat) (fromRemainderMap def subj) >> pure mempty
        -- match {...REST} with {...}
        -- succeeds as `...REST` is a variable of sort map or a function which evaluates to a map
        matchRemainderMaps (Rest (Remainder pat)) subj = pure $ Seq.singleton (pat, fromRemainderMap def subj)
{-# INLINE matchMaps #-}

failWith :: FailReason -> StateT s (Except MatchResult) a
failWith = lift . throwE . MatchFailed

enqueueRegularProblem, enqueueMapProblem :: Monad m => Term -> Term -> StateT MatchState m ()
enqueueRegularProblem term1 term2 =
    modify $ \s@State{mQueue} ->
        s
            { mQueue = mQueue :|> (term1, term2)
            }
enqueueMapProblem term1 term2 =
    modify $ \s@State{mMapQueue} ->
        s
            { mMapQueue = mMapQueue :|> (term1, term2)
            }

enqueueRegularProblems :: Monad m => Seq (Term, Term) -> StateT MatchState m ()
enqueueRegularProblems ts =
    modify $ \s@State{mQueue} -> s{mQueue = mQueue >< ts}

{- | Binds a variable to a term to add to the resulting unifier.

 We apply the accumulated substitution whenever a new variable
 binding to a term is added. This avoids repeated traversals while
 guarding against substitution loops.
-}
bindVariable :: MatchType -> Variable -> Term -> StateT MatchState (Except MatchResult) ()
bindVariable matchType var term@(Term termAttrs _) = do
    State{mSubstitution = currentSubst} <- get
    case Map.lookup var currentSubst of
        Just oldTerm@(Term oldTermAttrs _)
            | oldTerm == term -> pure () -- already bound
            | termAttrs.isConstructorLike
            , oldTermAttrs.isConstructorLike ->
                failWith $ VariableConflict var oldTerm term
            | otherwise ->
                -- the term in the binding could be _equivalent_
                -- (not necessarily syntactically equal) to term'
                case matchType of
                    Rewrite -> addIndeterminate oldTerm term
                    _ -> failWith $ VariableConflict var oldTerm term
        Nothing -> do
            let
                -- apply existing substitutions to term
                term' = Substitution.substituteInTerm currentSubst term
            when (var `Set.member` freeVariables term') $
                failWith (VariableRecursion var term)
            let
                -- substitute in existing substitution terms
                currentSubst' =
                    Map.map (Substitution.substituteInTerm $ Map.singleton var term') currentSubst
                newSubst = Map.insert var term' currentSubst'
            modify $ \s -> s{mSubstitution = newSubst}

addIndeterminate :: Term -> Term -> StateT MatchState (Except MatchResult) ()
addIndeterminate pat subj =
    modify $ \s -> s{mIndeterminate = (pat, subj) : s.mIndeterminate}

{- | Matches a subject sort to a pattern sort, ensuring that the subject
   sort can be used in place of the pattern sort, i.e., is a subsort.

Sort variables are only accepted if they are syntactically identical.
They should not occur in the patterns matched here, and should
not be sent by clients either.
-}
checkSubsort :: SortTable -> Sort -> Sort -> Except SortError Bool
checkSubsort subsorts sub sup
    | sub == sup = pure True
    | SortVar s <- sub = throwE $ FoundSortVariable s
    | SortVar s <- sup = throwE $ FoundSortVariable s
    | SortApp subName subArgs <- sub
    , SortApp supName supArgs <- sup =
        case Map.lookup supName subsorts of
            Nothing ->
                throwE $ FoundUnknownSort sup
            Just result
                | not (subName `Set.member` result) -> pure False
                | otherwise -> do
                    argsCheck <- zipWithM (checkSubsort subsorts) subArgs supArgs
                    pure $ and argsCheck

data SortError
    = FoundSortVariable VarName
    | FoundUnknownSort Sort
    deriving (Eq, Show)

instance FromModifiersT mods => Pretty (PrettyWithModifiers mods SortError) where
    pretty (PrettyWithModifiers e) = case e of
        FoundSortVariable v -> "Found sort variable" <+> pretty (show v)
        FoundUnknownSort s -> "Found unknown sort" <+> pretty' @mods s
