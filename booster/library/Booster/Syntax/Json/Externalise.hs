{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.Syntax.Json.Externalise (
    externalisePattern,
    externalisePredicate,
    externaliseCeil,
    externaliseSubstitution,
    externaliseSort,
    externaliseTerm,
    externaliseExistTerm,
) where

import Data.Foldable ()
import Data.Set qualified as Set
import Data.Text.Encoding qualified as Text

import Booster.Pattern.Base (externaliseKmapUnsafe)
import Booster.Pattern.Base qualified as Internal
import Booster.Pattern.Bool qualified as Internal
import Booster.Pattern.Substitution qualified as Substitution
import Booster.Pattern.Util (sortOfTerm)
import Data.Map (Map)
import Data.Map qualified as Map
import Kore.Syntax.Json.Types qualified as Syntax

{- | Converts an internal pattern to a triple of term, predicate and substitution in
 external format. The predicate and substitution are 'And'ed to avoid leaking
 Json format internals to the caller.
-}
externalisePattern ::
    Internal.Pattern ->
    Map Internal.Variable Internal.Term ->
    (Syntax.KorePattern, Maybe Syntax.KorePattern, Maybe Syntax.KorePattern)
externalisePattern Internal.Pattern{term = term, constraints, ceilConditions, substitution = ensuredSubstitution} inputSubstitution =
    -- need a sort for the predicates in external format
    let sort = externaliseSort $ sortOfTerm term
        -- inputSubstitution is probably not needed here at all
        substitutions = ensuredSubstitution `Substitution.compose` inputSubstitution
        externalisedSubstitution =
            if null substitutions
                then Nothing
                else Just . multiAnd sort . map (uncurry $ externaliseSubstitution sort) . Map.toList $ substitutions
        externalisedPredicate =
            if null constraints && null ceilConditions
                then Nothing
                else
                    Just $
                        multiAnd sort $
                            map (externalisePredicate sort) (Set.toList constraints)
                                ++ map (externaliseCeil sort) ceilConditions
     in (externaliseTerm term, externalisedPredicate, externalisedSubstitution)
  where
    multiAnd :: Syntax.Sort -> [Syntax.KorePattern] -> Syntax.KorePattern
    multiAnd _ [] = error "multiAnd: empty"
    multiAnd _ [p] = p
    multiAnd sort ps = Syntax.KJAnd sort ps

-- TODO: should KorePattern be the only type with an actual Unparse instance?
externaliseTerm :: Internal.Term -> Syntax.KorePattern
externaliseTerm = \case
    Internal.AndTerm first' second' ->
        Syntax.KJAnd
            (externaliseSort $ sortOfTerm second')
            [ externaliseTerm first'
            , externaliseTerm second'
            ]
    Internal.SymbolApplication symbol sorts args ->
        Syntax.KJApp
            (symbolNameToId symbol.name)
            (map externaliseSort sorts)
            (map externaliseTerm args)
    Internal.DomainValue sort bs ->
        Syntax.KJDV (externaliseSort sort) $ Text.decodeLatin1 bs
    Internal.Var Internal.Variable{variableSort = iSort, variableName = iName} ->
        Syntax.KJEVar (varNameToId iName) (externaliseSort iSort)
    Internal.Injection source target trm ->
        Syntax.KJApp
            (symbolNameToId Internal.injectionSymbol.name)
            (map externaliseSort [source, target])
            [externaliseTerm trm]
    Internal.KMap def keyVals rest -> externaliseTerm $ externaliseKmapUnsafe def keyVals rest
    Internal.KList def heads rest ->
        externaliseTerm $ Internal.externaliseKList def heads rest
    Internal.KSet def heads rest ->
        externaliseTerm $ Internal.externaliseKSet def heads rest

externaliseExistTerm :: [Internal.Variable] -> Internal.Term -> Syntax.KorePattern
externaliseExistTerm vars t = exist vars
  where
    sort = externaliseSort $ sortOfTerm t

    exist [] = externaliseTerm t
    exist (Internal.Variable{variableSort = iSort, variableName = iName} : vs) =
        Syntax.KJExists
            { sort
            , var = varNameToId iName
            , varSort = externaliseSort iSort
            , arg = exist vs
            }

externalisePredicate :: Syntax.Sort -> Internal.Predicate -> Syntax.KorePattern
externalisePredicate sort (Internal.Predicate t) =
    Syntax.KJEquals
        { argSort = externaliseSort $ sortOfTerm t
        , sort
        , first = externaliseTerm Internal.TrueBool
        , second = externaliseTerm t
        }

externaliseCeil :: Syntax.Sort -> Internal.Ceil -> Syntax.KorePattern
externaliseCeil sort (Internal.Ceil term) =
    Syntax.KJCeil
        { argSort = externaliseSort $ sortOfTerm term
        , sort
        , arg = externaliseTerm term
        }

externaliseSubstitution :: Syntax.Sort -> Internal.Variable -> Internal.Term -> Syntax.KorePattern
externaliseSubstitution sort Internal.Variable{variableSort = iSort, variableName = iName} t =
    Syntax.KJEquals
        { argSort = externaliseSort $ sortOfTerm t
        , sort
        , first = Syntax.KJEVar (varNameToId iName) (externaliseSort iSort)
        , second = externaliseTerm t
        }

varNameToId :: Internal.VarName -> Syntax.Id
varNameToId = Syntax.Id . Text.decodeLatin1

sortNameToId :: Internal.SortName -> Syntax.Id
sortNameToId = Syntax.Id . Text.decodeLatin1

symbolNameToId :: Internal.SymbolName -> Syntax.Id
symbolNameToId = Syntax.Id . Text.decodeLatin1

-- | converts an internal sort to an external one
externaliseSort :: Internal.Sort -> Syntax.Sort
externaliseSort (Internal.SortApp name args) =
    Syntax.SortApp (sortNameToId name) (map externaliseSort args)
externaliseSort (Internal.SortVar name) =
    Syntax.SortVar $ sortNameToId name
