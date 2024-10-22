{- |
Copyright   : (c) Runtime Verification, 2024
License     : BSD-3-Clause

Function to manipulate @'Substitution'@s and convert to/from @'Predicate'@s

This module is intended to be imported qualified.
-}
module Booster.Pattern.Substitution (
    mkEq,
    asEquations,
    compose,
    substituteInPredicate,
    substituteInTerm,
) where

import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

import Booster.Pattern.Base
import Booster.Pattern.Bool
import Booster.Pattern.Util (sortOfTerm)

{- | Compose substitutions:
     - apply the left one to the assignments in the right one
     - merge left with the result of above, left takes priority
-}
compose :: Substitution -> Substitution -> Substitution
compose newSubst oldSubst =
    let substitutedOldSubst = Map.map (substituteInTerm newSubst) oldSubst
     in (newSubst `Map.union` substitutedOldSubst) -- new bindings take priority

-- | Build an equality predicate form a variable assignment
mkEq :: Variable -> Term -> Predicate
mkEq x t = Predicate $ case sortOfTerm t of
    SortInt -> EqualsInt (Var x) t
    SortBool -> EqualsBool (Var x) t
    otherSort -> EqualsK (KSeq otherSort (Var x)) (KSeq otherSort t)

-- | turns a substitution into a list of equations
asEquations :: Map Variable Term -> [Predicate]
asEquations = map (uncurry mkEq) . Map.assocs

substituteInTerm :: Substitution -> Term -> Term
substituteInTerm substitution = goSubst
  where
    targetSet = Map.keysSet substitution
    goSubst t
        | Set.null (targetSet `Set.intersection` (getAttributes t).variables) = t
        | otherwise = case t of
            Var v -> fromMaybe t (Map.lookup v substitution)
            DomainValue{} -> t
            SymbolApplication sym sorts args ->
                SymbolApplication sym sorts $ map goSubst args
            AndTerm t1 t2 -> AndTerm (goSubst t1) (goSubst t2)
            Injection ss s sub -> Injection ss s (goSubst sub)
            KMap attrs keyVals rest -> KMap attrs (bimap goSubst goSubst <$> keyVals) (goSubst <$> rest)
            KList def heads rest ->
                KList def (map goSubst heads) (bimap goSubst (map goSubst) <$> rest)
            KSet def elements rest ->
                KSet def (map goSubst elements) (goSubst <$> rest)

substituteInPredicate :: Substitution -> Predicate -> Predicate
substituteInPredicate substitution = coerce . substituteInTerm substitution . coerce
