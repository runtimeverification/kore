{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Rewrite.AntiLeft (
    AntiLeft (..),
    antiLeftPredicate,
    forgetSimplified,
    mapVariables,
    parse,
    toTermLike,
) where

import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Debug (
    Debug,
    Diff,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables (freeVariables),
    bindVariables,
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables (
    toNames,
 )
import Kore.Internal.Predicate (
    Predicate,
    makeAndPredicate,
    makeCeilPredicate,
    makeMultipleExists,
    makeMultipleOrPredicate,
    makeOrPredicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.TermLike (
    TermLike,
    mkAnd,
    mkElemVar,
    mkVar,
    pattern And_,
    pattern ApplyAlias_,
    pattern Bottom_,
    pattern Exists_,
    pattern Or_,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Internal.Variable (
    InternalVariable,
 )
import Kore.Simplify.ExpandAlias (
    substituteInAlias,
 )
import Kore.Substitute
import Kore.Syntax.Variable (
    AdjSomeVariableName,
    ElementVariable,
    SomeVariableName (SomeVariableNameElement),
    Variable (variableName),
    mapElementVariable,
 )
import Kore.Variables.Fresh (
    refreshElementVariable,
 )
import Prelude.Kore

{- | The part of an antileft pattern corresponding to a single rule.

Normally an AntiLeftLhs pattern is an alias that expands to

@
exists x1 . exists x2 . ... exists xn . lhsTermAlias(x1, ..., xn)
@

where the lhsTermAlias expands to

@
and(lhsPredicate, lhsTerm)
@

As an example, for a rule

@
f(X) => g(X) requires x < 5
@

the fully expanded AntiLeftLhs pattern would be

@
exists X . and(X<5, f(X))
@
-}
data AntiLeftLhs variable = AntiLeftLhs
    { existentials :: ![ElementVariable variable]
    , predicate :: !(Predicate variable)
    , term :: !(TermLike variable)
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (NFData, Hashable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance InternalVariable variable => Substitute (AntiLeftLhs variable) where
    type TermType (AntiLeftLhs variable) = TermLike variable

    type VariableNameType (AntiLeftLhs variable) = variable

    substitute subst antiLeft@(AntiLeftLhs _ _ _) =
        AntiLeftLhs
            { existentials
            , predicate = substitute subst' predicate
            , term = substitute subst' term
            }
      where
        AntiLeftLhs{existentials, predicate, term} = antiLeft
        subst' =
            foldl'
                (flip Map.delete)
                subst
                (map (SomeVariableNameElement . variableName) existentials)

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

{- | Expansion of an antileft alias.

Note that the alias usually occurs under a `not`, which is not represented here.

An antileft alias expands to

@
or(nextAntiLeft, or(lhs1, or(lhs2, ... or(lhsn, bottom) ... )))
@

where nextAntiLeft is optional. The lhsi patterns correspund to rules that have
the same priority. If present, the nextAntiLeft corrensponds to rules with
higher priority (i.e. lower priority number)
-}
data AntiLeft variable = AntiLeft
    { -- | The alias that was expanded.
      aliasTerm :: !(TermLike variable)
    , -- | The nextAntiLeft, if any.
      maybeInner :: !(Maybe (AntiLeft variable))
    , -- | patterns corresponding to rules with the same priority number.
      leftHands :: ![AntiLeftLhs variable]
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (NFData, Hashable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance
    InternalVariable variable =>
    HasFreeVariables (AntiLeft variable) variable
    where
    freeVariables antiLeft@(AntiLeft _ _ _) =
        freeVariables aliasTerm
            <> fold
                ( toList (freeVariables <$> maybeInner)
                    ++ map freeVariables leftHands
                )
      where
        AntiLeft{aliasTerm, maybeInner, leftHands} = antiLeft

instance
    InternalVariable variable =>
    HasFreeVariables (AntiLeftLhs variable) variable
    where
    freeVariables antiLeft@(AntiLeftLhs _ _ _) =
        bindVariables
            (map (fmap SomeVariableNameElement) existentials)
            ( freeVariables predicate
                <> freeVariables term
            )
      where
        AntiLeftLhs{existentials, predicate, term} = antiLeft

instance InternalVariable variable => Substitute (AntiLeft variable) where
    type TermType (AntiLeft variable) = TermLike variable

    type VariableNameType (AntiLeft variable) = variable

    substitute subst antiLeft@(AntiLeft _ _ _) =
        AntiLeft
            { aliasTerm = substitute subst aliasTerm
            , maybeInner = substitute subst <$> maybeInner
            , leftHands = map (substitute subst) leftHands
            }
      where
        AntiLeft{aliasTerm, maybeInner, leftHands} = antiLeft

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

mapVariables ::
    (InternalVariable variable1, InternalVariable variable2) =>
    AdjSomeVariableName (variable1 -> variable2) ->
    AntiLeft variable1 ->
    AntiLeft variable2
mapVariables adj antiLeft@(AntiLeft _ _ _) =
    AntiLeft
        { aliasTerm = TermLike.mapVariables adj aliasTerm
        , maybeInner = mapVariables adj <$> maybeInner
        , leftHands = map (mapVariablesLeft adj) leftHands
        }
  where
    AntiLeft{aliasTerm, maybeInner, leftHands} = antiLeft

mapVariablesLeft ::
    (InternalVariable variable1, InternalVariable variable2) =>
    AdjSomeVariableName (variable1 -> variable2) ->
    AntiLeftLhs variable1 ->
    AntiLeftLhs variable2
mapVariablesLeft adj antiLeft@(AntiLeftLhs _ _ _) =
    AntiLeftLhs
        { existentials = map (mapElementVariable adj) existentials
        , predicate = Predicate.mapVariables adj predicate
        , term = TermLike.mapVariables adj term
        }
  where
    AntiLeftLhs{existentials, predicate, term} = antiLeft

forgetSimplified ::
    InternalVariable variable =>
    AntiLeft variable ->
    AntiLeft variable
forgetSimplified antiLeft@(AntiLeft _ _ _) =
    AntiLeft
        { aliasTerm = TermLike.forgetSimplified aliasTerm
        , maybeInner = forgetSimplified <$> maybeInner
        , leftHands = map forgetSimplifiedLeft leftHands
        }
  where
    AntiLeft{aliasTerm, maybeInner, leftHands} = antiLeft

forgetSimplifiedLeft ::
    InternalVariable variable =>
    AntiLeftLhs variable ->
    AntiLeftLhs variable
forgetSimplifiedLeft antiLeftLhs@(AntiLeftLhs _ _ _) =
    AntiLeftLhs
        { existentials
        , predicate = Predicate.forgetSimplified predicate
        , term = TermLike.forgetSimplified term
        }
  where
    AntiLeftLhs{existentials, predicate, term} = antiLeftLhs

toTermLike :: AntiLeft variable -> TermLike variable
toTermLike AntiLeft{aliasTerm} = aliasTerm

{-
Supported syntax:
antiLeft = antiLeftAlias

The antiLeftAlias expands to
or(nextAntiLeft, or(lhs1, or(lhs2, ... or(lhsn, bottom) ... )))
where nextAntiLeft is optional.

nextAntileft has the same syntax as antiLeft.
lhs is of the form
exists x1 . exists x2 . ... exists xn . lhsTermAlias(x1, ..., xn)

lhsTermAlias expands to
and(lhsPredicate, lhsTerm)
-}
parse ::
    InternalVariable variable =>
    TermLike variable ->
    Maybe (AntiLeft variable)
parse aliasTerm@(ApplyAlias_ alias params) = do
    (maybeInner, lhss) <-
        case substituteInAlias alias params of
            substituted@(Or_ _ first remaining) ->
                case parse first of
                    Just nextAntiLeft ->
                        Just (Just nextAntiLeft, remaining)
                    Nothing -> Just (Nothing, substituted)
            _ -> Nothing
    leftHands <- parseLhss lhss
    return AntiLeft{aliasTerm, maybeInner, leftHands}
parse _ = Nothing

parseLhss ::
    InternalVariable variable =>
    TermLike variable ->
    Maybe [AntiLeftLhs variable]
parseLhss (Or_ _ first nexts) = do
    firstParsed <- parseLhs first
    nextsParsed <- parseLhss nexts
    return (firstParsed : nextsParsed)
parseLhss (Bottom_ _) = Just []
parseLhss _ = Nothing

parseLhs ::
    InternalVariable variable =>
    TermLike variable ->
    Maybe (AntiLeftLhs variable)
parseLhs lhs = case aliasTerm of
    (ApplyAlias_ alias params) -> case substituteInAlias alias params of
        (And_ _ predicate term) ->
            Just
                AntiLeftLhs
                    { existentials
                    , predicate = Predicate.wrapPredicate predicate
                    , term
                    }
        _ -> Nothing
    _ -> Nothing
  where
    (existentials, aliasTerm) = stripExistentials lhs

    stripExistentials ::
        TermLike variable -> ([ElementVariable variable], TermLike variable)
    stripExistentials (Exists_ _ var term) = (var : vars, remaining)
      where
        (vars, remaining) = stripExistentials term
    stripExistentials term = ([], term)

{- | Creates the AntiLeft predicate as described in
docs/2020-06-30-Combining-Priority-Axioms.md

Note that, in the document, an antileft is encoded as an and(not)

@
∧ ¬ (∃ X₁ . φ₁(X₁) ∧ P₁(X₁))
...
∧ ¬ (∃ Xn . φn(Xn) ∧ Pn(Xn))
@

while antilefts are actually encoded as a not(or)

@
¬ (
    (∃ X₁ . φ₁(X₁) ∧ P₁(X₁))
    ∨ (∃ X2 . φ2(X2) ∧ P2(X2))
    ...
    ∨ (∃ Xn . φn(Xn) ∧ Pn(Xn))
)
@

The implementation of antiLeftPredicate, of course, follows the actual encoding.

The negation at the top of the antileft should be handled by the caller:
* the antileft parsing code in this file does not handle the @not@ on top
  of the term, it expects it to be removed by the caller;
* the antiLeftPredicate function also does not add a @not@ on top of the
  generated predicate, and relies on the caller doing that.
-}
antiLeftPredicate ::
    InternalVariable variable =>
    AntiLeft variable ->
    TermLike variable ->
    Predicate variable
antiLeftPredicate antiLeft@(AntiLeft _ _ _) term = case antiLeft of
    AntiLeft{maybeInner = Nothing, leftHands} ->
        antiLeftHandsPredicate leftHands term
    AntiLeft{maybeInner = Just inner, leftHands} ->
        makeOrPredicate
            (antiLeftPredicate inner term)
            (antiLeftHandsPredicate leftHands term)

antiLeftHandsPredicate ::
    forall variable.
    InternalVariable variable =>
    [AntiLeftLhs variable] ->
    TermLike variable ->
    Predicate variable
antiLeftHandsPredicate antiLefts termLike =
    makeMultipleOrPredicate (map antiLeftHandPredicate antiLefts)
  where
    antiLeftHandPredicate :: AntiLeftLhs variable -> Predicate variable
    antiLeftHandPredicate antiLeftLhs@(AntiLeftLhs _ _ _) =
        makeMultipleExists
            existentials
            ( makeAndPredicate
                predicate
                (makeCeilPredicate (mkAnd termLike term))
            )
      where
        used :: Set (SomeVariableName variable)
        used = FreeVariables.toNames (freeVariables termLike)

        refreshedAntiLeftLhs = refreshAntiLeftExistentials used antiLeftLhs

        AntiLeftLhs{existentials, predicate, term} = refreshedAntiLeftLhs

refreshAntiLeftExistentials ::
    forall variable.
    InternalVariable variable =>
    Set (SomeVariableName variable) ->
    AntiLeftLhs variable ->
    AntiLeftLhs variable
refreshAntiLeftExistentials
    alreadyUsed
    antiLeftLhs@(AntiLeftLhs _ _ _) =
        AntiLeftLhs
            { existentials = map renameVar existentials
            , predicate = substitute substitution predicate
            , term = substitute substitution term
            }
      where
        AntiLeftLhs{existentials, predicate, term} = antiLeftLhs

        refreshVariable ::
            Set (SomeVariableName variable) ->
            ElementVariable variable ->
            ElementVariable variable
        refreshVariable avoiding x =
            refreshElementVariable avoiding x & fromMaybe x

        refreshAndAvoid ::
            Set (SomeVariableName variable) ->
            ElementVariable variable ->
            (Set (SomeVariableName variable), ElementVariable variable)
        refreshAndAvoid avoiding x =
            ( Set.insert (SomeVariableNameElement $ variableName refreshed) avoiding
            , refreshed
            )
          where
            refreshed = refreshVariable avoiding x

        refreshMap ::
            Set (SomeVariableName variable) ->
            [ElementVariable variable] ->
            Map (SomeVariableName variable) (ElementVariable variable)
        refreshMap _ [] = Map.empty
        refreshMap avoiding (var : vars) =
            Map.insert
                (SomeVariableNameElement $ variableName var)
                refreshedVar
                (refreshMap newAvoiding vars)
          where
            (newAvoiding, refreshedVar) = refreshAndAvoid avoiding var

        varSubstitution ::
            Map (SomeVariableName variable) (ElementVariable variable)
        varSubstitution = refreshMap alreadyUsed existentials

        substitution :: Map (SomeVariableName variable) (TermLike variable)
        substitution = fmap mkElemVar varSubstitution

        renameVar :: ElementVariable variable -> ElementVariable variable
        renameVar var =
            fromMaybe var (Map.lookup someVariableName varSubstitution)
          where
            someVariableName = SomeVariableNameElement (variableName var)
