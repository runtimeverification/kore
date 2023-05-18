{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Equation.Sentence (
    fromSentenceAxiom,
    matchEquation,
    MatchEquationError (..),
) where

import Data.Bifunctor qualified as Bifunctor
import Data.Binary (Binary)
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Axiom.Constructor (
    Constructor (..),
 )
import Kore.Attribute.Subsort (
    Subsorts (..),
 )
import Kore.Attribute.Total (
    Total (..),
 )
import Kore.Equation.Equation
import Kore.Internal.Predicate (
    NotPredicate,
    makePredicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.TermLike (
    InternalVariable,
    Symbol,
    TermLike,
    VariableName,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Syntax.Sentence (
    SentenceAxiom (..),
 )
import Kore.Unparser (
    unparse,
 )
import Kore.Verified qualified as Verified
import Prelude.Kore
import Pretty

fromSentenceAxiom ::
    (Attribute.Axiom Symbol VariableName, Verified.SentenceAxiom) ->
    Either (MatchEquationError VariableName) (Equation VariableName)
fromSentenceAxiom (attributes, SentenceAxiom{sentenceAxiomPattern}) =
    matchEquation attributes sentenceAxiomPattern

data MatchEquationError variable
    = NotEquation !(TermLike variable)
    | RequiresError !(NotPredicate variable)
    | ArgumentError !(NotPredicate variable)
    | AntiLeftError !(NotPredicate variable)
    | EnsuresError !(NotPredicate variable)
    | TotalAxiom
    | ConstructorAxiom
    | SubsortAxiom
    | UnsupportedLHS !(TermLike variable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

instance InternalVariable variable => Pretty (MatchEquationError variable) where
    pretty (NotEquation term) =
        Pretty.vsep
            [ "The given term is not an equation:"
            , unparse term
            ]
    pretty (RequiresError notPred) =
        Pretty.vsep
            [ "The equation's requires clause is not a predicate:"
            , pretty notPred
            , "This is a frontend bug. Please report this error at https://github.com/runtimeverification/k/issues."
            ]
    pretty (ArgumentError notPred) =
        Pretty.vsep
            [ "The equation's argument clause is not a predicate:"
            , pretty notPred
            , "This is a frontend bug. Please report this error at https://github.com/runtimeverification/k/issues."
            ]
    pretty (AntiLeftError notPred) =
        Pretty.vsep
            [ "The equation's anti-left clause is not a predicate:"
            , pretty notPred
            , "This is a frontend bug. Please report this error at https://github.com/runtimeverification/k/issues."
            ]
    pretty (EnsuresError notPred) =
        Pretty.vsep
            [ "The equation's ensures clause is not a predicate:"
            , pretty notPred
            , "This is a frontend bug. Please report this error at https://github.com/runtimeverification/k/issues."
            ]
    pretty TotalAxiom = "The term is a total axiom."
    pretty ConstructorAxiom = "The term is a constructor axiom."
    pretty SubsortAxiom = "The term is a subsort axiom."
    pretty (UnsupportedLHS term) =
        Pretty.vsep
            [ "Unsupported LHS of equation:"
            , unparse term
            ]

matchEquation ::
    forall variable.
    InternalVariable variable =>
    Attribute.Axiom Symbol variable ->
    TermLike variable ->
    Either (MatchEquationError variable) (Equation variable)
matchEquation attributes termLike
    | isTotalAxiom = Left TotalAxiom
    | isConstructorAxiom = Left ConstructorAxiom
    | isSubsortAxiom = Left SubsortAxiom
    | TermLike.Forall_ _ _ child <- termLike = matchEquation attributes child
    | otherwise = match termLike >>= removeRedundantEnsures
  where
    isTotalAxiom = (isDeclaredTotal . Attribute.total) attributes
    isConstructorAxiom = (isConstructor . Attribute.constructor) attributes
    isSubsortAxiom = (not . null . getSubsorts . Attribute.subsorts) attributes

    -- function rule without priority
    match
        ( TermLike.Implies_
                _
                ( TermLike.And_
                        _
                        requires
                        argument@( TermLike.And_
                                        _
                                        (TermLike.In_ _ _ _ _)
                                        _
                                    )
                    )
                ( TermLike.Equals_
                        _
                        _
                        left
                        (TermLike.And_ _ right ensures)
                    )
            ) =
            do
                requires' <- makePredicate requires & Bifunctor.first RequiresError
                argument' <- makePredicate argument & Bifunctor.first ArgumentError
                ensures' <- makePredicate ensures & Bifunctor.first EnsuresError
                pure
                    Equation
                        { requires = requires'
                        , argument = Just argument'
                        , antiLeft = Nothing
                        , left
                        , right
                        , ensures = ensures'
                        , attributes
                        }

    -- function rule with priority
    match
        ( TermLike.Implies_
                _
                ( TermLike.And_
                        _
                        antiLeft
                        ( TermLike.And_
                                _
                                requires
                                argument
                            )
                    )
                ( TermLike.Equals_
                        _
                        _
                        left
                        (TermLike.And_ _ right ensures)
                    )
            ) =
            do
                requires' <- makePredicate requires & Bifunctor.first RequiresError
                argument' <- makePredicate argument & Bifunctor.first ArgumentError
                antiLeft' <- makePredicate antiLeft & Bifunctor.first AntiLeftError
                ensures' <- makePredicate ensures & Bifunctor.first EnsuresError
                pure
                    Equation
                        { requires = requires'
                        , argument = Just argument'
                        , antiLeft = Just antiLeft'
                        , left
                        , right
                        , ensures = ensures'
                        , attributes
                        }
    match
        ( TermLike.Implies_
                _
                requires
                ( TermLike.Equals_
                        _
                        _
                        left
                        (TermLike.And_ _ right ensures)
                    )
            ) =
            do
                left' <- matchLhs left
                requires' <- makePredicate requires & Bifunctor.first RequiresError
                ensures' <- makePredicate ensures & Bifunctor.first EnsuresError
                pure
                    Equation
                        { requires = requires'
                        , argument = Nothing
                        , antiLeft = Nothing
                        , left = left'
                        , right
                        , ensures = ensures'
                        , attributes
                        }
    match (TermLike.Equals_ _ _ left right) = do
        pure
            Equation
                { requires = Predicate.makeTruePredicate
                , argument = Nothing
                , antiLeft = Nothing
                , left
                , right
                , ensures = Predicate.makeTruePredicate
                , attributes
                }
    match left@(TermLike.Ceil_ _ sort _) = do
        pure
            Equation
                { requires = Predicate.makeTruePredicate
                , argument = Nothing
                , antiLeft = Nothing
                , left
                , right = TermLike.mkTop sort
                , ensures = Predicate.makeTruePredicate
                , attributes
                }
    match termLike' = Left (NotEquation termLike')
    matchLhs term =
        case term of
            TermLike.Equals_ _ _ _ _ -> Right term
            TermLike.Ceil_ _ _ _ -> Right term
            TermLike.App_ _ _ -> Right term
            TermLike.Inj_ _ -> Right term
            TermLike.Not_ _ _ -> Right term
            _ -> Left $ UnsupportedLHS term

    -- If the ensures and requires are the same, then the ensures is redundant.
    removeRedundantEnsures equation@Equation{requires, ensures}
        | ensures == requires =
            return equation{ensures = Predicate.makeTruePredicate}
        | otherwise = return equation
