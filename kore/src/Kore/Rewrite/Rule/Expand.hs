{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Rewrite.Rule.Expand (
    ExpandSingleConstructors (..),
) where

import Control.Lens (
    (%=),
 )
import Control.Monad.State.Strict (
    State,
    evalState,
 )
import Control.Monad.State.Strict qualified as State
import Data.Bifunctor qualified as Bifunctor
import Data.Binary (Binary)
import Data.Generics.Product (
    field,
 )
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Debug qualified
import GHC.Generics qualified as GHC
import Kore.Attribute.Pattern.FreeVariables (
    freeVariables,
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Attribute.Sort.Constructors qualified as Attribute.Constructors (
    Constructor (Constructor),
    ConstructorLike (ConstructorLikeConstructor),
    Constructors (Constructors),
 )
import Kore.Attribute.Sort.Constructors qualified as Constructors.DoNotUse
import Kore.IndexedModule.MetadataTools (
    SmtMetadataTools,
    findSortConstructors,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Substitution (
    Substitution,
 )
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike (
    InternalVariable,
    TermLike,
    mkApplySymbol,
    mkElemVar,
 )
import Kore.Reachability (
    AllPathClaim (..),
    OnePathClaim (..),
    SomeClaim (..),
 )
import Kore.Rewrite.ClaimPattern (
    ClaimPattern (..),
    freeVariablesLeft,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Sort (
    Sort (..),
    SortActual (SortActual),
 )
import Kore.Sort qualified as Sort.DoNotUse
import Kore.Substitute
import Kore.Syntax.Variable
import Kore.Variables.Fresh (
    refreshVariable,
 )
import Prelude.Kore
import Pretty qualified

-- | Instantiate variables on sorts with a single constructor

{- TODO(ttuegel): make this a strategy step, so that we expand any
    single-constructor variables at the start of each proof step.
    Going even further: make this a step in the variable simplifier?
-}
class ExpandSingleConstructors rule where
    expandSingleConstructors ::
        SmtMetadataTools attributes ->
        rule ->
        rule

instance ExpandSingleConstructors ClaimPattern where
    expandSingleConstructors
        metadataTools
        rule@(ClaimPattern _ _ _ _) =
            case rule of
                ClaimPattern
                    { left
                    , existentials
                    , right
                    } ->
                        let leftElementVariables ::
                                [ElementVariable RewritingVariableName]
                            leftElementVariables =
                                extractFreeElementVariables
                                    . freeVariablesLeft
                                    $ rule
                            freeElementVariables ::
                                [ElementVariable RewritingVariableName]
                            freeElementVariables =
                                extractFreeElementVariables
                                    . freeVariables
                                    $ rule
                            allElementVariableNames ::
                                Set (ElementVariableName RewritingVariableName)
                            allElementVariableNames =
                                variableName <$> freeElementVariables <> existentials
                                    & Set.fromList
                            expansion ::
                                Map.Map
                                    (SomeVariable RewritingVariableName)
                                    (TermLike RewritingVariableName)
                            expansion =
                                expandVariables
                                    metadataTools
                                    leftElementVariables
                                    allElementVariableNames
                            substitutionPredicate =
                                Substitution.toPredicate
                                    . from @(Map _ _) @(Substitution _)
                                    $ expansion
                            subst = Map.mapKeysMonotonic variableName expansion
                         in rule
                                { left =
                                    Pattern.andCondition
                                        (substitute subst left)
                                        (Condition.fromPredicate substitutionPredicate)
                                , existentials
                                , right = substitute subst right
                                }
          where
            extractFreeElementVariables =
                mapMaybe retractElementVariable
                    . FreeVariables.toList

instance ExpandSingleConstructors OnePathClaim where
    expandSingleConstructors tools =
        OnePathClaim . expandSingleConstructors tools . getOnePathClaim

instance ExpandSingleConstructors AllPathClaim where
    expandSingleConstructors tools =
        AllPathClaim . expandSingleConstructors tools . getAllPathClaim

instance ExpandSingleConstructors SomeClaim where
    expandSingleConstructors tools (OnePath rule) =
        OnePath
            . OnePathClaim
            . expandSingleConstructors tools
            . getOnePathClaim
            $ rule
    expandSingleConstructors tools (AllPath rule) =
        AllPath
            . AllPathClaim
            . expandSingleConstructors tools
            . getAllPathClaim
            $ rule

newtype Expansion variable = Expansion {stale :: Set (ElementVariableName variable)}
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)

type Expander variable = State (Expansion variable)

expandVariables ::
    forall variable attributes.
    InternalVariable variable =>
    SmtMetadataTools attributes ->
    [ElementVariable variable] ->
    Set (ElementVariableName variable) ->
    Map (SomeVariable variable) (TermLike variable)
expandVariables metadataTools variables stale =
    wither expandAddVariable variables
        & flip evalState Expansion{stale}
        & (map . Bifunctor.first) inject
        & Map.fromList
  where
    expandAddVariable ::
        ElementVariable variable ->
        Expander
            variable
            (Maybe (ElementVariable variable, TermLike variable))
    expandAddVariable variable = do
        term <- expandVariable metadataTools variable
        if mkElemVar variable == term
            then return Nothing
            else return $ Just (variable, term)

expandVariable ::
    forall variable attributes.
    InternalVariable variable =>
    SmtMetadataTools attributes ->
    ElementVariable variable ->
    Expander variable (TermLike variable)
expandVariable metadataTools variable@Variable{variableSort} =
    expandSort metadataTools variable UseDirectly variableSort

expandSort ::
    forall variable attributes.
    InternalVariable variable =>
    SmtMetadataTools attributes ->
    ElementVariable variable ->
    VariableUsage ->
    Sort ->
    Expander variable (TermLike variable)
expandSort metadataTools defaultVariable variableUsage sort =
    case findSingleConstructor sort of
        Just constructor ->
            expandConstructor metadataTools defaultVariable constructor
        Nothing ->
            maybeNewVariable defaultVariable sort variableUsage
  where
    findSingleConstructor (SortVariableSort _) = Nothing
    findSingleConstructor (SortActualSort SortActual{sortActualName}) = do
        Attribute.Constructors.Constructors ctors <-
            findSortConstructors metadataTools sortActualName
        ctorLikes <- ctors
        case ctorLikes of
            Attribute.Constructors.ConstructorLikeConstructor ctor :| [] ->
                Just ctor
            _ -> Nothing

expandConstructor ::
    forall variable attributes.
    InternalVariable variable =>
    SmtMetadataTools attributes ->
    ElementVariable variable ->
    Attribute.Constructors.Constructor ->
    Expander variable (TermLike variable)
expandConstructor
    metadataTools
    defaultVariable
    Attribute.Constructors.Constructor{name = symbol, sorts} =
        mkApplySymbol symbol <$> traverse expandChildSort sorts
      where
        expandChildSort :: Sort -> Expander variable (TermLike variable)
        expandChildSort = expandSort metadataTools defaultVariable UseAsPrototype

{- | Context: we have a TermLike that contains a variables, and we
attempt to expand them into constructor applications whenever that's possible.

We expand a variable by attempting to expand its sort into an unique
constructor application, and, recursively, the argument sorts of that
constructor.

This data type tells us how to use the initial variable that we are expanding
when we can't expand a sort, so we have to return a variable of that sort
instead.
-}
data VariableUsage
    = -- | We don't need to generate a new variable, we are at the top and
      -- we didn't manage to expand anything, so we can just reuse the
      -- variable in the original term as the sort's expansion (useful if we
      -- want to have prettier terms).
      UseDirectly
    | -- | We have expanded the initial sort at least once, so we need a variable
      -- somewhere in the middle of the expansion. We can't reuse the original
      -- variable, so we need to generate a new one based on it.
      UseAsPrototype

maybeNewVariable ::
    forall variable.
    InternalVariable variable =>
    HasCallStack =>
    ElementVariable variable ->
    Sort ->
    VariableUsage ->
    Expander variable (TermLike variable)
maybeNewVariable
    variable@Variable{variableSort}
    sort
    UseDirectly =
        if sort /= variableSort
            then error "Unmatching sort for direct use variable."
            else pure (mkElemVar variable)
maybeNewVariable variable sort UseAsPrototype = do
    Expansion{stale} <- State.get
    case refreshVariable stale variable' of
        Just newVariable -> do
            field @"stale" %= Set.insert (variableName newVariable)
            pure (mkElemVar newVariable)
        Nothing ->
            (error . show . Pretty.hang 4 . Pretty.vsep)
                [ "Failed to generate a new name for:"
                , Pretty.indent 4 $ Debug.debug variable'
                , "while avoiding:"
                , Pretty.indent 4 $ Debug.debug stale
                ]
  where
    variable' = resort variable
    resort var = var{variableSort = sort}
