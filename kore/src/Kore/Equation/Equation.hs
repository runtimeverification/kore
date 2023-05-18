{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Equation.Equation (
    Equation (..),
    mkEquation,
    toTermLike,
    mapVariables,
    refreshVariables,
    isSimplificationRule,
    equationPriority,
    identifiers,
) where

import Control.Lens qualified as Lens
import Data.Binary (Binary)
import Data.Default qualified as Default
import Data.Functor.Foldable qualified as Recursive
import Data.Generics.Wrapped (
    _Wrapped,
 )
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Text (
    Text,
 )
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.AST.AstWithLocation
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
    HasFreeVariables (..),
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Attribute.Symbol qualified as Attribute.Symbol
import Kore.Internal.Predicate (
    Predicate,
    fromPredicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.Symbol (
    Symbol (..),
 )
import Kore.Internal.TermLike (
    InternalVariable,
    TermLike,
    mkVar,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Rewrite.UnifyingRule (
    Renaming,
 )
import Kore.Sort
import Kore.Substitute
import Kore.Syntax.Application (
    Application (..),
 )
import Kore.Syntax.Variable
import Kore.TopBottom
import Kore.Unparser (
    Unparse (..),
 )
import Kore.Variables.Fresh qualified as Fresh
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified
import SQL qualified

{- | A type for representing equational rules, which in K can appear as
_function definition_ rules or as _simplification_ rules.

The following are common to both types of equational rules:
* the 'requires' field corresponds to the requires predicate of the K rule
* the 'ensures' field corresponds to the ensures predicate of the K rule
* the 'right' field corresponds to the term on the right hand side of the '=>' in the K rule.

The 'left' field will vary in contents (as described below), but its structure
should be the same: it should be a term with a function symbol at the top.

For function definition rules, the Kore encoding is specified here:
https://github.com/runtimeverification/haskell-backend/blob/master/design-decisions/2020-05-02-function-rules.md#solution
The _Args_ and _Prio_ predicates correspond to the 'argument' and 'antiLeft' field, respectively.
See the linked design document for the reasons why the 'argument' is encoded like that.
The 'antiLeft' is used to encode the priority of the function definition. This corresponds
to the priority attribute, which only equations which are function definitions may have.

In the case of simplification rules, there is no 'argument' or 'antiLeft'.

The 'left' will differ between function definitions and simplification rules as in the following example:
A K function definition rule's left hand side which looks like
@
    f(term1, term2, term3)
@
will be encoded into Kore as follows (in pseudo-Kore):
@
    f(Var1, Var2, Var3)
@
with the 'argument' predicate (modulo the properties of logical conjunction):
@
    \\and(\\and(\\in(Var1, term1), \\in(Var2, term2)), \\in(Var3, term3))
@
A K simplification rule's left hand side will get translated directly to Kore, without
making any changes to the contents of the left hand side.
Simplification rules are required to have the simplification attribute. Some prioritization
of simplification rules is allowed, through the argument of the simplification attribute.
These are regarded as hints for the backend, they don't carry semantic meaning as in the
case of function rules and their priority attributes.

For more information see:
* https://github.com/runtimeverification/k/blob/master/docs/user_manual.md#function-and-total-attributes
* https://github.com/runtimeverification/k/blob/master/docs/user_manual.md#simplification-attribute-haskell-backend
* https://github.com/runtimeverification/k/blob/master/docs/user_manual.md#owise-and-priority-attributes
* 'Kore.Equation.Sentence.matchEquation', for the structure of all equation types
-}
data Equation variable = Equation
    { requires :: !(Predicate variable)
    , argument :: !(Maybe (Predicate variable))
    , antiLeft :: !(Maybe (Predicate variable))
    , left :: !(TermLike variable)
    , right :: !(TermLike variable)
    , ensures :: !(Predicate variable)
    , attributes :: !(Attribute.Axiom Symbol variable)
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving anyclass (Hashable)

-- | Creates a basic, unconstrained, Equality pattern
mkEquation ::
    HasCallStack =>
    InternalVariable variable =>
    TermLike variable ->
    TermLike variable ->
    Equation variable
mkEquation left right =
    assert
        (TermLike.termLikeSort left == TermLike.termLikeSort right)
        Equation
            { left
            , requires = Predicate.makeTruePredicate
            , argument = Nothing
            , antiLeft = Nothing
            , right
            , ensures = Predicate.makeTruePredicate
            , attributes = Default.def
            }

instance InternalVariable variable => Pretty (Equation variable) where
    pretty equation@(Equation _ _ _ _ _ _ _) =
        Pretty.vsep
            [ "requires:"
            , Pretty.indent 4 (pretty requires)
            , "argument:"
            , Pretty.indent 4 (maybe "" pretty argument)
            , "antiLeft:"
            , Pretty.indent 4 (maybe "" pretty antiLeft)
            , "left:"
            , Pretty.indent 4 (unparse left)
            , "right:"
            , Pretty.indent 4 (unparse right)
            , "ensures:"
            , Pretty.indent 4 (pretty ensures)
            ]
      where
        Equation
            { requires
            , argument
            , antiLeft
            , left
            , right
            , ensures
            } = equation

instance TopBottom (Equation variable) where
    isTop _ = False
    isBottom _ = False

instance SQL.Table (Equation VariableName)

instance SQL.Column (Equation VariableName) where
    defineColumn = SQL.defineForeignKeyColumn
    toColumn = SQL.toForeignKeyColumn

instance InternalVariable variable => Substitute (Equation variable) where
    type TermType (Equation variable) = TermLike variable

    type VariableNameType (Equation variable) = variable

    substitute assignments equation =
        Equation
            { requires = substitute assignments (requires equation)
            , argument = substitute assignments <$> argument equation
            , antiLeft = substitute assignments <$> antiLeft equation
            , left = substitute assignments (left equation)
            , right = substitute assignments (right equation)
            , ensures = substitute assignments (ensures equation)
            , attributes = attributes equation
            }

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

toTermLike ::
    InternalVariable variable =>
    Sort ->
    Equation variable ->
    TermLike variable
toTermLike sort equation
    -- \ceil axiom
    | isTop requires
    , isTop ensures
    , TermLike.Ceil_ _ sort1 _ <- left
    , TermLike.Top_ sort2 <- right
    , sort1 == sort2 =
        left
    -- function rule
    | Just argument' <- argument
    , Just antiLeft' <- antiLeft =
        let antiLeftTerm = fromPredicate sort antiLeft'
            argumentTerm = fromPredicate sort argument'
         in TermLike.mkImplies
                ( TermLike.mkAnd
                    antiLeftTerm
                    ( TermLike.mkAnd
                        requires'
                        argumentTerm
                    )
                )
                ( TermLike.mkEquals sort left $
                    TermLike.mkAnd right ensures'
                )
    -- function rule without priority
    | Just argument' <- argument =
        let argumentTerm = fromPredicate sort argument'
         in TermLike.mkImplies
                ( TermLike.mkAnd
                    requires'
                    (TermLike.mkAnd argumentTerm $ TermLike.mkTop sort)
                )
                ( TermLike.mkEquals sort left $
                    TermLike.mkAnd right ensures'
                )
    -- unconditional equation
    | isTop requires
    , isTop ensures =
        TermLike.mkEquals sort left right
    -- conditional equation
    | otherwise =
        TermLike.mkImplies
            requires'
            ( TermLike.mkEquals sort left $
                TermLike.mkAnd right ensures'
            )
  where
    requires' = fromPredicate sort requires
    ensures' = fromPredicate rightSort ensures
    Equation
        { requires
        , argument
        , antiLeft
        , left
        , right
        , ensures
        } = equation
    rightSort = TermLike.termLikeSort right

instance
    InternalVariable variable =>
    HasFreeVariables (Equation variable) variable
    where
    freeVariables rule@(Equation _ _ _ _ _ _ _) = case rule of
        Equation{left, argument, antiLeft, requires, right, ensures} ->
            freeVariables left
                <> freeVariables requires
                <> maybe mempty freeVariables argument
                <> maybe mempty freeVariables antiLeft
                <> freeVariables right
                <> freeVariables ensures

instance AstWithLocation variable => AstWithLocation (Equation variable) where
    locationFromAst = locationFromAst . left

mapVariables ::
    (InternalVariable variable1, InternalVariable variable2) =>
    AdjSomeVariableName (variable1 -> variable2) ->
    Equation variable1 ->
    Equation variable2
mapVariables mapping equation@(Equation _ _ _ _ _ _ _) =
    equation
        { requires = mapPredicateVariables requires
        , argument = mapPredicateVariables <$> argument
        , antiLeft = mapPredicateVariables <$> antiLeft
        , left = mapTermLikeVariables left
        , right = mapTermLikeVariables right
        , ensures = mapPredicateVariables ensures
        , attributes = Attribute.mapAxiomVariables mapping attributes
        }
  where
    Equation
        { requires
        , argument
        , antiLeft
        , left
        , right
        , ensures
        , attributes
        } = equation
    mapTermLikeVariables = TermLike.mapVariables mapping
    mapPredicateVariables = Predicate.mapVariables mapping

refreshVariables ::
    forall variable.
    InternalVariable variable =>
    FreeVariables variable ->
    Equation variable ->
    (Renaming variable, Equation variable)
refreshVariables
    (FreeVariables.toNames -> avoid)
    equation@(Equation _ _ _ _ _ _ _) =
        let rename' :: Map (SomeVariableName variable) (SomeVariable variable)
            rename' =
                FreeVariables.toSet originalFreeVariables
                    & Fresh.refreshVariablesSet avoid
            lookupSomeVariableName ::
                forall variable'.
                Injection (SomeVariableName variable) variable' =>
                variable' ->
                variable'
            lookupSomeVariableName variable =
                do
                    let injected = inject @(SomeVariableName _) variable
                    someVariableName <- variableName <$> Map.lookup injected rename'
                    retract someVariableName
                    & fromMaybe variable
            adj :: AdjSomeVariableName (variable -> variable)
            adj =
                AdjSomeVariableName
                    { adjSomeVariableNameElement =
                        ElementVariableName . Lens.over _Wrapped $
                            lookupSomeVariableName @(ElementVariableName _)
                    , adjSomeVariableNameSet =
                        SetVariableName . Lens.over _Wrapped $
                            lookupSomeVariableName @(SetVariableName _)
                    }
            subst :: Map (SomeVariableName variable) (TermLike variable)
            subst =
                FreeVariables.toList originalFreeVariables
                    & map mkSubst
                    & Map.fromList
            mkSubst variable =
                ( variableName variable
                , TermLike.mkVar (mapSomeVariable adj variable)
                )
            left' = substitute subst left
            requires' = substitute subst requires
            argument' = substitute subst <$> argument
            antiLeft' = substitute subst <$> antiLeft
            right' = substitute subst right
            ensures' = substitute subst ensures
            attributes' = Attribute.mapAxiomVariables adj attributes
            equation' =
                equation
                    { left = left'
                    , requires = requires'
                    , argument = argument'
                    , antiLeft = antiLeft'
                    , right = right'
                    , ensures = ensures'
                    , attributes = attributes'
                    }
         in (rename', equation')
      where
        Equation
            { requires
            , argument
            , antiLeft
            , left
            , right
            , ensures
            , attributes
            } = equation
        originalFreeVariables = freeVariables equation

isSimplificationRule :: Equation variable -> Bool
isSimplificationRule Equation{attributes} =
    case Attribute.simplification attributes of
        Attribute.IsSimplification _ -> True
        _ -> False

equationPriority :: Equation variable -> Integer
equationPriority = Attribute.getPriorityOfAxiom . attributes

{- | The list of identifiers for an 'Equation'.

The identifiers are:

* the rule label
* symbol names on the left-hand side
* symbol 'Klabel's on the left-hand side
-}
identifiers :: Equation variable -> [Text]
identifiers Equation{left, attributes} =
    rule <> symbols
  where
    symbols =
        flip Recursive.fold left $ \case
            _ :< TermLike.ApplySymbolF application ->
                applySymbolIdentifiers application <> fold application
            termLikeF -> fold termLikeF
    rule = maybe [] pure $ Attribute.unLabel $ Attribute.label attributes

    applySymbolIdentifiers application =
        catMaybes [symbolName, symbolKlabel]
      where
        Application{applicationSymbolOrAlias = symbol} = application
        symbolName = Just . getId $ symbolConstructor symbol
        symbolKlabel =
            (Attribute.Symbol.getKlabel . Attribute.Symbol.klabel)
                (symbolAttributes symbol)
