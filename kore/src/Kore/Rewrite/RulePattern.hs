{- |
Description : Rewrite rules
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Rewrite.RulePattern (
    RulePattern (..),
    RewriteRule (..),
    ImplicationRule (..),
    RHS (..),
    HasAttributes (..),
    UnifyingRule (..),
    rulePattern,
    leftPattern,
    applySubstitution,
    topExistsToImplicitForall,
    isFreeOf,
    lhsEqualsRhs,
    rhsForgetSimplified,
    rhsToTerm,
    lhsToTerm,
    rhsToPattern,
    termToRHS,
    injectTermIntoRHS,
    rewriteRuleToTerm,
    implicationRuleToTerm,
    allPathGlobally,
    aPG,
    mapRuleVariables,
    mkRewritingRule,
    unRewritingRule,
) where

import Control.Lens (
    Lens',
 )
import Control.Lens qualified as Lens
import Control.Monad.State.Strict (
    evalState,
 )
import Control.Monad.State.Strict qualified as State
import Data.Binary (Binary)
import Data.Coerce (
    Coercible,
    coerce,
 )
import Data.Default qualified as Default
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
    HasFreeVariables (..),
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Debug
import Kore.Internal.Alias (
    Alias (..),
 )
import Kore.Internal.ApplicationSorts
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.Pattern (
    Conditional (..),
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.Substitution (
    Substitution,
 )
import Kore.Internal.Substitution qualified as Substitution (
    toMap,
    variables,
 )
import Kore.Internal.Symbol (
    Symbol (..),
 )
import Kore.Internal.TermLike (
    TermLike,
    mkVar,
    termLikeSort,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Internal.Variable (
    InternalVariable,
 )
import Kore.Rewrite.AntiLeft (
    AntiLeft,
 )
import Kore.Rewrite.AntiLeft qualified as AntiLeft
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
    getRewritingVariable,
    mkRuleVariable,
 )
import Kore.Rewrite.UnifyingRule (
    UnifyingRule (..),
 )
import Kore.Sort (
    Sort (..),
 )
import Kore.Substitute
import Kore.Syntax.Id (
    AstLocation (..),
    getId,
    idLocation,
    pattern Id,
 )
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser (
    Unparse,
    unparse,
    unparse2,
 )
import Kore.Variables.Fresh hiding (
    refreshVariables',
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

-- | Defines the right-hand-side of a rewrite rule / claim
data RHS variable = RHS
    { existentials :: ![TermLike.ElementVariable variable]
    , right :: !(TermLike.TermLike variable)
    , ensures :: !(Predicate variable)
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance InternalVariable variable => Substitute (RHS variable) where
    type TermType (RHS variable) = TermLike variable

    type VariableNameType (RHS variable) = variable

    substitute subst RHS{existentials, right, ensures} =
        RHS
            { existentials
            , right = substitute subst' right
            , ensures = substitute subst' ensures
            }
      where
        subst' = foldr (Map.delete . inject . variableName) subst existentials

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

{- | Given a collection of 'FreeVariables' and a RHS, it removes
converts existential quantifications at the top of the term to implicit
universal quantification,
renaming them (if needed) to avoid clashing with the given free variables.
-}
topExistsToImplicitForall ::
    forall variable.
    InternalVariable variable =>
    FreeVariables variable ->
    RHS variable ->
    Pattern variable
topExistsToImplicitForall avoid' RHS{existentials, right, ensures} =
    (rename renamed)
        Conditional
            { term = right
            , predicate = ensures
            , substitution = mempty
            }
  where
    avoid = FreeVariables.toNames avoid'
    bindExistsFreeVariables =
        freeVariables right <> freeVariables ensures
            & FreeVariables.bindVariables (mkSomeVariable <$> existentials)
            & FreeVariables.toNames
    renamed :: Map (SomeVariableName variable) (SomeVariable variable)
    renamed =
        refreshVariablesSet
            (avoid <> bindExistsFreeVariables)
            (Set.fromList $ mkSomeVariable <$> existentials)

-- | Normal rewriting axioms
data RulePattern variable = RulePattern
    { left :: !(TermLike.TermLike variable)
    , antiLeft :: !(Maybe (AntiLeft variable))
    , requires :: !(Predicate variable)
    , rhs :: !(RHS variable)
    , attributes :: !(Attribute.Axiom Symbol variable)
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance From (RulePattern variable) Attribute.SourceLocation where
    from = Attribute.sourceLocation . attributes

instance From (RulePattern variable) Attribute.Label where
    from = Attribute.label . attributes

instance From (RulePattern variable) Attribute.RuleIndex where
    from = Attribute.identifier . attributes

instance From (RulePattern variable) Attribute.UniqueId where
    from = Attribute.uniqueId . attributes

instance InternalVariable variable => Pretty (RulePattern variable) where
    pretty rulePattern'@(RulePattern _ _ _ _ _) =
        Pretty.vsep
            [ "left:"
            , Pretty.indent 4 (unparse left)
            , "requires:"
            , Pretty.indent 4 (pretty requires)
            , "existentials:"
            , Pretty.indent 4 (Pretty.list $ unparse <$> existentials)
            , "right:"
            , Pretty.indent 4 (unparse right)
            , "ensures:"
            , Pretty.indent 4 (pretty ensures)
            ]
      where
        RulePattern
            { left
            , requires
            , rhs = RHS{right, existentials, ensures}
            } = rulePattern'

instance TopBottom (RulePattern variable) where
    isTop _ = False
    isBottom _ = False

instance From (RulePattern variable) Attribute.PriorityAttributes where
    from = from @(Attribute.Axiom _ _) . attributes

instance InternalVariable variable => Substitute (RulePattern variable) where
    type TermType (RulePattern variable) = TermLike variable

    type VariableNameType (RulePattern variable) = variable

    substitute subst rulePattern'@(RulePattern _ _ _ _ _) =
        rulePattern'
            { left = substitute subst left
            , antiLeft = substitute subst <$> antiLeft
            , requires = substitute subst requires
            , rhs = substitute subst rhs
            }
      where
        RulePattern{left, antiLeft, requires, rhs} = rulePattern'

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

-- | Creates a basic, unconstrained, Equality pattern
rulePattern ::
    InternalVariable variable =>
    TermLike.TermLike variable ->
    TermLike.TermLike variable ->
    RulePattern variable
rulePattern left right =
    RulePattern
        { left
        , antiLeft = Nothing
        , requires = Predicate.makeTruePredicate
        , rhs = termToRHS right
        , attributes = Default.def
        }

-- | A 'Lens\'' to view the left-hand side of a 'RulePattern' as a 'Pattern'.
leftPattern ::
    InternalVariable variable =>
    Lens' (RulePattern variable) (Pattern variable)
leftPattern =
    Lens.lens get set
  where
    get RulePattern{left, requires} =
        Pattern.withCondition left $ from @(Predicate _) requires
    set rule@(RulePattern _ _ _ _ _) pattern' =
        rule{left, requires = Condition.toPredicate condition}
      where
        (left, condition) = Pattern.splitTerm pattern'

-- | Converts the 'RHS' back to the term form.
rhsToTerm ::
    InternalVariable variable =>
    RHS variable ->
    TermLike.TermLike variable
rhsToTerm RHS{existentials, right, ensures} =
    TermLike.mkExistsN existentials rhs
  where
    rhs = case ensures of
        Predicate.PredicateTrue -> right
        _ -> TermLike.mkAnd (Predicate.fromPredicate sort ensures) right
    sort = TermLike.termLikeSort right

-- | Converts the 'RHS' back to the term form.
rhsToPattern ::
    InternalVariable variable =>
    RHS variable ->
    Pattern variable
rhsToPattern RHS{existentials, right, ensures} =
    TermLike.mkExistsN existentials rhs
        & Pattern.fromTermLike
  where
    rhs =
        Pattern.toTermLike
            Conditional
                { term = right
                , predicate = ensures
                , substitution = mempty
                }

-- | Converts the left-hand side to the term form
lhsToTerm ::
    InternalVariable variable =>
    TermLike variable ->
    Maybe (AntiLeft variable) ->
    Predicate variable ->
    TermLike variable
lhsToTerm left Nothing requires =
    TermLike.mkAnd (Predicate.fromPredicate (termLikeSort left) requires) left
lhsToTerm left (Just antiLeft) requires =
    TermLike.mkAnd
        (TermLike.mkNot (AntiLeft.toTermLike antiLeft))
        ( TermLike.mkAnd
            (Predicate.fromPredicate (termLikeSort left) requires)
            left
        )

-- | Wraps a term as a RHS
injectTermIntoRHS ::
    InternalVariable variable =>
    TermLike.TermLike variable ->
    RHS variable
injectTermIntoRHS right =
    RHS
        { existentials = []
        , right
        , ensures = Predicate.makeTruePredicate
        }

-- | Parses a term representing a RHS into a RHS
termToRHS ::
    InternalVariable variable =>
    TermLike.TermLike variable ->
    RHS variable
termToRHS (TermLike.Exists_ _ v pat) =
    rhs{existentials = v : existentials rhs}
  where
    rhs = termToRHS pat
termToRHS (TermLike.And_ _ right ensures')
    | Right ensures <- Predicate.makePredicate ensures' =
        RHS{existentials = [], right, ensures}
termToRHS (TermLike.And_ _ ensures' right)
    | Right ensures <- Predicate.makePredicate ensures' =
        RHS{existentials = [], right, ensures}
termToRHS term = injectTermIntoRHS term

instance
    InternalVariable variable =>
    HasFreeVariables (RHS variable) variable
    where
    freeVariables = freeVariables . rhsToTerm

instance
    InternalVariable variable =>
    HasFreeVariables (RulePattern variable) variable
    where
    freeVariables rule@(RulePattern _ _ _ _ _) = case rule of
        RulePattern{left, antiLeft, requires, rhs} ->
            freeVariables left
                <> foldMap freeVariables antiLeft
                <> freeVariables requires
                <> freeVariables rhs

-- | Is the rule free of the given variables?
isFreeOf ::
    forall variable.
    InternalVariable variable =>
    RulePattern variable ->
    Set.Set (SomeVariable variable) ->
    Bool
isFreeOf rule =
    Set.disjoint $
        FreeVariables.toSet $
            freeVariables rule

renameExistentials ::
    forall variable.
    HasCallStack =>
    InternalVariable variable =>
    Map (SomeVariableName variable) (SomeVariable variable) ->
    RHS variable ->
    RHS variable
renameExistentials subst RHS{existentials, right, ensures} =
    RHS
        { existentials = renameVariable <$> existentials
        , right = rename subst right
        , ensures = rename subst ensures
        }
  where
    renameVariable ::
        ElementVariable variable ->
        ElementVariable variable
    renameVariable var =
        let name = SomeVariableNameElement . variableName $ var
         in maybe var expectElementVariable $ Map.lookup name subst

rhsForgetSimplified :: InternalVariable variable => RHS variable -> RHS variable
rhsForgetSimplified RHS{existentials, right, ensures} =
    RHS
        { existentials
        , right = TermLike.forgetSimplified right
        , ensures = Predicate.forgetSimplified ensures
        }

{- | Applies a substitution to a rule and checks that it was fully applied,
i.e. there is no substitution variable left in the rule.
-}
applySubstitution ::
    HasCallStack =>
    InternalVariable variable =>
    Substitution variable ->
    RulePattern variable ->
    RulePattern variable
applySubstitution substitution rule =
    if finalRule `isFreeOf` substitutedVariables
        then finalRule
        else
            error
                ( "Substituted variables not removed from the rule, cannot throw "
                    ++ "substitution away."
                )
  where
    subst = Substitution.toMap substitution
    finalRule = substitute subst rule
    substitutedVariables = Substitution.variables substitution

class HasAttributes rule where
    getAttributes :: rule variable -> Attribute.Axiom Symbol variable

instance HasAttributes RulePattern where
    getAttributes = attributes

{-  | Rewrite-based rule pattern.
-}
newtype RewriteRule variable = RewriteRule {getRewriteRule :: RulePattern variable}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance From (RewriteRule variable) Attribute.SourceLocation where
    from = Attribute.sourceLocation . attributes . getRewriteRule

instance From (RewriteRule variable) Attribute.Label where
    from = Attribute.label . attributes . getRewriteRule

instance From (RewriteRule variable) Attribute.RuleIndex where
    from = Attribute.identifier . attributes . getRewriteRule

instance From (RewriteRule variable) Attribute.UniqueId where
    from = Attribute.uniqueId . attributes . getRewriteRule

instance
    InternalVariable variable =>
    Unparse (RewriteRule variable)
    where
    unparse = unparse . rewriteRuleToTerm
    unparse2 = unparse2 . rewriteRuleToTerm

instance
    InternalVariable variable =>
    HasFreeVariables (RewriteRule variable) variable
    where
    freeVariables (RewriteRule rule) = freeVariables rule
    {-# INLINE freeVariables #-}

instance From (RewriteRule variable) Attribute.PriorityAttributes where
    from = from @(RulePattern _) . getRewriteRule

instance TopBottom (RewriteRule variable) where
    isTop _ = False
    isBottom _ = False

{-  | Implication-based pattern.
-}
newtype ImplicationRule variable = ImplicationRule {getImplicationRule :: RulePattern variable}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance From (ImplicationRule variable) Attribute.SourceLocation where
    from (ImplicationRule rule) = from rule

instance
    InternalVariable variable =>
    Unparse (ImplicationRule variable)
    where
    unparse = unparse . implicationRuleToTerm
    unparse2 = unparse2 . implicationRuleToTerm

{- | Reverses an 'RewriteRule' back into its 'Pattern' representation.
  Should be the inverse of 'Rule.termToAxiomPattern'.
-}
rewriteRuleToTerm ::
    InternalVariable variable =>
    RewriteRule variable ->
    TermLike.TermLike variable
rewriteRuleToTerm
    ( RewriteRule
            (RulePattern left antiLeft requires rhs _)
        ) =
        TermLike.mkRewrites
            (lhsToTerm left antiLeft requires)
            (rhsToTerm rhs)

-- | Converts an 'ImplicationRule' into its term representation
implicationRuleToTerm ::
    InternalVariable variable =>
    ImplicationRule variable ->
    TermLike.TermLike variable
implicationRuleToTerm
    (ImplicationRule (RulePattern left _ _ (RHS _ right _) _)) =
        TermLike.mkImplies left right

-- | 'Alias' construct for all path globally
aPG :: Sort -> Alias (TermLike.TermLike VariableName)
aPG sort =
    Alias
        { aliasConstructor =
            Id
                { getId = allPathGlobally
                , idLocation = AstLocationNone
                }
        , aliasParams = [sort]
        , aliasSorts =
            ApplicationSorts
                { applicationSortsOperands = [sort]
                , applicationSortsResult = sort
                }
        , aliasLeft = []
        , aliasRight = TermLike.mkTop sort
        }

-- | all path globally modality symbol
allPathGlobally :: Text
allPathGlobally = "allPathGlobally"

instance UnifyingRule (RulePattern variable) where
    type UnifyingRuleVariable (RulePattern variable) = variable

    matchingPattern = left

    precondition = requires

    refreshRule stale rule0@(RulePattern _ _ _ _ _) =
        do
            let variables = freeVariables rule0 & FreeVariables.toSet
            renaming <- refreshVariables' variables
            let existentials' = Set.fromList (inject <$> existentials rhs)
            renamingExists <- refreshVariables' existentials'
            let subst = TermLike.mkVar <$> renaming
                rule1 =
                    RulePattern
                        { left = substitute subst left
                        , antiLeft = substitute subst <$> antiLeft
                        , requires = substitute subst requires
                        , rhs =
                            rhs
                                & renameExistentials renamingExists
                                & substitute subst
                        , attributes
                        }
            -- Only return the renaming of free variables.
            -- Renaming the bound variables is invisible from outside.
            pure (renaming, rule1)
            & flip evalState (FreeVariables.toNames stale)
      where
        RulePattern{left, antiLeft, requires, rhs, attributes} = rule0
        refreshVariables' variables = do
            staleNames <- State.get
            let renaming = refreshVariablesSet staleNames variables
                staleNames' = Set.mapMonotonic variableName variables
                staleNames'' =
                    Map.elems renaming
                        & foldMap FreeVariables.freeVariable
                        & FreeVariables.toNames
            State.put (staleNames <> staleNames' <> staleNames'')
            pure renaming

mapRuleVariables ::
    forall rule variable1 variable2.
    Coercible rule RulePattern =>
    InternalVariable variable1 =>
    InternalVariable variable2 =>
    AdjSomeVariableName
        (variable1 -> variable2) ->
    rule variable1 ->
    rule variable2
mapRuleVariables adj (coerce -> rule1@(RulePattern _ _ _ _ _)) =
    coerce $
        rule1
            { left = mapTermLikeVariables left
            , antiLeft = mapAntiLeftVariables <$> antiLeft
            , requires = mapPredicateVariables requires
            , rhs =
                RHS
                    { existentials = mapElementVariable adj <$> existentials
                    , right = mapTermLikeVariables right
                    , ensures = mapPredicateVariables ensures
                    }
            , attributes =
                Attribute.mapAxiomVariables adj attributes
            }
  where
    RulePattern
        { left
        , antiLeft
        , requires
        , rhs = RHS{existentials, right, ensures}
        , attributes
        } = rule1
    mapTermLikeVariables = TermLike.mapVariables adj
    mapPredicateVariables = Predicate.mapVariables adj
    mapAntiLeftVariables = AntiLeft.mapVariables adj

instance UnifyingRule (RewriteRule variable) where
    type UnifyingRuleVariable (RewriteRule variable) = variable

    matchingPattern (RewriteRule rule) = matchingPattern rule
    {-# INLINE matchingPattern #-}

    precondition (RewriteRule rule) = precondition rule
    {-# INLINE precondition #-}

    refreshRule avoiding (RewriteRule rule) =
        RewriteRule <$> refreshRule avoiding rule
    {-# INLINE refreshRule #-}

lhsEqualsRhs ::
    InternalVariable variable =>
    RulePattern variable ->
    Bool
lhsEqualsRhs rule =
    lhsToTerm left antiLeft requires == rhsToTerm rhs
  where
    RulePattern{left, antiLeft, requires, rhs} = rule

{- | Prepare a rule for unification or matching with the configuration.

The rule's variables are:

* marked with 'RewritingVariable' so that they are preferred targets for substitution, and
* renamed to avoid any free variables from the configuration and side condition.
-}
mkRewritingRule ::
    Coercible rule RulePattern =>
    rule VariableName ->
    rule RewritingVariableName
mkRewritingRule = mapRuleVariables (pure mkRuleVariable)

-- | Unwrap the variables in a 'RulePattern'. Inverse of 'mkRewritingRule'.
unRewritingRule ::
    Coercible rule RulePattern =>
    rule RewritingVariableName ->
    rule VariableName
unRewritingRule = mapRuleVariables getRewritingVariable
