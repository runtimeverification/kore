{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
    RewritingVariable,
    isEquationVariable,
    isConfigVariable,
    isRuleVariable,
    isSomeEquationVariable,
    isSomeEquationVariableName,
    isSomeConfigVariable,
    isSomeConfigVariableName,
    isSomeRuleVariable,
    isSomeRuleVariableName,
    isElementRuleVariable,
    isElementRuleVariableName,
    mkEquationVariable,
    mkConfigVariable,
    mkRuleVariable,
    mkElementConfigVariable,
    configElementVariableFromId,
    ruleElementVariableFromId,
    mkElementRuleVariable,
    mkUnifiedRuleVariable,
    mkUnifiedConfigVariable,
    mkRewritingPattern,
    mkRewritingTerm,
    resetResultPattern,
    getRemainderPredicate,
    assertRemainderPattern,
    resetConfigVariable,
    resetRuleVariable,
    getRewritingVariable,
    withoutEquationVariables,

    -- * Exported for unparsing/testing
    getRewritingPattern,
    getRewritingTerm,
) where

import Data.Binary (Binary)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.AST.AstWithLocation (
    AstWithLocation (..),
 )
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
    toNames,
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike as TermLike hiding (
    refreshVariables,
 )
import Kore.Substitute
import Kore.Unparser
import Kore.Variables.Fresh
import Prelude.Kore

-- | The name of a 'RewritingVariable'.
data RewritingVariableName
    = EquationVariableName !VariableName
    | ConfigVariableName !VariableName
    | RuleVariableName !VariableName
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance SubstitutionOrd RewritingVariableName where
    compareSubstitution (EquationVariableName _) (RuleVariableName _) = LT
    compareSubstitution (EquationVariableName _) (ConfigVariableName _) = LT
    compareSubstitution (RuleVariableName _) (EquationVariableName _) = GT
    compareSubstitution (RuleVariableName _) (ConfigVariableName _) = LT
    compareSubstitution (ConfigVariableName _) (EquationVariableName _) = GT
    compareSubstitution (ConfigVariableName _) (RuleVariableName _) = GT
    compareSubstitution variable1 variable2 =
        on compareSubstitution toVariableName variable1 variable2

instance FreshPartialOrd RewritingVariableName where
    minBoundName =
        \case
            EquationVariableName var -> EquationVariableName (minBoundName var)
            RuleVariableName var -> RuleVariableName (minBoundName var)
            ConfigVariableName var -> ConfigVariableName (minBoundName var)
    {-# INLINE minBoundName #-}

    maxBoundName =
        \case
            EquationVariableName var -> EquationVariableName (maxBoundName var)
            RuleVariableName var -> RuleVariableName (maxBoundName var)
            ConfigVariableName var -> ConfigVariableName (maxBoundName var)
    {-# INLINE maxBoundName #-}

    nextName (EquationVariableName name1) (EquationVariableName name2) =
        EquationVariableName <$> nextName name1 name2
    nextName (RuleVariableName name1) (RuleVariableName name2) =
        RuleVariableName <$> nextName name1 name2
    nextName (ConfigVariableName name1) (ConfigVariableName name2) =
        ConfigVariableName <$> nextName name1 name2
    nextName _ _ = Nothing
    {-# INLINE nextName #-}

instance Unparse RewritingVariableName where
    unparse (EquationVariableName variable) = "Equation" <> unparse variable
    unparse (ConfigVariableName variable) = "Config" <> unparse variable
    unparse (RuleVariableName variable) = "Rule" <> unparse variable

    unparse2 (EquationVariableName variable) = "Equation" <> unparse2 variable
    unparse2 (ConfigVariableName variable) = "Config" <> unparse2 variable
    unparse2 (RuleVariableName variable) = "Rule" <> unparse2 variable

instance From RewritingVariableName VariableName where
    from (EquationVariableName variable) = variable
    from (ConfigVariableName variable) = variable
    from (RuleVariableName variable) = variable

instance From VariableName RewritingVariableName where
    from = RuleVariableName

instance FreshName RewritingVariableName

instance AstWithLocation RewritingVariableName where
    locationFromAst = locationFromAst . base . from @RewritingVariableName @VariableName

type RewritingVariable = Variable RewritingVariableName

mkElementConfigVariable ::
    ElementVariable VariableName ->
    ElementVariable RewritingVariableName
mkElementConfigVariable = (fmap . fmap) ConfigVariableName

configElementVariableFromId ::
    Id -> Sort -> ElementVariable RewritingVariableName
configElementVariableFromId identifier sort =
    mkElementConfigVariable (mkElementVariable identifier sort)

ruleElementVariableFromId ::
    Id -> Sort -> ElementVariable RewritingVariableName
ruleElementVariableFromId identifier sort =
    mkElementRuleVariable (mkElementVariable identifier sort)

mkElementRuleVariable ::
    ElementVariable VariableName ->
    ElementVariable RewritingVariableName
mkElementRuleVariable = (fmap . fmap) RuleVariableName

mkUnifiedRuleVariable ::
    SomeVariable VariableName ->
    SomeVariable RewritingVariableName
mkUnifiedRuleVariable = (fmap . fmap) RuleVariableName

mkUnifiedConfigVariable ::
    SomeVariable VariableName ->
    SomeVariable RewritingVariableName
mkUnifiedConfigVariable = (fmap . fmap) ConfigVariableName

getRuleVariable :: RewritingVariableName -> Maybe VariableName
getRuleVariable (RuleVariableName var) = Just var
getRuleVariable _ = Nothing

getUnifiedRuleVariable ::
    SomeVariable RewritingVariableName ->
    Maybe (SomeVariable VariableName)
getUnifiedRuleVariable = (traverse . traverse) getRuleVariable

{- | Unwrap every variable in the pattern. This is unsafe in
 contexts related to unification. To be used only where the
 rewriting information is not necessary anymore, such as
 unparsing.
-}
getRewritingPattern ::
    Pattern RewritingVariableName ->
    Pattern VariableName
getRewritingPattern = Pattern.mapVariables getRewritingVariable

{- | Unwrap every variable in the term. This is unsafe in
 contexts related to unification. To be used only where the
 rewriting information is not necessary anymore, such as
 unparsing.
-}
getRewritingTerm ::
    TermLike RewritingVariableName ->
    TermLike VariableName
getRewritingTerm = TermLike.mapVariables getRewritingVariable

resetConfigVariable ::
    AdjSomeVariableName
        (RewritingVariableName -> RewritingVariableName)
resetConfigVariable =
    pure (.) <*> pure mkConfigVariable <*> getRewritingVariable

resetRuleVariable ::
    AdjSomeVariableName
        (RewritingVariableName -> RewritingVariableName)
resetRuleVariable =
    pure (.) <*> pure mkRuleVariable <*> getRewritingVariable

getRewritingVariable ::
    AdjSomeVariableName (RewritingVariableName -> VariableName)
getRewritingVariable = pure (from @RewritingVariableName @VariableName)

mkEquationVariable :: VariableName -> RewritingVariableName
mkEquationVariable = EquationVariableName

mkConfigVariable :: VariableName -> RewritingVariableName
mkConfigVariable = ConfigVariableName

mkRuleVariable :: VariableName -> RewritingVariableName
mkRuleVariable = RuleVariableName

isEquationVariable :: RewritingVariableName -> Bool
isEquationVariable (EquationVariableName _) = True
isEquationVariable _ = False

isConfigVariable :: RewritingVariableName -> Bool
isConfigVariable (ConfigVariableName _) = True
isConfigVariable _ = False

isRuleVariable :: RewritingVariableName -> Bool
isRuleVariable (RuleVariableName _) = True
isRuleVariable _ = False

{- | Safely reset all the variables in the pattern to configuration
 variables.
-}
resetResultPattern ::
    HasCallStack =>
    FreeVariables RewritingVariableName ->
    Pattern RewritingVariableName ->
    Pattern RewritingVariableName
resetResultPattern initial config@Conditional{substitution} =
    Pattern.mapVariables resetConfigVariable renamed
  where
    substitution' = Substitution.filter isSomeConfigVariable substitution
    filtered = config{Pattern.substitution = substitution'}
    avoiding =
        initial
            & FreeVariables.toNames
            & (Set.map . fmap) toVariableName
    introduced =
        Set.fromAscList
            . mapMaybe getUnifiedRuleVariable
            . Set.toAscList
            . FreeVariables.toSet
            $ freeVariables filtered
    renaming =
        Map.mapKeysMonotonic (fmap RuleVariableName)
            . Map.map (TermLike.mkVar . mkUnifiedConfigVariable)
            $ refreshVariablesSet avoiding introduced
    renamed :: Pattern RewritingVariableName
    renamed = filtered & substitute renaming

-- | Renames configuration variables to distinguish them from those in the rule.
mkRewritingPattern :: Pattern VariableName -> Pattern RewritingVariableName
mkRewritingPattern = Pattern.mapVariables (pure ConfigVariableName)

mkRewritingTerm :: TermLike VariableName -> TermLike RewritingVariableName
mkRewritingTerm = TermLike.mapVariables (pure mkConfigVariable)

getRemainderPredicate ::
    Predicate RewritingVariableName ->
    Predicate VariableName
getRemainderPredicate predicate =
    Predicate.mapVariables getRewritingVariable predicate
        & assert (all isSomeConfigVariable freeVars)
  where
    freeVars = freeVariables predicate & FreeVariables.toList

assertRemainderPattern ::
    HasCallStack =>
    Pattern RewritingVariableName ->
    Pattern RewritingVariableName
assertRemainderPattern pattern' =
    pattern'
        & assert (all isSomeConfigVariable freeVars)
  where
    freeVars = freeVariables pattern' & FreeVariables.toList

isSomeEquationVariable :: SomeVariable RewritingVariableName -> Bool
isSomeEquationVariable = isSomeEquationVariableName . variableName

isSomeEquationVariableName :: SomeVariableName RewritingVariableName -> Bool
isSomeEquationVariableName = foldSomeVariableName (pure isEquationVariable)

isSomeConfigVariable :: SomeVariable RewritingVariableName -> Bool
isSomeConfigVariable = isSomeConfigVariableName . variableName

isSomeConfigVariableName :: SomeVariableName RewritingVariableName -> Bool
isSomeConfigVariableName = foldSomeVariableName (pure isConfigVariable)

isSomeRuleVariable :: SomeVariable RewritingVariableName -> Bool
isSomeRuleVariable = isSomeRuleVariableName . variableName

isSomeRuleVariableName :: SomeVariableName RewritingVariableName -> Bool
isSomeRuleVariableName = foldSomeVariableName (pure isRuleVariable)

isElementRuleVariable :: ElementVariable RewritingVariableName -> Bool
isElementRuleVariable = isElementRuleVariableName . variableName

isElementRuleVariableName :: ElementVariableName RewritingVariableName -> Bool
isElementRuleVariableName = any isRuleVariable

withoutEquationVariables ::
    FreeVariables RewritingVariableName -> Bool
withoutEquationVariables fv =
    (not . any isSomeEquationVariableName) (toNames fv)
