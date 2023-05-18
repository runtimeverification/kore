{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Rewrite.Implication (
    Implication (..),
    freeVariablesLeft,
    freeVariablesRight,
    mkImplication,
    substitute,
    assertRefreshed,
    refreshExistentials,
    applySubstitution,
    termToExistentials,
    resetConfigVariables,
    forgetSimplified,
    parseRightHandSide,
    implicationToTerm,
) where

import Control.Error.Util (
    hush,
 )
import Control.Monad.State.Strict (
    evalState,
 )
import Control.Monad.State.Strict qualified as State
import Data.Binary (Binary)
import Data.Default qualified as Default
import Data.Foldable qualified as Foldable
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
    HasFreeVariables (..),
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Debug
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.Substitution (
    Substitution,
 )
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.Symbol (
    Symbol,
 )
import Kore.Internal.TermLike (
    ElementVariable,
    InternalVariable,
    Modality,
    SomeVariable,
    SomeVariableName (..),
    Sort,
    TermLike,
    Variable (..),
    VariableName,
    mkVar,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
    getRewritingTerm,
    resetConfigVariable,
 )
import Kore.Rewrite.UnifyingRule (
    UnifyingRule (..),
 )
import Kore.Substitute
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser (
    Unparse (..),
 )
import Kore.Variables.Fresh (
    refreshVariables,
    refreshVariablesSet,
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

-- | Representation of reachability claim types as well as rules
data Implication modality = Implication
    { left :: !(Pattern RewritingVariableName)
    , modality :: !modality
    , existentials :: ![ElementVariable RewritingVariableName]
    , right :: !(OrPattern RewritingVariableName)
    , attributes :: !(Attribute.Axiom Symbol RewritingVariableName)
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance From (Implication modality) Attribute.SourceLocation where
    from = Attribute.sourceLocation . attributes

instance From (Implication modality) Attribute.Label where
    from = Attribute.label . attributes

instance From (Implication modality) Attribute.RuleIndex where
    from = Attribute.identifier . attributes

instance Pretty (Implication modality) where
    pretty implication'@(Implication _ _ _ _ _) =
        Pretty.vsep
            [ "left:"
            , Pretty.indent 4 (unparse left)
            , "existentials:"
            , Pretty.indent 4 (Pretty.list $ unparse <$> existentials)
            , "right:"
            , Pretty.indent 4 (pretty right)
            ]
      where
        Implication
            { left
            , right
            , existentials
            } = implication'

instance TopBottom (Implication modality) where
    isTop _ = False
    isBottom _ = False

instance From (Implication modality) Attribute.PriorityAttributes where
    from = from @(Attribute.Axiom _ _) . attributes

getImplicationSort ::
    Implication modality ->
    Sort
getImplicationSort (Implication left _ _ _ _) = Pattern.patternSort left

freeVariablesRight ::
    Implication modality ->
    FreeVariables RewritingVariableName
freeVariablesRight implication'@(Implication _ _ _ _ _) =
    freeVariables
        ( TermLike.mkExistsN
            existentials
            (OrPattern.toTermLike sort right)
        )
  where
    sort = getImplicationSort implication'
    Implication{right, existentials} = implication'

freeVariablesLeft ::
    Implication modality ->
    FreeVariables RewritingVariableName
freeVariablesLeft implication'@(Implication _ _ _ _ _) =
    freeVariables left
  where
    Implication{left} = implication'

instance HasFreeVariables (Implication modality) RewritingVariableName where
    freeVariables implication'@(Implication _ _ _ _ _) =
        freeVariablesLeft implication'
            <> freeVariablesRight implication'

instance Substitute (Implication modality) where
    type TermType (Implication modality) = TermLike RewritingVariableName

    type VariableNameType (Implication modality) = RewritingVariableName

    substitute subst implication'@(Implication _ _ _ _ _) =
        substituteRight subst $
            implication'
                { left = substitute subst left
                }
      where
        Implication{left} = implication'

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

{- | Creates an 'Implication' from a left hand side 'Pattern'
 and an 'OrPattern', representing the right hand side pattern.
 The list of element variables are existentially quantified
 in the right hand side.
-}
mkImplication ::
    modality ->
    Pattern RewritingVariableName ->
    OrPattern RewritingVariableName ->
    [ElementVariable RewritingVariableName] ->
    Implication modality
mkImplication modality left right existentials =
    Implication
        { left
        , right
        , modality
        , existentials
        , attributes = Default.def
        }

{- | Construct a 'TermLike' from the parts of an implication-based rule.

The 'TermLike' has the following form:

@
\\implies{S}(\\and{S}(left, requires), modality{S}(\\exists{S}({Xₙ:Sₙ}, right))
@

that is,

@
left ∧ requires → modality(∃ {Xₙ:Sₙ}. right)
@
-}
implicationToTerm ::
    Implication Modality ->
    TermLike VariableName
implicationToTerm representation@(Implication _ _ _ _ _) =
    TermLike.mkImplies
        (TermLike.mkAnd leftCondition leftTerm)
        (TermLike.applyModality modality rightPattern)
  where
    Implication{left, modality, right, existentials} = representation
    leftTerm =
        Pattern.term left
            & getRewritingTerm
    sort = getImplicationSort representation
    leftCondition =
        Pattern.withoutTerm left
            & Condition.toPredicate
            & Predicate.fromPredicate sort
            & getRewritingTerm
    rightPattern =
        TermLike.mkExistsN existentials (OrPattern.toTermLike sort right)
            & getRewritingTerm

substituteRight ::
    Map
        (SomeVariableName RewritingVariableName)
        (TermLike RewritingVariableName) ->
    Implication modality ->
    Implication modality
substituteRight subst0 implication'@Implication{right, existentials} =
    implication'
        { right = substitute newSubst right
        , existentials = renameVariable <$> existentials
        }
  where
    existentials' = inject . TermLike.variableName <$> existentials
    subst = foldr Map.delete subst0 existentials'
    newFreeVars =
        Foldable.foldl'
            Set.union
            Set.empty
            ( FreeVariables.toNames . freeVariables @_ @RewritingVariableName
                <$> subst
            )

    freeVars = FreeVariables.toNames $ freeVariablesRight implication'

    avoid = Set.union newFreeVars (freeVars Set.\\ Map.keysSet subst)
    subst' = refreshVariables avoid (inject <$> existentials)
    newSubst = Map.union subst (TermLike.mkVar <$> subst')

    renameVariable ::
        ElementVariable RewritingVariableName ->
        ElementVariable RewritingVariableName
    renameVariable var =
        let name = SomeVariableNameElement $ variableName var
         in maybe var TermLike.expectElementVariable $
                Map.lookup name subst'

{- | Applies a substitution to an implication and checks that
 it was fully applied, i.e. there is no substitution
 variable left in the implication.
-}
applySubstitution ::
    HasCallStack =>
    Substitution RewritingVariableName ->
    Implication modality ->
    Implication modality
applySubstitution substitution implication' =
    if finalImplication `isFreeOf` substitutedVariables
        then finalImplication
        else error "Internal error: substitution not fully applied to Implication"
  where
    subst = Substitution.toMap substitution
    finalImplication = substitute subst implication'
    substitutedVariables = Substitution.variables substitution

-- | Is the rule free of the given variables?
isFreeOf ::
    Implication modality ->
    Set.Set (SomeVariable RewritingVariableName) ->
    Bool
isFreeOf rule =
    Set.disjoint $
        FreeVariables.toSet $
            freeVariables rule

-- TODO(Ana): move this to Internal.TermLike?

-- | Extracts all top level existential quantifications.
termToExistentials ::
    TermLike RewritingVariableName ->
    (TermLike RewritingVariableName, [ElementVariable RewritingVariableName])
termToExistentials (TermLike.Exists_ _ v term) =
    fmap (v :) (termToExistentials term)
termToExistentials term = (term, [])

forgetSimplified :: Implication modality -> Implication modality
forgetSimplified implication'@(Implication _ _ _ _ _) =
    implication'
        { left = Pattern.forgetSimplified left
        , right = OrPattern.forgetSimplified right
        }
  where
    Implication{left, right} = implication'

{- | Ensure that the 'Implication''s bound variables are fresh.

The 'existentials' should not appear free on the left-hand side so that we can
freely unwrap the right-hand side as needed.

See also: 'refreshExistentials'
-}
assertRefreshed :: HasCallStack => Implication modality -> a -> a
assertRefreshed implication'@Implication{existentials} =
    assert (isFreeOf implication' (Set.fromList $ inject <$> existentials))

-- | Refresh the 'existentials' of the 'Implication'.
refreshExistentials :: Implication modality -> Implication modality
refreshExistentials = snd . refreshRule mempty

instance UnifyingRule (Implication modality) where
    type UnifyingRuleVariable (Implication modality) = RewritingVariableName

    matchingPattern implication'@(Implication _ _ _ _ _) =
        Pattern.term left
      where
        Implication{left} = implication'

    precondition implication'@(Implication _ _ _ _ _) =
        Condition.toPredicate . Pattern.withoutTerm $ left
      where
        Implication{left} = implication'

    refreshRule stale implication'@(Implication _ _ _ _ _) =
        do
            let variables = freeVariables implication' & FreeVariables.toSet
            renaming <- refreshVariables' variables
            refreshedImplication <- renameExistentials
            let fullyRefreshedImplication =
                    substitute
                        (TermLike.mkVar <$> renaming)
                        refreshedImplication
            -- Only return the renaming of free variables.
            -- Renaming the bound variables is invisible from outside.
            pure (renaming, fullyRefreshedImplication)
            & flip evalState (FreeVariables.toNames stale)
      where
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

        Implication{right, existentials} = implication'

        renameExistentials = do
            let existentials' = Set.fromList (inject <$> existentials)
            subst0 <- refreshVariables' existentials'
            let renameVariable var =
                    Map.lookup (inject (variableName var)) subst0
                        & maybe var TermLike.expectElementVariable
                subst = TermLike.mkVar <$> subst0
            pure $
                implication'
                    { right = substitute subst right
                    , existentials = renameVariable <$> existentials
                    }

resetConfigVariables :: Implication modality -> Implication modality
resetConfigVariables implication'@(Implication _ _ _ _ _) =
    implication'
        { left =
            Pattern.mapVariables resetConfigVariable left
        , right =
            OrPattern.mapVariables resetConfigVariable right
        , existentials =
            TermLike.mapElementVariable resetConfigVariable
                <$> existentials
        }
  where
    Implication{left, right, existentials} = implication'

parseRightHandSide ::
    forall variable.
    InternalVariable variable =>
    TermLike variable ->
    OrPattern variable
parseRightHandSide term =
    let (term', condition) =
            parsePatternFromTermLike term
                & Pattern.splitTerm
     in OrPattern.map
            (flip Pattern.andCondition condition)
            (parseOrPatternFromTermLike term')
  where
    parseOrPatternFromTermLike ::
        TermLike variable ->
        OrPattern variable
    parseOrPatternFromTermLike (TermLike.Or_ _ term1 term2) =
        parseOrPatternFromTermLike term1
            <> parseOrPatternFromTermLike term2
    parseOrPatternFromTermLike term' =
        OrPattern.fromPattern
            . parsePatternFromTermLike
            $ term'

    parsePatternFromTermLike ::
        TermLike variable ->
        Pattern variable
    parsePatternFromTermLike original@(TermLike.And_ _ term1 term2)
        | isTop term1 = Pattern.fromTermLike term2
        | isTop term2 = Pattern.fromTermLike term1
        | otherwise =
            case (tryPredicate term1, tryPredicate term2) of
                (Nothing, Nothing) ->
                    Pattern.fromTermLike original
                (Just predicate, Nothing) ->
                    Pattern.fromTermAndPredicate
                        term2
                        predicate
                (Nothing, Just predicate) ->
                    Pattern.fromTermAndPredicate
                        term1
                        predicate
                (Just predicate, _) ->
                    Pattern.fromTermAndPredicate
                        term2
                        predicate
      where
        tryPredicate = hush . Predicate.makePredicate
    parsePatternFromTermLike term' = Pattern.fromTermLike term'
