{- |
Copyright   : (c) Runtime Verification, 2021
License     : NCSA
-}
module Kore.Internal.MultiExists (
    MultiExists,
    unquantified,
    quantify,
    refresh,
    filterRelevant,
) where

import Data.Binary (Binary)
import Data.Map.Strict qualified as Map
import Data.Sequence (
    Seq,
    (<|),
 )
import Data.Sequence qualified as Seq
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables (..),
    freeVariableNames,
    occursIn,
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Internal.Variable (
    ElementVariable,
    SomeVariable,
    SomeVariableName,
    expectElementVariable,
    variableName,
 )
import Kore.Substitute
import Kore.Unparser (
    Unparse (unparse),
 )
import Kore.Variables.Fresh (
    FreshPartialOrd,
    refreshVariables',
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

{- | @MultiExists@ represents a pattern (type @child@) quantified by many
 variables (type @'ElementVariable' variable@), that is:

 @
 \exists{_}(x:_, \exists{_}(y:_, \exists{_}(z:_, child))
 @
-}
data MultiExists variable child = MultiExists
    { existsVariables :: !(Seq (ElementVariable variable))
    , existsChild :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance
    (Unparse variable, Pretty child) =>
    Pretty (MultiExists variable child)
    where
    pretty MultiExists{existsVariables, existsChild} =
        Pretty.vsep
            [ Pretty.vsep (mkExistsDoc <$> toList existsVariables)
            , pretty existsChild
            , mconcat (")" <$ toList existsVariables)
            ]
      where
        mkExistsDoc variable = "\\exists{_}(" <> unparse variable <> ","
    {-# INLINE pretty #-}

instance
    (Ord variable, HasFreeVariables child variable) =>
    HasFreeVariables (MultiExists variable child) variable
    where
    freeVariables multiExists =
        FreeVariables.bindVariables
            (inject <$> existsVariables)
            (freeVariables existsChild)
      where
        MultiExists{existsVariables, existsChild} = multiExists

instance
    ( FreshPartialOrd variable
    , Substitute child
    , Semigroup child
    , VariableNameType child ~ variable
    ) =>
    Semigroup (MultiExists variable child)
    where
    (<>) exists1 exists2 =
        MultiExists variables' child'
      where
        variables' = variables1' <> variables2'
        child' = child1' <> child2'
        avoid2 = allVariableNames exists1
        exists2' = refresh avoid2 exists2
        MultiExists variables2' child2' = exists2'
        avoid1 = allVariableNames exists2'
        exists1' = refresh avoid1 exists1
        MultiExists variables1' child1' = exists1'

-- | All (free and bound) variable names of a 'MultiExists'.
allVariableNames ::
    Ord variable =>
    HasFreeVariables child variable =>
    MultiExists variable child ->
    Set (SomeVariableName variable)
allVariableNames MultiExists{existsVariables, existsChild} =
    FreeVariables.toNames (freeVariables existsChild)
        <> (Set.fromList . map (inject . variableName) . toList) existsVariables

refresh ::
    FreshPartialOrd variable =>
    Substitute child =>
    VariableNameType child ~ variable =>
    Set (SomeVariableName variable) ->
    MultiExists variable child ->
    MultiExists variable child
refresh extraAvoiding multiExists@MultiExists{existsVariables, existsChild} =
    MultiExists
        { existsVariables = expectElementVariable <$> someVariables'
        , existsChild = rename (Map.mapKeysMonotonic variableName refreshing) existsChild
        }
  where
    avoiding = freeVariableNames multiExists <> extraAvoiding
    someVariables = inject @(SomeVariable _) <$> existsVariables
    (someVariables', refreshing) = refreshVariables' avoiding someVariables

{- | Quantify over a variable in a 'MultiExists'. If the variable is redundant
(does not occur free in the 'MultiExists') then it will be omitted.
-}
quantify ::
    forall variable child.
    Ord variable =>
    HasFreeVariables child variable =>
    ElementVariable variable ->
    MultiExists variable child ->
    MultiExists variable child
quantify elementVariable ~original@MultiExists{existsVariables, existsChild}
    | someVariableName `occursIn` existsChild =
        MultiExists
            { existsVariables = elementVariable <| existsVariables
            , existsChild
            }
    | otherwise = original
  where
    someVariableName :: SomeVariableName variable
    someVariableName = inject (variableName elementVariable)

-- | Construct a 'MultiExists' containing @child@ without any quantifiers.
unquantified :: child -> MultiExists variable child
unquantified = MultiExists Seq.empty
{-# INLINE unquantified #-}

{- | Filter the variables of a 'MultiExists', keeping only those which actually
 quantify a free variable in @child@.
-}
filterRelevant ::
    forall variable child.
    Ord variable =>
    HasFreeVariables child variable =>
    MultiExists variable child ->
    MultiExists variable child
filterRelevant multiExists =
    case existsVariables multiExists of
        Seq.Empty -> multiExists
        x Seq.:<| xs
            | someVariableName `occursIn` inner ->
                quantify x (filterRelevant inner)
            | otherwise -> filterRelevant inner
          where
            someVariableName :: SomeVariableName variable
            someVariableName = inject (variableName x)
            inner = multiExists{existsVariables = xs}
