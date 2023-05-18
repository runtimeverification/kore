{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.PredicatePattern (
    PredicatePattern (PredicatePattern, freeVariables),
    -- simplified is excluded on purpose
    simplifiedAttribute,
    isSimplified,
    isSimplifiedAnyCondition,
    isSimplifiedSomeCondition,
    setSimplified,
    mapVariables,
    traverseVariables,
    deleteFreeVariable,

    -- * Re-exports
    module Kore.Attribute.Pattern.FreeVariables,
) where

import Control.Lens qualified as Lens
import Data.Binary (Binary)
import Data.Generics.Product
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables hiding (
    freeVariables,
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables (
    freeVariables,
 )
import Kore.Attribute.Pattern.Simplified hiding (
    isSimplified,
    isSimplifiedAnyCondition,
    isSimplifiedSomeCondition,
 )
import Kore.Attribute.Pattern.Simplified qualified as Simplified (
    isSimplified,
    isSimplifiedAnyCondition,
    isSimplifiedSomeCondition,
 )
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Internal.SideCondition.SideCondition qualified as SideCondition (
    Representation,
 )
import Kore.Syntax.Variable
import Prelude.Kore

-- | @Pattern@ are the attributes of a pattern collected during verification.
data PredicatePattern variable = PredicatePattern
    { freeVariables :: !(FreeVariables variable)
    , simplified :: !Simplified
    }
    deriving stock (Eq, GHC.Generic, Show)
    deriving anyclass (Binary)

instance NFData variable => NFData (PredicatePattern variable)

instance Hashable variable => Hashable (PredicatePattern variable)

instance SOP.Generic (PredicatePattern variable)

instance SOP.HasDatatypeInfo (PredicatePattern variable)

instance Debug variable => Debug (PredicatePattern variable) where
    debugPrecBrief _ _ = "_"

instance (Debug variable, Diff variable) => Diff (PredicatePattern variable)

instance
    ( Synthetic (FreeVariables variable) base
    , Synthetic Simplified base
    ) =>
    Synthetic (PredicatePattern variable) base
    where
    synthetic base =
        PredicatePattern
            { freeVariables = synthetic (freeVariables <$> base)
            , simplified = synthetic (simplified <$> base)
            }

simplifiedAttribute :: PredicatePattern variable -> Simplified
simplifiedAttribute PredicatePattern{simplified} = simplified

{- Checks whether the pattern is simplified relative to the given side
condition.
-}
isSimplified ::
    SideCondition.Representation -> PredicatePattern variable -> Bool
isSimplified sideCondition = Simplified.isSimplified sideCondition . simplifiedAttribute

{- Checks whether the pattern is simplified relative to some side condition.
-}
isSimplifiedSomeCondition ::
    PredicatePattern variable -> Bool
isSimplifiedSomeCondition =
    Simplified.isSimplifiedSomeCondition . simplifiedAttribute

{- Checks whether the pattern is simplified relative to any side condition.
-}
isSimplifiedAnyCondition :: PredicatePattern variable -> Bool
isSimplifiedAnyCondition PredicatePattern{simplified} =
    Simplified.isSimplifiedAnyCondition simplified

setSimplified ::
    Simplified ->
    PredicatePattern variable ->
    PredicatePattern variable
setSimplified simplified patt = patt{simplified}

{- | Use the provided mapping to replace all variables in a 'Pattern'.

See also: 'traverseVariables'
-}
mapVariables ::
    Ord variable2 =>
    AdjSomeVariableName (variable1 -> variable2) ->
    PredicatePattern variable1 ->
    PredicatePattern variable2
mapVariables adj = Lens.over (field @"freeVariables") (mapFreeVariables adj)

{- | Use the provided traversal to replace the free variables in a 'Pattern'.

See also: 'mapVariables'
-}
traverseVariables ::
    forall m variable1 variable2.
    Monad m =>
    Ord variable2 =>
    AdjSomeVariableName (variable1 -> m variable2) ->
    PredicatePattern variable1 ->
    m (PredicatePattern variable2)
traverseVariables adj = field @"freeVariables" (traverseFreeVariables adj)

-- | Delete the given variable from the set of free variables.
deleteFreeVariable ::
    Ord variable =>
    SomeVariable variable ->
    PredicatePattern variable ->
    PredicatePattern variable
deleteFreeVariable variable =
    Lens.over (field @"freeVariables") (bindVariable variable)

instance HasFreeVariables (PredicatePattern variable) variable where
    freeVariables = freeVariables
