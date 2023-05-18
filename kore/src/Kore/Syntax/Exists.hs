{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.Exists (
    Exists (..),
    refreshExists,
    existsBinder,
) where

import Control.Lens qualified as Lens
import Data.Binary (Binary)
import Data.Set (
    Set,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Substitute
import Kore.Syntax.Variable
import Kore.Unparser
import Kore.Variables.Binding (
    Binder (..),
 )
import Kore.Variables.Fresh (FreshPartialOrd)
import Prelude.Kore
import Pretty qualified

{- | 'Exists' corresponds to the @\\exists@ branch of the @matching-logic-pattern@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#patterns kore-syntax.md#patterns>.

'existsSort' is both the sort of the operands and the sort of the result.
-}
data Exists sort variable child = Exists
    { existsSort :: !sort
    , existsVariable :: !(ElementVariable variable)
    , existsChild :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance
    (Unparse variable, Unparse child) =>
    Unparse (Exists Sort variable child)
    where
    unparse Exists{existsSort, existsVariable, existsChild} =
        "\\exists"
            <> parameters [existsSort]
            <> arguments' [unparse existsVariable, unparse existsChild]

    unparse2 Exists{existsVariable, existsChild} =
        Pretty.parens
            ( Pretty.fillSep
                [ "\\exists"
                , unparse2SortedVariable existsVariable
                , unparse2 existsChild
                ]
            )

instance
    (Unparse variable, Unparse child) =>
    Unparse (Exists () variable child)
    where
    unparse Exists{existsVariable, existsChild} =
        "\\exists"
            <> arguments' [unparse existsVariable, unparse existsChild]

    unparse2 Exists{existsVariable, existsChild} =
        Pretty.parens
            ( Pretty.fillSep
                [ "\\exists"
                , unparse2SortedVariable existsVariable
                , unparse2 existsChild
                ]
            )

instance
    Ord variable =>
    Synthetic (FreeVariables variable) (Exists sort variable)
    where
    synthetic Exists{existsVariable, existsChild} =
        bindVariable (inject existsVariable) existsChild
    {-# INLINE synthetic #-}

instance Synthetic Sort (Exists Sort variable) where
    synthetic Exists{existsSort, existsChild} =
        existsSort `sameSort` existsChild
    {-# INLINE synthetic #-}

instance
    (Ord variable, HasFreeVariables child variable) =>
    HasFreeVariables (Exists sort variable child) variable
    where
    freeVariables Exists{existsVariable, existsChild} =
        bindVariable (inject existsVariable) (freeVariables existsChild)

{- | A 'Lens.Lens' to view an 'Exists' as a 'Binder'.

'existsBinder' may be used to implement 'Kore.Variables.Binding.traverseBinder'.

See also: 'Kore.Syntax.Forall.forallBinder'.
-}
existsBinder ::
    Lens.Lens
        (Exists sort variable1 child1)
        (Exists sort variable2 child2)
        (Binder (ElementVariable variable1) child1)
        (Binder (ElementVariable variable2) child2)
existsBinder mapping exists =
    finish <$> mapping binder
  where
    binder =
        Binder
            { binderVariable = existsVariable
            , binderChild = existsChild
            }
      where
        Exists{existsVariable, existsChild} = exists
    finish Binder{binderVariable, binderChild} =
        exists{existsVariable = binderVariable, existsChild = binderChild}

refreshExists ::
    forall sort variable child.
    Substitute child =>
    VariableNameType child ~ variable =>
    FreshPartialOrd variable =>
    Set (SomeVariableName variable) ->
    Exists sort variable child ->
    Exists sort variable child
refreshExists extraAvoid existsF =
    Lens.over existsBinder (refreshElementBinder avoid) existsF
  where
    avoid = freeVariableNames existsF <> extraAvoid
{-# INLINE refreshExists #-}
