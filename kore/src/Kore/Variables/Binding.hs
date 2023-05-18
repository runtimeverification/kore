{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Variables.Binding (
    Binding (..),
    matchWith,
    SomeVariableType,
    ElementVariableType,
    SetVariableType,

    -- * Binders
    Binder (..),
    muBinder,
    nuBinder,
) where

import Control.Comonad.Trans.Env
import Control.Lens (
    (%~),
 )
import Control.Lens qualified as Lens
import Data.Binary (Binary)
import Data.Generics.Product (
    field,
 )
import Data.Monoid (
    Any (..),
 )
import GHC.Generics qualified as GHC
import Kore.Syntax.Mu
import Kore.Syntax.Nu
import Kore.Syntax.Variable
import Prelude.Kore

{- | @Binding@ defines traversals for patterns with binders.

@traverseBinder@ and @traverseVariable@ should be empty if the pattern is not
respectively a binder or a variable at the top level.
-}
class Binding binding where
    type VariableType binding

    -- | Traverse the binder at the top of a pattern.
    traverseBinder ::
        Lens.Traversal'
            binding
            (Binder (SomeVariableType binding) binding)
    traverseBinder traversal binding =
        fromMaybe (pure binding) (matchElem <|> matchSet)
      where
        matchSet = matchWith traverseSetBinder traversalSet binding
        matchElem = matchWith traverseElementBinder traversalElem binding
        traversalSet =
            fmap (field @"binderVariable" %~ expectSetVariable)
                . traversal
                . (field @"binderVariable" %~ inject)
        traversalElem =
            fmap (field @"binderVariable" %~ expectElementVariable)
                . traversal
                . (field @"binderVariable" %~ inject)

    -- | Traverse the element variable binder at the top of a pattern.
    traverseElementBinder ::
        Lens.Traversal'
            binding
            (Binder (ElementVariableType binding) binding)

    -- | Traverse the set variable binder at the top of a pattern.
    traverseSetBinder ::
        Lens.Traversal'
            binding
            (Binder (SetVariableType binding) binding)

    -- | Traverse the variable at the top of a pattern.
    traverseVariable :: Lens.Traversal' binding (SomeVariableType binding)

    -- | Traverse the element variable at the top of a pattern.
    traverseElementVariable ::
        Lens.Traversal' binding (ElementVariableType binding)
    traverseElementVariable = traverseVariable . injection
    {-# INLINE traverseElementVariable #-}

    -- | Traverse the element variable at the top of a pattern.
    traverseSetVariable ::
        Lens.Traversal' binding (SetVariableType binding)
    traverseSetVariable = traverseVariable . injection
    {-# INLINE traverseSetVariable #-}

type SomeVariableType binding = SomeVariable (VariableType binding)
type ElementVariableType binding = ElementVariable (VariableType binding)
type SetVariableType binding = SetVariable (VariableType binding)

{- | Apply a traversing function while distinguishing an empty 'Lens.Traversal'.

The result is 'Nothing' if the 'Lens.Traversal' is empty, or 'Just' if the
traversing function was ever applied.
-}
matchWith ::
    Applicative f =>
    -- | 'Lens.Traversal'
    Lens.ATraversal s t a b ->
    -- | Traversing function
    (a -> f b) ->
    s ->
    Maybe (f t)
matchWith (Lens.cloneTraversal -> traverse') = \afb s ->
    let (getAny -> matched, ft) = runEnvT (traverse' (EnvT (Any True) . afb) s)
     in if matched then Just ft else Nothing

-- | @Binder@ abstracts over binders such as 'Exists' and 'Forall'.
data Binder variable child = Binder
    { binderVariable :: variable
    , binderChild :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)

{- | A 'Lens.Lens' to view a 'Mu' as a 'Binder'.

@muBinder@ may be used to implement 'traverseBinder'.

See also: 'nuBinder'.
-}
muBinder ::
    Lens.Lens
        (Mu variable1 child1)
        (Mu variable2 child2)
        (Binder (SetVariable variable1) child1)
        (Binder (SetVariable variable2) child2)
muBinder mapping mu =
    finish <$> mapping binder
  where
    binder =
        Binder{binderVariable = muVariable, binderChild}
      where
        Mu{muVariable} = mu
        Mu{muChild = binderChild} = mu
    finish Binder{binderVariable, binderChild} =
        mu{muVariable = binderVariable, muChild = binderChild}

{- | A 'Lens.Lens' to view a 'Nu' as a 'Binder'.

@nuBinder@ may be used to implement 'traverseBinder'.

See also: 'muBinder'.
-}
nuBinder ::
    Lens.Lens
        (Nu variable1 child1)
        (Nu variable2 child2)
        (Binder (SetVariable variable1) child1)
        (Binder (SetVariable variable2) child2)
nuBinder mapping nu =
    finish <$> mapping binder
  where
    binder =
        Binder{binderVariable = nuVariable, binderChild}
      where
        Nu{nuVariable} = nu
        Nu{nuChild = binderChild} = nu
    finish Binder{binderVariable, binderChild} =
        nu{nuVariable = binderVariable, nuChild = binderChild}
