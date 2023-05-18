{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Kore.Internal.MultiOr
Description : Data structures and functions for manipulating
              Or with any number of children.
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Internal.MultiOr (
    MultiOr,
    bottom,
    flatten,
    distributeApplication,
    gather,
    observeAllT,
    observeAll,
    make,
    merge,
    mergeAll,
    singleton,
    map,
    traverse,
    traverseOr,
    patternToMaybeBool,

    -- * Re-exports
    Alternative (..),
) where

import Data.Binary (Binary)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable qualified as Traversable
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables (..),
 )
import Kore.Debug
import Kore.Internal.Variable (
    InternalVariable,
 )
import Kore.Substitute
import Kore.Syntax.Application (
    Application (..),
 )
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser (
    unparseAssoc',
 )
import Logic (
    Logic,
    LogicT,
    MonadLogic,
 )
import Logic qualified
import Prelude.Kore hiding (
    map,
    traverse,
 )
import Pretty (
    Pretty (..),
 )

-- | 'MultiOr' is a Matching logic or of its children
data MultiOr child
    = MultiOrTop child
    | MultiOrBottom
    | MultiOr (Set child)
    deriving stock (Eq, Ord, Show, Foldable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Hashable child => Hashable (MultiOr child) where
    hashWithSalt salt = \case
        MultiOrTop child -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` child
        MultiOrBottom -> salt `hashWithSalt` (1 :: Int)
        MultiOr children -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` (Set.toList children)

instance Debug child => Debug (MultiOr child)

instance (Debug child, Diff child) => Diff (MultiOr child)

instance Pretty child => Pretty (MultiOr child) where
    pretty = unparseAssoc' "\\or{_}" "\\bottom{_}()" . (<$>) pretty . toList
    {-# INLINE pretty #-}

instance Ord child => Semigroup (MultiOr child) where
    a@(MultiOrTop _) <> _ = a
    _ <> b@(MultiOrTop _) = b
    MultiOrBottom <> b = b
    a <> MultiOrBottom = a
    (MultiOr a) <> (MultiOr b) = MultiOr (a <> b)

instance Ord child => Monoid (MultiOr child) where
    mempty = MultiOrBottom

instance TopBottom (MultiOr child) where
    isTop (MultiOrTop _) = True
    isTop _ = False
    isBottom MultiOrBottom = True
    isBottom _ = False

instance TopBottom child => From child (MultiOr child) where
    from = singleton

instance (Ord child, TopBottom child) => From [child] (MultiOr child) where
    from = make

instance From (MultiOr child) [child] where
    from = toList

instance
    (Ord variable, HasFreeVariables child variable) =>
    HasFreeVariables (MultiOr child) variable
    where
    freeVariables = foldMap freeVariables
    {-# INLINE freeVariables #-}

instance
    ( InternalVariable (VariableNameType child)
    , Ord child
    , TopBottom child
    , Substitute child
    ) =>
    Substitute (MultiOr child)
    where
    type TermType (MultiOr child) = TermType child

    type VariableNameType (MultiOr child) = VariableNameType child

    substitute = map . substitute
    {-# INLINE substitute #-}

    rename = map . rename
    {-# INLINE rename #-}

bottom :: MultiOr term
bottom = MultiOrBottom

-- | Construct a normalized 'MultiOr' from a single pattern.
singleton ::
    TopBottom term =>
    term ->
    MultiOr term
singleton term
    | isBottom term = MultiOrBottom
    | isTop term = MultiOrTop term
    | otherwise = MultiOr $ Set.singleton term

distributeApplication ::
    Ord head =>
    Ord term =>
    TopBottom term =>
    Application head (MultiOr term) ->
    MultiOr (Application head term)
distributeApplication
    Application
        { applicationSymbolOrAlias
        , applicationChildren
        } =
        foldr
            (curry go)
            (singleton application)
            applicationChildren
      where
        go = \case
            (MultiOrBottom, _) -> MultiOrBottom
            (_, MultiOrBottom) -> MultiOrBottom
            (mor, mapp) -> make [applyTo child app | child <- toList mor, app <- toList mapp]
        applyTo term Application{applicationSymbolOrAlias = alias, applicationChildren = children} =
            Application{applicationSymbolOrAlias = alias, applicationChildren = term : children}
        application =
            Application{applicationSymbolOrAlias, applicationChildren = []}

{- | 'flatten' transforms a MultiOr (MultiOr term)
into a (MultiOr term) by or-ing all the inner elements.
-}
flatten ::
    Ord term =>
    MultiOr (MultiOr term) ->
    MultiOr term
flatten = \case
    MultiOrBottom -> MultiOrBottom
    MultiOrTop (MultiOrTop child) -> MultiOrTop child
    MultiOrTop MultiOrBottom -> error "flatten: MultiOrTop MultiOrBottom is undefined!"
    MultiOrTop (MultiOr children) -> case toList children of
        [] -> error "flatten: invalid MultiOr children"
        child : _ -> MultiOrTop child
    MultiOr ors -> mergeAll ors

{- | Does a very simple attempt to check whether a pattern
is top or bottom.
-}
patternToMaybeBool ::
    TopBottom term =>
    term ->
    Maybe Bool
patternToMaybeBool patt
    | isTop patt = Just True
    | isBottom patt = Just False
    | otherwise = Nothing

{- | 'make' constructs a normalized 'MultiOr'. It simplifies a set of children according to the `patternToMaybeBool`
function which evaluates to true/false/unknown.
-}
make ::
    (Ord term, TopBottom term, Foldable f) =>
    f term ->
    MultiOr term
make ~patts = foldr go stop patts Set.empty
  where
    go element ~r !es =
        case patternToMaybeBool element of
            Just True -> MultiOrTop element
            Just False -> r es
            Nothing -> r (Set.insert element es)
    stop es
        | null es = MultiOrBottom
        | otherwise = MultiOr es

{- | Merge two disjunctions of items.

The result is simplified with the 'filterOr' function.
-}
merge ::
    Ord term =>
    MultiOr term ->
    MultiOr term ->
    MultiOr term
merge = (<>)

{- | Merge any number of disjunctions of items.

The result is simplified with the 'filterOr' function.
-}
mergeAll ::
    (Ord term, Foldable f) =>
    f (MultiOr term) ->
    MultiOr term
mergeAll = foldl' (<>) mempty

gather :: (Ord a, TopBottom a, MonadLogic m) => m a -> m (MultiOr a)
gather act = make <$> Logic.gather act
{-# INLINE gather #-}

observeAllT :: (Ord a, TopBottom a, Monad m) => LogicT m a -> m (MultiOr a)
observeAllT act = make <$> Logic.observeAllT act
{-# INLINE observeAllT #-}

observeAll :: (Ord a, TopBottom a) => Logic a -> MultiOr a
observeAll = make . Logic.observeAll
{-# INLINE observeAll #-}

map ::
    Ord child2 =>
    TopBottom child2 =>
    (child1 -> child2) ->
    MultiOr child1 ->
    MultiOr child2
map f = make . fmap f . toList
{-# INLINE map #-}

-- | Warning: 'traverse' should not be used with 'LogicT'.
traverse ::
    Ord child2 =>
    TopBottom child2 =>
    Applicative f =>
    (child1 -> f child2) ->
    MultiOr child1 ->
    f (MultiOr child2)
traverse f = fmap make . Traversable.traverse f . toList
{-# INLINE traverse #-}

-- | Traverse a @MultiOr@ using an action that returns a disjunction.
traverseOr ::
    Ord child2 =>
    Applicative f =>
    (child1 -> f (MultiOr child2)) ->
    MultiOr child1 ->
    f (MultiOr child2)
traverseOr f = fmap fold . traverse f
{-# INLINE traverseOr #-}
