{-# LANGUAGE Trustworthy #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.Pattern (
    Pattern (..),
    asPattern,
    fromPattern,
    eraseAnnotations,
    traverseVariables,
    mapVariables,
    asConcretePattern,
    isConcrete,
    fromConcretePattern,

    -- * Re-exports
    Base,
    CofreeF (..),
    PatternF (..),
    Const (..),
    module Control.Comonad,
) where

import Control.Comonad
import Control.Comonad.Trans.Cofree (
    ComonadCofree (..),
 )
import Control.Comonad.Trans.Env qualified as Env
import Data.Bifunctor qualified as Bifunctor
import Data.Functor.Compose (
    Compose (..),
 )
import Data.Functor.Foldable (
    Base,
    Corecursive,
    Recursive,
 )
import Data.Functor.Foldable qualified as Recursive
import Data.Functor.Identity (
    Identity (..),
 )
import Data.Kind (
    Type,
 )
import Data.Monoid (
    All (..),
 )
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Null qualified as Attribute
import Kore.Debug
import Kore.Sort
import Kore.Syntax.And
import Kore.Syntax.Application
import Kore.Syntax.Bottom
import Kore.Syntax.Ceil
import Kore.Syntax.DomainValue
import Kore.Syntax.Equals
import Kore.Syntax.Exists
import Kore.Syntax.Floor
import Kore.Syntax.Forall
import Kore.Syntax.Iff
import Kore.Syntax.Implies
import Kore.Syntax.In
import Kore.Syntax.Inhabitant
import Kore.Syntax.Mu
import Kore.Syntax.Next
import Kore.Syntax.Not
import Kore.Syntax.Nu
import Kore.Syntax.Or
import Kore.Syntax.PatternF (
    Const (..),
    PatternF (..),
 )
import Kore.Syntax.PatternF qualified as PatternF
import Kore.Syntax.Rewrites
import Kore.Syntax.StringLiteral
import Kore.Syntax.Top
import Kore.Syntax.Variable
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser
import Prelude.Kore
import Pretty qualified
import SQL qualified
import Unsafe.Coerce (unsafeCoerce)

{- | The abstract syntax of Kore.

@variable@ is the family of variable types.

@annotation@ is the type of annotations decorating each node of the abstract
syntax tree. 'Pattern' is a 'Traversable' 'Comonad' over the type of
annotations.
-}
newtype
    Pattern
        (variable :: Type)
        (annotation :: Type) = Pattern
    {getPattern :: Cofree (PatternF variable) annotation}
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving stock (Traversable)
    deriving newtype (Functor, Foldable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Eq variable => Eq (Pattern variable annotation) where
    (==) = eqWorker
      where
        eqWorker
            (Recursive.project -> _ :< pat1)
            (Recursive.project -> _ :< pat2) =
                pat1 == pat2
    {-# INLINE (==) #-}

instance Ord variable => Ord (Pattern variable annotation) where
    compare = compareWorker
      where
        compareWorker
            (Recursive.project -> _ :< pat1)
            (Recursive.project -> _ :< pat2) =
                compare pat1 pat2
    {-# INLINE compare #-}

instance Hashable variable => Hashable (Pattern variable annotation) where
    hashWithSalt salt (Recursive.project -> _ :< pat) = hashWithSalt salt pat
    {-# INLINE hashWithSalt #-}

instance
    (NFData annotation, NFData variable) =>
    NFData (Pattern variable annotation)
    where
    rnf (Recursive.project -> annotation :< pat) =
        rnf annotation `seq` rnf pat

instance
    (Unparse variable) =>
    Unparse (Pattern variable annotation)
    where
    unparse (Recursive.project -> _ :< pat) = unparse pat
    unparse2 (Recursive.project -> _ :< pat) = unparse2 pat

type instance
    Base (Pattern variable annotation) =
        CofreeF (PatternF variable) annotation

-- This instance implements all class functions for the Pattern newtype
-- because the their implementations for the inner type may be specialized.
instance Recursive (Pattern variable annotation) where
    project = \(Pattern embedded) ->
        case Recursive.project embedded of
            Compose (Identity projected) -> Pattern <$> projected
    {-# INLINE project #-}

    -- This specialization is particularly important: The default implementation
    -- of 'cata' in terms of 'project' would involve an extra call to 'fmap' at
    -- every level of the tree due to the implementation of 'project' above.
    cata alg = \(Pattern fixed) ->
        Recursive.cata
            (\(Compose (Identity base)) -> alg base)
            fixed
    {-# INLINE cata #-}

    para alg = \(Pattern fixed) ->
        Recursive.para
            ( \(Compose (Identity base)) ->
                alg (Bifunctor.first Pattern <$> base)
            )
            fixed
    {-# INLINE para #-}

    gpara dist alg = \(Pattern fixed) ->
        Recursive.gpara
            (\(Compose (Identity base)) -> Compose . Identity <$> dist base)
            (\(Compose (Identity base)) -> alg (Env.local Pattern <$> base))
            fixed
    {-# INLINE gpara #-}

    prepro pre alg = \(Pattern fixed) ->
        Recursive.prepro
            (\(Compose (Identity base)) -> (Compose . Identity) (pre base))
            (\(Compose (Identity base)) -> alg base)
            fixed
    {-# INLINE prepro #-}

    gprepro dist pre alg = \(Pattern fixed) ->
        Recursive.gprepro
            (\(Compose (Identity base)) -> Compose . Identity <$> dist base)
            (\(Compose (Identity base)) -> (Compose . Identity) (pre base))
            (\(Compose (Identity base)) -> alg base)
            fixed
    {-# INLINE gprepro #-}

-- This instance implements all class functions for the Pattern newtype
-- because the their implementations for the inner type may be specialized.
instance Corecursive (Pattern variable annotation) where
    embed = \projected ->
        (Pattern . Recursive.embed . Compose . Identity)
            (getPattern <$> projected)
    {-# INLINE embed #-}

    ana coalg = Pattern . ana0
      where
        ana0 =
            Recursive.ana (Compose . Identity . coalg)
    {-# INLINE ana #-}

    apo coalg = Pattern . apo0
      where
        apo0 =
            Recursive.apo
                ( \a ->
                    (Compose . Identity)
                        (Bifunctor.first getPattern <$> coalg a)
                )
    {-# INLINE apo #-}

    postpro post coalg = Pattern . postpro0
      where
        postpro0 =
            Recursive.postpro
                (\(Compose (Identity base)) -> (Compose . Identity) (post base))
                (Compose . Identity . coalg)
    {-# INLINE postpro #-}

    gpostpro dist post coalg = Pattern . gpostpro0
      where
        gpostpro0 =
            Recursive.gpostpro
                (Compose . Identity . dist . (<$>) (runIdentity . getCompose))
                (\(Compose (Identity base)) -> (Compose . Identity) (post base))
                (Compose . Identity . coalg)
    {-# INLINE gpostpro #-}

-- This instance implements all class functions for the Pattern newtype
-- because the their implementations for the inner type may be specialized.
instance Comonad (Pattern variable) where
    extract = \(Pattern fixed) -> extract fixed
    {-# INLINE extract #-}
    duplicate = \(Pattern fixed) -> Pattern (extend Pattern fixed)
    {-# INLINE duplicate #-}
    extend extending = \(Pattern fixed) ->
        Pattern (extend (extending . Pattern) fixed)
    {-# INLINE extend #-}

instance ComonadCofree (PatternF variable) (Pattern variable) where
    unwrap = \(Pattern fixed) -> Pattern <$> unwrap fixed
    {-# INLINE unwrap #-}

instance TopBottom (Pattern variable annotation) where
    isTop (Recursive.project -> _ :< TopF _) = True
    isTop _ = False
    isBottom (Recursive.project -> _ :< BottomF _) = True
    isBottom _ = False

instance Unparse variable => SQL.Column (Pattern variable annotation) where
    defineColumn tableName _ = SQL.defineColumn tableName (SQL.Proxy @Text)
    toColumn = SQL.toColumn . Pretty.renderText . Pretty.layoutOneLine . unparse

instance
    From
        (And Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Application SymbolOrAlias (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Bottom Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Ceil Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (DomainValue Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Equals Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Exists Sort variable (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Floor Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Forall Sort variable (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Iff Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Implies Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (In Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Mu variable (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Next Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Not Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Nu variable (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Or Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Rewrites Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Top Sort (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance
    From
        (Inhabitant (Pattern variable Attribute.Null))
        (Pattern variable Attribute.Null)
    where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance From StringLiteral (Pattern variable Attribute.Null) where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

instance From (SomeVariable variable) (Pattern variable Attribute.Null) where
    from = Recursive.embed . (:<) Attribute.Null . from
    {-# INLINE CONLIKE from #-}

fromPattern ::
    Pattern variable annotation ->
    Base
        (Pattern variable annotation)
        (Pattern variable annotation)
fromPattern = Recursive.project

asPattern ::
    Base
        (Pattern variable annotation)
        (Pattern variable annotation) ->
    Pattern variable annotation
asPattern = Recursive.embed

-- | Erase the annotations from any 'Pattern'.
eraseAnnotations ::
    Pattern variable erased ->
    Pattern variable Attribute.Null
eraseAnnotations = (<$) Attribute.Null

{- | Use the provided traversal to replace all variables in a 'Pattern'.

'traverseVariables' is strict, i.e. its argument is fully evaluated before it
returns. When composing multiple transformations with 'traverseVariables', the
intermediate trees will be fully allocated; 'mapVariables' is more composable in
this respect.

See also: 'mapVariables'
-}
traverseVariables ::
    forall m variable1 variable2 annotation.
    Monad m =>
    AdjSomeVariableName (variable1 -> m variable2) ->
    Pattern variable1 annotation ->
    m (Pattern variable2 annotation)
traverseVariables adj =
    Recursive.fold traverseVariablesWorker
  where
    traverseF = PatternF.traverseVariables adj

    traverseVariablesWorker ::
        Base
            (Pattern variable1 annotation)
            (m (Pattern variable2 annotation)) ->
        m (Pattern variable2 annotation)
    traverseVariablesWorker (a :< pat) =
        reannotate <$> (traverseF =<< sequence pat)
      where
        reannotate pat' = Recursive.embed (a :< pat')

{- | Use the provided mapping to replace all variables in a 'Pattern'.

'mapVariables' is lazy: it descends into its argument only as the result is
demanded. Intermediate allocation from composing multiple transformations with
'mapVariables' is amortized; the intermediate trees are never fully resident.

See also: 'traverseVariables'
-}
mapVariables ::
    AdjSomeVariableName (variable1 -> variable2) ->
    Pattern variable1 annotation ->
    Pattern variable2 annotation
mapVariables adj =
    Recursive.ana (mapVariablesWorker . Recursive.project)
  where
    mapF = PatternF.mapVariables adj
    mapVariablesWorker (a :< pat) = a :< mapF pat

{- | Construct a @ConcretePattern@ from a 'Pattern'.

A concrete pattern contains no variables, so 'asConcretePattern' is
fully polymorphic on the variable type in the pure pattern. If the argument
contains any variables, the result is 'Nothing'.

'asConcretePattern' is strict, i.e. it traverses its argument entirely,
because the entire tree must be traversed to inspect for variables before
deciding if the result is 'Nothing' or @'Just' _@.
-}
asConcretePattern ::
    Pattern variable annotation ->
    Maybe (Pattern Concrete annotation)
asConcretePattern = traverseVariables (pure toConcrete)

-- | Check whether a pattern is concrete.
isConcrete :: forall variable annotation. Pattern variable annotation -> Bool
isConcrete = Recursive.fold worker
  where
    worker :: CofreeF (PatternF variable) annotation Bool -> Bool
    worker (_ann :< t) = isConcreteF t && and t

isConcreteF :: PatternF variable child -> Bool
isConcreteF = getAll . getConst . PatternF.traverseVariables (pure (\_var -> Const (All False)))

{- | Construct a 'Pattern' from a @ConcretePattern@.

The concrete pattern contains no variables, so the result is fully
polymorphic in the variable type.

'fromConcretePattern' uses 'unsafeCoerce' to avoid traversing the pattern.
-}
fromConcretePattern ::
    Pattern Concrete annotation ->
    Pattern variable annotation
fromConcretePattern = unsafeCoerce
