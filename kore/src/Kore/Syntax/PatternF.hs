{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.PatternF (
    PatternF (..),
    mapVariables,
    traverseVariables,

    -- * Pure pattern heads
    groundHead,
    constant,

    -- * Re-exports
    Const (..),
) where

import Control.Lens qualified as Lens
import Data.Binary (Binary)
import Data.Functor.Const
import Data.Functor.Identity (
    Identity (..),
 )
import Data.Generics.Wrapped (
    _Unwrapped,
 )
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
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
import Kore.Syntax.Rewrites
import Kore.Syntax.StringLiteral
import Kore.Syntax.Top
import Kore.Syntax.Variable
import Kore.Unparser
import Prelude.Kore

-- | 'PatternF' is the 'Base' functor of Kore patterns
data PatternF variable child
    = AndF !(And Sort child)
    | ApplicationF !(Application SymbolOrAlias child)
    | BottomF !(Bottom Sort child)
    | CeilF !(Ceil Sort child)
    | DomainValueF !(DomainValue Sort child)
    | EqualsF !(Equals Sort child)
    | ExistsF !(Exists Sort variable child)
    | FloorF !(Floor Sort child)
    | ForallF !(Forall Sort variable child)
    | IffF !(Iff Sort child)
    | ImpliesF !(Implies Sort child)
    | InF !(In Sort child)
    | MuF !(Mu variable child)
    | NextF !(Next Sort child)
    | NotF !(Not Sort child)
    | NuF !(Nu variable child)
    | OrF !(Or Sort child)
    | RewritesF !(Rewrites Sort child)
    | TopF !(Top Sort child)
    | InhabitantF !(Inhabitant child)
    | StringLiteralF !(Const StringLiteral child)
    | VariableF !(Const (SomeVariable variable) child)
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Binary a => Binary (Const a b)

instance (Unparse variable, Unparse child) => Unparse (PatternF variable child) where
    unparse = unparseGeneric
    unparse2 = unparse2Generic

instance From (And Sort child) (PatternF variable child) where
    from = AndF
    {-# INLINE CONLIKE from #-}

instance From (Application SymbolOrAlias child) (PatternF variable child) where
    from = ApplicationF
    {-# INLINE CONLIKE from #-}

instance From (Bottom Sort child) (PatternF variable child) where
    from = BottomF
    {-# INLINE CONLIKE from #-}

instance From (Ceil Sort child) (PatternF variable child) where
    from = CeilF
    {-# INLINE CONLIKE from #-}

instance From (DomainValue Sort child) (PatternF variable child) where
    from = DomainValueF
    {-# INLINE CONLIKE from #-}

instance From (Equals Sort child) (PatternF variable child) where
    from = EqualsF
    {-# INLINE CONLIKE from #-}

instance From (Exists Sort variable child) (PatternF variable child) where
    from = ExistsF
    {-# INLINE CONLIKE from #-}

instance From (Floor Sort child) (PatternF variable child) where
    from = FloorF
    {-# INLINE CONLIKE from #-}

instance From (Forall Sort variable child) (PatternF variable child) where
    from = ForallF
    {-# INLINE CONLIKE from #-}

instance From (Iff Sort child) (PatternF variable child) where
    from = IffF
    {-# INLINE CONLIKE from #-}

instance From (Implies Sort child) (PatternF variable child) where
    from = ImpliesF
    {-# INLINE CONLIKE from #-}

instance From (In Sort child) (PatternF variable child) where
    from = InF
    {-# INLINE CONLIKE from #-}

instance From (Mu variable child) (PatternF variable child) where
    from = MuF
    {-# INLINE CONLIKE from #-}

instance From (Next Sort child) (PatternF variable child) where
    from = NextF
    {-# INLINE CONLIKE from #-}

instance From (Not Sort child) (PatternF variable child) where
    from = NotF
    {-# INLINE CONLIKE from #-}

instance From (Nu variable child) (PatternF variable child) where
    from = NuF
    {-# INLINE CONLIKE from #-}

instance From (Or Sort child) (PatternF variable child) where
    from = OrF
    {-# INLINE CONLIKE from #-}

instance From (Rewrites Sort child) (PatternF variable child) where
    from = RewritesF
    {-# INLINE CONLIKE from #-}

instance From (Top Sort child) (PatternF variable child) where
    from = TopF
    {-# INLINE CONLIKE from #-}

instance From (Inhabitant child) (PatternF variable child) where
    from = InhabitantF
    {-# INLINE CONLIKE from #-}

instance From StringLiteral (PatternF variable child) where
    from = StringLiteralF . Const
    {-# INLINE CONLIKE from #-}

instance From (SomeVariable variable) (PatternF variable child) where
    from = VariableF . Const
    {-# INLINE CONLIKE from #-}

{- | Use the provided mapping to replace all variables in a 'PatternF' head.

__Warning__: 'mapVariables' will capture variables if the provided mapping is
not injective!
-}
mapVariables ::
    AdjSomeVariableName (variable1 -> variable2) ->
    PatternF variable1 child ->
    PatternF variable2 child
mapVariables adj =
    runIdentity . traverseVariables adj'
  where
    adj' = (.) pure <$> adj
{-# INLINE mapVariables #-}

{- | Use the provided traversal to replace all variables in a 'PatternF' head.

__Warning__: 'traverseVariables' will capture variables if the provided
traversal is not injective!
-}
traverseVariables ::
    Applicative f =>
    AdjSomeVariableName (variable1 -> f variable2) ->
    PatternF variable1 child ->
    f (PatternF variable2 child)
traverseVariables adj =
    \case
        -- Non-trivial cases
        ExistsF any0 -> ExistsF <$> traverseVariablesExists any0
        ForallF all0 -> ForallF <$> traverseVariablesForall all0
        MuF any0 -> MuF <$> traverseVariablesMu any0
        NuF any0 -> NuF <$> traverseVariablesNu any0
        VariableF variableF -> traverseVariable variableF
        -- Trivial cases
        AndF andP -> pure (AndF andP)
        ApplicationF appP -> pure (ApplicationF appP)
        BottomF botP -> pure (BottomF botP)
        CeilF ceilP -> pure (CeilF ceilP)
        DomainValueF dvP -> pure (DomainValueF dvP)
        EqualsF eqP -> pure (EqualsF eqP)
        FloorF flrP -> pure (FloorF flrP)
        IffF iffP -> pure (IffF iffP)
        ImpliesF impP -> pure (ImpliesF impP)
        InF inP -> pure (InF inP)
        NextF nxtP -> pure (NextF nxtP)
        NotF notP -> pure (NotF notP)
        OrF orP -> pure (OrF orP)
        RewritesF rewP -> pure (RewritesF rewP)
        StringLiteralF strP -> pure (StringLiteralF strP)
        TopF topP -> pure (TopF topP)
        InhabitantF s -> pure (InhabitantF s)
  where
    trElemVar = traverse $ traverseElementVariableName adj
    trSetVar = traverse $ traverseSetVariableName adj
    traverseVariable =
        fmap VariableF
            . Lens.traverseOf _Unwrapped (traverseSomeVariable adj)
    traverseVariablesExists Exists{existsSort, existsVariable, existsChild} =
        Exists existsSort
            <$> trElemVar existsVariable
            <*> pure existsChild
    traverseVariablesForall Forall{forallSort, forallVariable, forallChild} =
        Forall forallSort
            <$> trElemVar forallVariable
            <*> pure forallChild
    traverseVariablesMu Mu{muVariable, muChild} =
        Mu <$> trSetVar muVariable <*> pure muChild
    traverseVariablesNu Nu{nuVariable, nuChild} =
        Nu <$> trSetVar nuVariable <*> pure nuChild

{- | Given an 'Id', 'groundHead' produces the head of an 'Application'
 corresponding to that argument.
-}
groundHead :: Text -> AstLocation -> SymbolOrAlias
groundHead ctor location =
    SymbolOrAlias
        { symbolOrAliasConstructor =
            Id
                { getId = ctor
                , idLocation = location
                }
        , symbolOrAliasParams = []
        }

{- | Given a head and a list of children, produces an 'ApplicationF'
  applying the given head to the children
-}
apply :: SymbolOrAlias -> [child] -> PatternF variable child
apply patternHead patterns =
    ApplicationF
        Application
            { applicationSymbolOrAlias = patternHead
            , applicationChildren = patterns
            }

{- | Applies the given head to the empty list of children to obtain a
 constant 'ApplicationF'
-}
constant :: SymbolOrAlias -> PatternF variable child
constant patternHead = apply patternHead []
