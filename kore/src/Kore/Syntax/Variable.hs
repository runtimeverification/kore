{-# LANGUAGE EmptyDataDeriving #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause

Please refer to
<http://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md kore-syntax.md>.
-}
module Kore.Syntax.Variable (
    illegalVariableCounter,
    externalizeFreshVariableName,
    Variable (..),
    SomeVariable,
    mkSomeVariable,
    foldSomeVariable,
    mapSomeVariable,
    traverseSomeVariable,
    retractElementVariable,
    isElementVariable,
    expectElementVariable,
    retractSetVariable,
    isSetVariable,
    expectSetVariable,
    ElementVariable,
    mkElementVariable,
    mapElementVariable,
    traverseElementVariable,
    SetVariable,
    mkSetVariable,
    mapSetVariable,
    traverseSetVariable,

    -- * Variable names
    VariableName (..),
    mkVariableName,
    VariableCounter,
    ElementVariableName (..),
    SetVariableName (..),
    SomeVariableName (..),
    AdjSomeVariableName (..),
    foldSomeVariableName,
    mapSomeVariableName,
    mapElementVariableName,
    mapSetVariableName,
    traverseSomeVariableName,
    traverseElementVariableName,
    traverseSetVariableName,
    toVariableName,
    fromVariableName,

    -- * Variable sorts
    unparse2SortedVariable,

    -- * Concrete
    Concrete,
    toConcrete,
) where

import Data.Binary (Binary)
import Data.Distributive (
    Distributive (..),
 )
import Data.Functor.Adjunction (
    Adjunction (..),
    extractL,
    indexAdjunction,
    tabulateAdjunction,
 )
import Data.Functor.Const (
    Const (..),
 )
import Data.Functor.Rep (
    Representable (..),
 )
import Data.Generics.Sum (
    _Ctor,
 )
import Data.Sup
import Data.Text qualified as Text
import Data.Void
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Unparser
import Numeric.Natural
import Prelude.Kore
import Pretty qualified

-- | Error thrown when 'variableCounter' takes an illegal value.
illegalVariableCounter :: a
illegalVariableCounter =
    error "Illegal use of Variable { variableCounter = Just Sup }"

{- | Reset 'counter' so that a 'VariableName' may be unparsed.

'externalizeFreshVariableName' is not injective and is unsafe if used with
'mapVariables'. See 'Kore.Internal.Pattern.externalizeFreshVariables' instead.
-}
externalizeFreshVariableName :: VariableName -> VariableName
externalizeFreshVariableName VariableName{base, counter} =
    VariableName{base = base', counter = Nothing}
  where
    base' =
        base
            { getId =
                case counter of
                    Nothing -> getId base
                    Just (Element n) -> getId base <> Text.pack (show n)
                    Just Sup -> illegalVariableCounter
            , idLocation = AstLocationGeneratedVariable
            }

fromVariableName ::
    forall variable. From VariableName variable => VariableName -> variable
fromVariableName = from @VariableName @variable

toVariableName ::
    forall variable. From variable VariableName => variable -> VariableName
toVariableName = from @variable @VariableName

{- | Unparse a 'Variable' in an Applicative Kore binder.

Variables occur without their sorts as subterms in Applicative Kore patterns,
but with their sorts in binders like @\\exists@ and
@\\forall@. 'unparse2SortedVariable' adds the sort ascription to the unparsed
variable for the latter case.
-}
unparse2SortedVariable ::
    Unparse variable =>
    Variable variable ->
    Pretty.Doc ann
unparse2SortedVariable Variable{variableName, variableSort} =
    unparse2 variableName <> Pretty.colon <> unparse variableSort

instance From VariableName Void where
    from = error "Cannot construct a variable in a concrete term!"
    {-# INLINE from #-}

-- * Variable names

type VariableCounter = Maybe (Sup Natural)

data VariableName = VariableName
    { base :: !Id
    , counter :: !VariableCounter
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

mkVariableName :: Id -> VariableName
mkVariableName base = VariableName{base, counter = mempty}

instance Unparse VariableName where
    unparse VariableName{base, counter} =
        unparse base <> Pretty.pretty counter

    unparse2 VariableName{base, counter} =
        unparse base <> Pretty.pretty counter

instance From VariableName VariableName where
    from = id
    {-# INLINE from #-}

-- * Element variables

newtype ElementVariableName variable = ElementVariableName {unElementVariableName :: variable}
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Semigroup a => Semigroup (ElementVariableName a) where
    (<>) a b = ElementVariableName (on (<>) unElementVariableName a b)
    {-# INLINE (<>) #-}

instance Monoid a => Monoid (ElementVariableName a) where
    mempty = ElementVariableName mempty
    {-# INLINE mempty #-}

instance Applicative ElementVariableName where
    pure = ElementVariableName
    {-# INLINE pure #-}

    (<*>) (ElementVariableName f) (ElementVariableName a) =
        ElementVariableName (f a)
    {-# INLINE (<*>) #-}

instance Distributive ElementVariableName where
    distribute = ElementVariableName . fmap unElementVariableName
    {-# INLINE distribute #-}

instance Unparse variable => Unparse (ElementVariableName variable) where
    unparse = unparseGeneric
    {-# INLINE unparse #-}

    unparse2 = unparse2Generic
    {-# INLINE unparse2 #-}

-- * Set variables

instance
    From variable VariableName =>
    From (ElementVariableName variable) VariableName
    where
    from = from . unElementVariableName

newtype SetVariableName variable = SetVariableName {unSetVariableName :: variable}
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic, GHC.Generic1)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Semigroup a => Semigroup (SetVariableName a) where
    (<>) a b = SetVariableName (on (<>) unSetVariableName a b)

instance Monoid a => Monoid (SetVariableName a) where
    mempty = SetVariableName mempty
    {-# INLINE mempty #-}

instance Applicative SetVariableName where
    pure = SetVariableName
    {-# INLINE pure #-}

    (<*>) (SetVariableName f) (SetVariableName a) =
        SetVariableName (f a)
    {-# INLINE (<*>) #-}

instance Distributive SetVariableName where
    distribute = SetVariableName . fmap unSetVariableName
    {-# INLINE distribute #-}

instance Unparse variable => Unparse (SetVariableName variable) where
    unparse = unparseGeneric
    {-# INLINE unparse #-}

    unparse2 = unparse2Generic
    {-# INLINE unparse2 #-}

instance From variable VariableName => From (SetVariableName variable) VariableName where
    from = from . unSetVariableName

-- * Variable occurrences

{- | 'Variable' is an occurrence of a variable in a Kore pattern.

The @variable@ parameter is the type of variable names.

Every occurrence of a variable carries the 'Sort' of the variable.
-}
data Variable variable = Variable
    -- Note: we sometimes use mapKeysMonotonic with variableName, so
    -- any changes to this representation may require modifications to
    -- those use sites.
    { variableName :: !variable
    , variableSort :: !Sort
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse variable => Unparse (Variable variable) where
    unparse Variable{variableName, variableSort} =
        unparse variableName
            <> Pretty.colon
            <> unparse variableSort

    unparse2 Variable{variableName} = unparse2 variableName

instance Injection into from => Injection (Variable into) (Variable from) where
    inject = fmap inject
    {-# INLINE inject #-}

    retract = traverse retract
    {-# INLINE retract #-}

instance Synthetic Sort (Const (Variable variable)) where
    synthetic = variableSort . getConst
    {-# INLINE synthetic #-}

-- | Element (singleton) Kore variables
type ElementVariable variable = Variable (ElementVariableName variable)

mkElementVariable :: Id -> Sort -> ElementVariable VariableName
mkElementVariable base variableSort =
    Variable
        { variableName = ElementVariableName (mkVariableName base)
        , variableSort
        }

mapElementVariable ::
    AdjSomeVariableName (variable1 -> variable2) ->
    ElementVariable variable1 ->
    ElementVariable variable2
mapElementVariable adj = fmap (mapElementVariableName adj)

traverseElementVariable ::
    Applicative f =>
    AdjSomeVariableName (variable1 -> f variable2) ->
    ElementVariable variable1 ->
    f (ElementVariable variable2)
traverseElementVariable adj = traverse (traverseElementVariableName adj)

-- | Kore set variables
type SetVariable variable = Variable (SetVariableName variable)

mkSetVariable :: Id -> Sort -> SetVariable VariableName
mkSetVariable base variableSort =
    Variable
        { variableName = SetVariableName (mkVariableName base)
        , variableSort
        }

mapSetVariable ::
    AdjSomeVariableName (variable1 -> variable2) ->
    SetVariable variable1 ->
    SetVariable variable2
mapSetVariable adj = fmap (mapSetVariableName adj)

traverseSetVariable ::
    Applicative f =>
    AdjSomeVariableName (variable1 -> f variable2) ->
    SetVariable variable1 ->
    f (SetVariable variable2)
traverseSetVariable adj = traverse (traverseSetVariableName adj)

{- | 'SomeVariableName' is the name of a variable in a pattern.

'SomeVariableName' may be an 'ElementVariableName' or a 'SetVariableName'.
-}
data SomeVariableName variable
    = SomeVariableNameElement !(ElementVariableName variable)
    | SomeVariableNameSet !(SetVariableName variable)
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse variable => Unparse (SomeVariableName variable) where
    unparse = unparseGeneric
    {-# INLINE unparse #-}

    unparse2 = unparse2Generic
    {-# INLINE unparse2 #-}

instance Injection (SomeVariableName variable) (ElementVariableName variable) where
    injection = _Ctor @"SomeVariableNameElement"
    {-# INLINE injection #-}

instance Injection (SomeVariableName variable) (SetVariableName variable) where
    injection = _Ctor @"SomeVariableNameSet"
    {-# INLINE injection #-}

instance From variable VariableName => From (SomeVariableName variable) VariableName where
    from = extractL . fmap from
    {-# INLINE from #-}

instance
    From variable1 variable2 =>
    From (SomeVariableName variable1) (SomeVariableName variable2)
    where
    from = fmap from
    {-# INLINE from #-}

{- | 'AdjSomeVariableName' is the right adjoint of 'SomeVariableName'.

The fields of product type 'AdjSomeVariableName' align with the constructors of
sum type 'SomeVariableName'.

In practice, 'AdjSomeVariableName' are used to represent mappings that
transform 'ElementVariableName' and 'SetVariableName' separately while
preserving each kind of variable. For example, the type
@
    'AdjSomeVariableName' (a -> b)
@
is a restriction of the type
@
    'SomeVariableName' a -> 'SomeVariableName' b
@
where the former ensures that (element, set) variables are mapped to (element,
set) variables respectively.

'AdjSomeVariableName' may be constructed and composed through its 'Applicative'
instance. @'pure' x@ constructs an 'AdjSomeVariableName' with the same value @x@
in both fields. @f '<*>' a@ composes two 'AdjSomeVariableName' by applying the
function in each field of @f@ to the value in the corresponding field of @a@.
-}
data AdjSomeVariableName a = AdjSomeVariableName
    { adjSomeVariableNameElement :: !(ElementVariableName a)
    -- ^ compare to: 'SomeVariableNameElement'
    , adjSomeVariableNameSet :: !(SetVariableName a)
    -- ^ compare to: 'SomeVariableNameSet'
    }
    deriving stock (Functor)
    deriving stock (GHC.Generic1)

instance Semigroup a => Semigroup (AdjSomeVariableName a) where
    (<>) a b =
        AdjSomeVariableName
            { adjSomeVariableNameElement = on (<>) adjSomeVariableNameElement a b
            , adjSomeVariableNameSet = on (<>) adjSomeVariableNameSet a b
            }
    {-# INLINE (<>) #-}

instance Monoid a => Monoid (AdjSomeVariableName a) where
    mempty =
        AdjSomeVariableName
            { adjSomeVariableNameElement = mempty
            , adjSomeVariableNameSet = mempty
            }
    {-# INLINE mempty #-}

instance Applicative AdjSomeVariableName where
    pure a = AdjSomeVariableName (ElementVariableName a) (SetVariableName a)
    {-# INLINE pure #-}

    (<*>) fs as =
        AdjSomeVariableName
            { adjSomeVariableNameElement =
                adjSomeVariableNameElement fs <*> adjSomeVariableNameElement as
            , adjSomeVariableNameSet =
                adjSomeVariableNameSet fs <*> adjSomeVariableNameSet as
            }
    {-# INLINE (<*>) #-}

instance Distributive AdjSomeVariableName where
    distribute f =
        AdjSomeVariableName
            { adjSomeVariableNameElement = collect adjSomeVariableNameElement f
            , adjSomeVariableNameSet = collect adjSomeVariableNameSet f
            }
    {-# INLINE distribute #-}

instance Representable AdjSomeVariableName where
    type Rep AdjSomeVariableName = SomeVariableName ()
    tabulate = tabulateAdjunction
    index = indexAdjunction

instance Adjunction SomeVariableName AdjSomeVariableName where
    unit a =
        AdjSomeVariableName
            (pure . SomeVariableNameElement . pure $ a)
            (pure . SomeVariableNameSet . pure $ a)
    {-# INLINE unit #-}

    counit (SomeVariableNameElement adj) =
        unElementVariableName
            . adjSomeVariableNameElement
            . unElementVariableName
            $ adj
    counit (SomeVariableNameSet adj) =
        unSetVariableName
            . adjSomeVariableNameSet
            . unSetVariableName
            $ adj
    {-# INLINE counit #-}

foldSomeVariableName ::
    AdjSomeVariableName (variable1 -> r) ->
    SomeVariableName variable1 ->
    r
foldSomeVariableName adj =
    rightAdjunct (\variable1 -> ($ variable1) <$> adj)

mapSomeVariableName ::
    AdjSomeVariableName (variable1 -> variable2) ->
    SomeVariableName variable1 ->
    SomeVariableName variable2
mapSomeVariableName adj variable1 =
    fmap (index adj idx) variable1
  where
    idx = void variable1

mapElementVariableName ::
    AdjSomeVariableName (variable1 -> variable2) ->
    ElementVariableName variable1 ->
    ElementVariableName variable2
mapElementVariableName adj =
    (<*>) (adjSomeVariableNameElement adj)

mapSetVariableName ::
    AdjSomeVariableName (variable1 -> variable2) ->
    SetVariableName variable1 ->
    SetVariableName variable2
mapSetVariableName adj =
    (<*>) (adjSomeVariableNameSet adj)

traverseSomeVariableName ::
    Applicative f =>
    AdjSomeVariableName (variable1 -> f variable2) ->
    SomeVariableName variable1 ->
    f (SomeVariableName variable2)
traverseSomeVariableName adj variable1 =
    traverse (index adj idx) variable1
  where
    idx = void variable1

traverseElementVariableName ::
    forall variable1 variable2 f.
    Applicative f =>
    AdjSomeVariableName (variable1 -> f variable2) ->
    ElementVariableName variable1 ->
    f (ElementVariableName variable2)
traverseElementVariableName adj =
    sequenceA . (<*>) (adjSomeVariableNameElement adj)

traverseSetVariableName ::
    Applicative f =>
    AdjSomeVariableName (variable1 -> f variable2) ->
    SetVariableName variable1 ->
    f (SetVariableName variable2)
traverseSetVariableName adj =
    sequenceA . (<*>) (adjSomeVariableNameSet adj)

type SomeVariable variable = Variable (SomeVariableName variable)

mkSomeVariable ::
    forall variable f.
    Injection (SomeVariable variable) (Variable (f variable)) =>
    Variable (f variable) ->
    SomeVariable variable
mkSomeVariable = inject

foldSomeVariable ::
    AdjSomeVariableName (variable1 -> r) ->
    SomeVariable variable1 ->
    r
foldSomeVariable adj = foldSomeVariableName adj . variableName

mapSomeVariable ::
    AdjSomeVariableName (variable1 -> variable2) ->
    SomeVariable variable1 ->
    SomeVariable variable2
mapSomeVariable adj = fmap (mapSomeVariableName adj)

traverseSomeVariable ::
    Applicative f =>
    AdjSomeVariableName (variable1 -> f variable2) ->
    SomeVariable variable1 ->
    f (SomeVariable variable2)
traverseSomeVariable adj = traverse (traverseSomeVariableName adj)

retractElementVariable ::
    SomeVariable variable ->
    Maybe (ElementVariable variable)
retractElementVariable = retract

isElementVariable :: SomeVariable variable -> Bool
isElementVariable = isJust . retractElementVariable

{- | Extract an 'ElementVariable' from a 'SomeVariable'.

It is an error if the 'SomeVariable' is not the 'ElemVar' constructor.

Use 'expectElementVariable' when maintaining the invariant outside the type
system that the 'SomeVariable' is an 'ElementVariable', but please include a
comment at the call site describing how the invariant is maintained.
-}
expectElementVariable ::
    HasCallStack =>
    SomeVariable variable ->
    ElementVariable variable
expectElementVariable unifiedVariable =
    retractElementVariable unifiedVariable
        & fromMaybe (error "Expected element variable")

retractSetVariable ::
    SomeVariable variable ->
    Maybe (SetVariable variable)
retractSetVariable = retract

isSetVariable :: forall variable. SomeVariable variable -> Bool
isSetVariable unifiedVariable
    | Just _ <- retract @_ @(SetVariable variable) unifiedVariable = True
    | otherwise = False

{- | Extract an 'SetVariable' from a 'SomeVariable'.

It is an error if the 'SomeVariable' is not the 'SetVar' constructor.

Use 'expectSetVariable' when maintaining the invariant outside the type system
that the 'SomeVariable' is an 'SetVariable', but please include a comment at the
call site describing how the invariant is maintained.
-}
expectSetVariable ::
    HasCallStack =>
    SomeVariable variable ->
    SetVariable variable
expectSetVariable unifiedVariable =
    retractSetVariable unifiedVariable
        & fromMaybe (error "Expected set variable")

-- | 'Concrete' patterns contain no variables.
type Concrete = Void

toConcrete :: any -> Maybe Void
toConcrete = const Nothing
