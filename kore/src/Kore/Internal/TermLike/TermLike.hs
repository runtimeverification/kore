{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2020-2021
License     : BSD-3-Clause
-}
module Kore.Internal.TermLike.TermLike (
    TermLike (..),
    TermLikeF (..),
    TermAttributes (..),
    freeVariables,
    retractKey,
    extractAttributes,
    mapVariables,
    traverseVariables,
    mkVar,
    traverseVariablesF,
    updateCallStack,
    depth,
    isAttributeSimplified,
    isAttributeSimplifiedAnyCondition,
    isAttributeSimplifiedSomeCondition,
    attributeSimplifiedAttribute,
    termLikeAttributes,
    setAttributeSimplified,
    mapAttributeVariables,
    deleteFreeVariable,
) where

import Control.Comonad.Trans.Cofree (
    CofreeT (CofreeT),
    tailF,
 )
import Control.Lens (
    Lens',
 )
import Control.Lens qualified as Lens
import Control.Monad qualified as Monad
import Control.Monad.Reader qualified as Reader
import Data.Bifunctor (first)
import Data.ByteString.Short qualified as ByteString
import Data.Functor.Const (
    Const (..),
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
import Data.Generics.Product
import Data.Generics.Product qualified as Lens.Product
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Void (absurd)
import GHC.Generics qualified as GHC
import GHC.Stack qualified as GHC
import Generics.SOP qualified as SOP
import Kore.AST.AstWithLocation
import Kore.Attribute.Pattern.ConstructorLike qualified as Attribute
import Kore.Attribute.Pattern.Created qualified as Attribute
import Kore.Attribute.Pattern.Defined qualified as Attribute
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables (..),
 )
import Kore.Attribute.Pattern.FreeVariables qualified as Attribute
import Kore.Attribute.Pattern.FreeVariables qualified as Attribute.FreeVariables
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Attribute.Pattern.Function qualified as Attribute
import Kore.Attribute.Pattern.Simplified qualified as Attribute
import Kore.Attribute.Pattern.Total qualified as Attribute
import Kore.Attribute.Synthetic
import Kore.Builtin.Encoding qualified as Encoding
import Kore.Builtin.Endianness.Endianness (
    Endianness,
 )
import Kore.Builtin.Endianness.Endianness qualified as Endianness
import Kore.Builtin.Signedness.Signedness (
    Signedness,
 )
import Kore.Builtin.Signedness.Signedness qualified as Signedness
import Kore.Debug
import Kore.Internal.Alias
import Kore.Internal.Alias qualified as Alias
import Kore.Internal.Inj
import Kore.Internal.Inj qualified as Inj
import Kore.Internal.InternalBool
import Kore.Internal.InternalBytes
import Kore.Internal.InternalInt
import Kore.Internal.InternalList
import Kore.Internal.InternalMap
import Kore.Internal.InternalSet
import Kore.Internal.InternalString
import Kore.Internal.Key (
    Key,
    KeyAttributes (KeyAttributes),
    KeyF,
 )
import Kore.Internal.Key qualified as Attribute
import Kore.Internal.Key qualified as Key
import Kore.Internal.SideCondition.SideCondition qualified as SideCondition (
    Representation,
 )
import Kore.Internal.Symbol (
    Symbol,
 )
import Kore.Internal.Symbol qualified as Symbol
import Kore.Internal.TermLike.Renaming
import Kore.Internal.Variable
import Kore.Sort
import Kore.Substitute
import Kore.Syntax.And
import Kore.Syntax.Application
import Kore.Syntax.BinaryAnd
import Kore.Syntax.BinaryOr
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
import Kore.Syntax.Pattern qualified as Pattern
import Kore.Syntax.PatternF qualified as PatternF
import Kore.Syntax.Rewrites
import Kore.Syntax.StringLiteral
import Kore.Syntax.Top
import Kore.TopBottom
import Kore.Unparser (
    Unparse (..),
 )
import Kore.Unparser qualified as Unparser
import Kore.Variables.Binding
import Kore.Variables.Fresh (refreshVariable)
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified
import SQL qualified

-- | 'TermLikeF' is the 'Base' functor of internal term-like patterns.
data TermLikeF variable child
    = AndF !(BinaryAnd Sort child)
    | ApplySymbolF !(Application Symbol child)
    | -- TODO (thomas.tuegel): Expand aliases during validation?
      ApplyAliasF !(Application (Alias (TermLike VariableName)) child)
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
    | OrF !(BinaryOr Sort child)
    | RewritesF !(Rewrites Sort child)
    | TopF !(Top Sort child)
    | InhabitantF !(Inhabitant child)
    | StringLiteralF !(Const StringLiteral child)
    | InternalBoolF !(Const InternalBool child)
    | InternalBytesF !(Const InternalBytes child)
    | InternalIntF !(Const InternalInt child)
    | InternalStringF !(Const InternalString child)
    | InternalListF !(InternalList child)
    | InternalMapF !(InternalMap Key child)
    | InternalSetF !(InternalSet Key child)
    | VariableF !(Const (SomeVariable variable) child)
    | EndiannessF !(Const Endianness child)
    | SignednessF !(Const Signedness child)
    | InjF !(Inj child)
    deriving stock (Eq, Ord, Show)
    deriving stock (Foldable, Functor, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance (Unparse variable, Unparse child) => Unparse (TermLikeF variable child) where
    unparse = Unparser.unparseGeneric
    unparse2 = Unparser.unparse2Generic

instance
    Ord variable =>
    Synthetic (Attribute.FreeVariables variable) (TermLikeF variable)
    where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBoolF internalBool -> synthetic internalBool
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            InternalStringF internalString -> synthetic internalString
            InternalListF internalList -> synthetic internalList
            InternalMapF internalMap -> synthetic internalMap
            InternalSetF internalSet -> synthetic internalSet
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj

instance Synthetic Sort (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBoolF internalBool -> synthetic internalBool
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            InternalStringF internalString -> synthetic internalString
            InternalListF internalList -> synthetic internalList
            InternalMapF internalMap -> synthetic internalMap
            InternalSetF internalSet -> synthetic internalSet
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj

instance Synthetic Attribute.Total (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBoolF internalBool -> synthetic internalBool
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            InternalStringF internalString -> synthetic internalString
            InternalListF internalList -> synthetic internalList
            InternalMapF internalMap -> synthetic internalMap
            InternalSetF internalSet -> synthetic internalSet
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj

instance Synthetic Attribute.Function (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBoolF internalBool -> synthetic internalBool
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            InternalStringF internalString -> synthetic internalString
            InternalListF internalList -> synthetic internalList
            InternalMapF internalMap -> synthetic internalMap
            InternalSetF internalSet -> synthetic internalSet
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj

instance Synthetic Attribute.Defined (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBoolF internalBool -> synthetic internalBool
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            InternalStringF internalString -> synthetic internalString
            InternalListF internalList -> synthetic internalList
            InternalMapF internalMap -> synthetic internalMap
            InternalSetF internalSet -> synthetic internalSet
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj

instance Synthetic Attribute.Simplified (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBoolF internalBool -> synthetic internalBool
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            InternalStringF internalString -> synthetic internalString
            InternalListF internalList -> synthetic internalList
            InternalMapF internalMap -> synthetic internalMap
            InternalSetF internalSet -> synthetic internalSet
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj

instance Synthetic Attribute.ConstructorLike (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBoolF internalBool -> synthetic internalBool
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            InternalStringF internalString -> synthetic internalString
            InternalListF internalList -> synthetic internalList
            InternalMapF internalMap -> synthetic internalMap
            InternalSetF internalSet -> synthetic internalSet
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj

instance From (KeyF child) (TermLikeF variable child) where
    from (Key.ApplySymbolF app) = ApplySymbolF app
    from (Key.InjF inj) = InjF inj
    from (Key.DomainValueF domainValue) = DomainValueF domainValue
    from (Key.InternalBoolF internalBool) = InternalBoolF internalBool
    from (Key.InternalIntF internalInt) = InternalIntF internalInt
    from (Key.InternalStringF internalString) = InternalStringF internalString
    from (Key.InternalSetF internalSet) = InternalSetF internalSet
    from (Key.InternalMapF internalMap) = InternalMapF internalMap
    from (Key.InternalListF internalList) = InternalListF internalList
    from (Key.InternalBytesF internalBytes) = InternalBytesF internalBytes
    from (Key.StringLiteralF stringLiteral) = StringLiteralF stringLiteral
    {-# INLINE from #-}

-- | @TermAttributes@ are the attributes of a pattern collected during verification.
data TermAttributes variable = TermAttributes
    { termSort :: !Sort
    , termFreeVariables :: !(Attribute.FreeVariables variable)
    , termTotal :: !Attribute.Total
    , termFunction :: !Attribute.Function
    , termDefined :: !Attribute.Defined
    , termCreated :: !Attribute.Created
    , termSimplified :: !Attribute.Simplified
    , termConstructorLike :: !Attribute.ConstructorLike
    }
    deriving stock (Eq, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Debug variable => Debug (TermAttributes variable) where
    debugPrecBrief _ _ = "_"

instance (Debug variable, Diff variable) => Diff (TermAttributes variable)

instance
    ( Synthetic Sort base
    , Synthetic (Attribute.FreeVariables variable) base
    , Synthetic Attribute.Total base
    , Synthetic Attribute.Function base
    , Synthetic Attribute.Defined base
    , Synthetic Attribute.Simplified base
    , Synthetic Attribute.ConstructorLike base
    ) =>
    Synthetic (TermAttributes variable) base
    where
    synthetic base =
        TermAttributes
            { termSort = synthetic (termSort <$> base)
            , termFreeVariables = synthetic (termFreeVariables <$> base)
            , termTotal = synthetic (termTotal <$> base)
            , termFunction = synthetic (termFunction <$> base)
            , termDefined = synthetic (termDefined <$> base)
            , termCreated = synthetic (termCreated <$> base)
            , termSimplified =
                if Attribute.isConstructorLike constructorLikeAttr
                    then Attribute.fullySimplified
                    else synthetic (termSimplified <$> base)
            , termConstructorLike = constructorLikeAttr
            }
      where
        constructorLikeAttr :: Attribute.ConstructorLike
        constructorLikeAttr = synthetic (termConstructorLike <$> base)

instance Attribute.HasConstructorLike (TermAttributes variable) where
    extractConstructorLike
        TermAttributes{termConstructorLike} =
            termConstructorLike

instance Ord variable => From KeyAttributes (TermAttributes variable) where
    from = fromKeyAttributes

attributeSimplifiedAttribute ::
    HasCallStack =>
    TermAttributes variable ->
    Attribute.Simplified
attributeSimplifiedAttribute patt@TermAttributes{termSimplified} =
    assertSimplifiedConsistency patt termSimplified

{- Checks whether the pattern is simplified relative to the given side
condition.
-}
isAttributeSimplified ::
    HasCallStack =>
    SideCondition.Representation ->
    TermAttributes variable ->
    Bool
isAttributeSimplified sideCondition patt@TermAttributes{termSimplified} =
    assertSimplifiedConsistency patt $
        Attribute.isSimplified sideCondition termSimplified

{- Checks whether the pattern is simplified relative to some side condition.
-}
isAttributeSimplifiedSomeCondition ::
    HasCallStack =>
    TermAttributes variable ->
    Bool
isAttributeSimplifiedSomeCondition patt@TermAttributes{termSimplified} =
    assertSimplifiedConsistency patt $
        Attribute.isSimplifiedSomeCondition termSimplified

{- Checks whether the pattern is simplified relative to any side condition.
-}
isAttributeSimplifiedAnyCondition ::
    HasCallStack =>
    TermAttributes variable ->
    Bool
isAttributeSimplifiedAnyCondition patt@TermAttributes{termSimplified} =
    assertSimplifiedConsistency patt $
        Attribute.isSimplifiedAnyCondition termSimplified

assertSimplifiedConsistency :: HasCallStack => TermAttributes variable -> a -> a
assertSimplifiedConsistency
    TermAttributes{termConstructorLike, termSimplified}
        | Attribute.isConstructorLike termConstructorLike
        , not (Attribute.isSimplifiedAnyCondition termSimplified) =
            error "Inconsistent attributes, constructorLike implies fully simplified."
        | otherwise = id

setAttributeSimplified ::
    Attribute.Simplified ->
    TermAttributes variable ->
    TermAttributes variable
setAttributeSimplified termSimplified attrs =
    attrs{termSimplified}

-- TODO: should we remove this? it isn't used anywhere

{- | Use the provided mapping to replace all variables in a 'TermAttributes'.

See also: 'traverseVariables'
-}
mapAttributeVariables ::
    Ord variable2 =>
    AdjSomeVariableName (variable1 -> variable2) ->
    TermAttributes variable1 ->
    TermAttributes variable2
mapAttributeVariables adj =
    Lens.over
        (field @"termFreeVariables")
        (Attribute.mapFreeVariables adj)

{- | Use the provided traversal to replace the free variables in a 'TermAttributes'.

See also: 'mapVariables'
-}
traverseAttributeVariables ::
    forall m variable1 variable2.
    Monad m =>
    Ord variable2 =>
    AdjSomeVariableName (variable1 -> m variable2) ->
    TermAttributes variable1 ->
    m (TermAttributes variable2)
traverseAttributeVariables adj =
    field @"termFreeVariables" (Attribute.traverseFreeVariables adj)

-- TODO: should we remove this? it isn't used anywhere

-- | Delete the given variable from the set of free variables.
deleteFreeVariable ::
    Ord variable =>
    SomeVariable variable ->
    TermAttributes variable ->
    TermAttributes variable
deleteFreeVariable variable =
    Lens.over
        (field @"termFreeVariables")
        (Attribute.FreeVariables.bindVariable variable)

instance HasFreeVariables (TermAttributes variable) variable where
    freeVariables = termFreeVariables

{- | @TermLike@ is a term-like Kore pattern.

@TermLike@ is the common internal representation of patterns, especially terms.

@TermLike@ is essentially 'Control.Comonad.Cofree.Cofree', but it caches hashes.
-}
data TermLike variable = TermLike__
    -- Some fields below are lazy to better match Cofree. Which do we actually
    -- want to be lazy, if any?
    { _tlAttributes :: ~(TermAttributes variable)
    , _tlHash :: ~Int
    -- ^ A hash of @_tlTermLikeF@
    , _tlTermLikeF :: ~(TermLikeF variable (TermLike variable))
    }
    deriving stock (Show)
    -- Deriving the stock Generic risks breaking the hash cache invariant
    -- if we're not careful about how we use it. Should we write a custom
    -- instance that maintains the invariant automagically? Unfortunately,
    -- this will impose Hashable constraints on `from`, not just `to`; how
    -- bad might that be? Custom Generic instances are always a pain due
    -- to the need to patch metadata, and keeping them up to date can be
    -- a bit of a challenge.
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

instance (Debug variable, Diff variable) => Diff (TermLike variable) where
    diffPrec termLike1 termLike2 =
        -- If the patterns differ, do not display the difference in the
        -- attributes, which would overload the user with redundant information.
        diffPrecGeneric
            termLike1
            (termLike2 & termLikeAttributes Lens..~ extractAttributes termLike1)
            <|> diffPrecGeneric termLike1 termLike2

instance Eq variable => Eq (TermLike variable) where
    (==)
        (Recursive.project -> _ :< pat1)
        (Recursive.project -> _ :< pat2) =
            pat1 == pat2

instance Ord variable => Ord (TermLike variable) where
    compare
        (Recursive.project -> _ :< pat1)
        (Recursive.project -> _ :< pat2) =
            compare pat1 pat2

instance Eq variable => Hashable (TermLike variable) where
    hashWithSalt salt (TermLike__ _ hsh _) =
        salt `hashWithSalt` hsh -- HACK
    {-# INLINE hashWithSalt #-}

instance NFData variable => NFData (TermLike variable) where
    rnf (Recursive.project -> annotation :< pat) =
        rnf annotation `seq` rnf pat

instance (Unparse variable, Ord variable) => Unparse (TermLike variable) where
    unparse term =
        case Recursive.project term of
            (attrs :< termLikeF)
                | Attribute.hasKnownCreator termCreated ->
                    Pretty.sep
                        [ Pretty.pretty termCreated
                        , unparse termLikeF
                        ]
                | otherwise ->
                    unparse termLikeF
              where
                TermAttributes{termCreated} = attrs

    unparse2 term =
        case Recursive.project term of
            (_ :< pat) -> unparse2 pat

instance InternalVariable variable => Pretty (TermLike variable) where
    pretty = unparse

type instance
    Base (TermLike variable) =
        CofreeF (TermLikeF variable) (TermAttributes variable)

-- This instance implements all class functions for the TermLike newtype
-- because the their implementations for the inner type may be specialized.
instance Recursive (TermLike variable) where
    project (TermLike__ attr _hash pat) = attr :< pat
    {-# INLINE project #-}

-- This instance implements all class functions for the TermLike newtype
-- because the their implementations for the inner type may be specialized.
instance Hashable variable => Corecursive (TermLike variable) where
    embed (attr :< pat) = TermLike__ attr (hash pat) pat
    {-# INLINE embed #-}

instance TopBottom (TermLike variable) where
    isTop (Recursive.project -> _ :< TopF Top{}) = True
    isTop _ = False
    isBottom (Recursive.project -> _ :< BottomF Bottom{}) = True
    isBottom _ = False

instance InternalVariable variable => Binding (TermLike variable) where
    type VariableType (TermLike variable) = variable

    traverseVariable traversal termLike =
        case termLikeF of
            VariableF (Const unifiedVariable) ->
                mkVar <$> traversal unifiedVariable
            _ -> pure termLike
      where
        _ :< termLikeF = Recursive.project termLike

    traverseSetBinder traversal termLike =
        case termLikeF of
            MuF mu -> synthesize . MuF <$> muBinder traversal mu
            NuF nu -> synthesize . NuF <$> nuBinder traversal nu
            _ -> pure termLike
      where
        _ :< termLikeF = Recursive.project termLike

    traverseElementBinder traversal termLike =
        case termLikeF of
            ExistsF exists ->
                synthesize . ExistsF <$> existsBinder traversal exists
            ForallF forAll ->
                synthesize . ForallF <$> forallBinder traversal forAll
            _ -> pure termLike
      where
        _ :< termLikeF = Recursive.project termLike

instance Attribute.HasConstructorLike (TermLike variable) where
    extractConstructorLike (Recursive.project -> attrs :< _) =
        Attribute.extractConstructorLike attrs

instance Unparse (TermLike variable) => SQL.Column (TermLike variable) where
    defineColumn = SQL.defineTextColumn
    toColumn = SQL.toColumn . Pretty.renderText . Pretty.layoutOneLine . unparse

instance From (TermLike Concrete) (TermLike variable) where
    from = vacuousVariables
    {-# INLINE from #-}

vacuousVariables :: forall variable. TermLike Concrete -> TermLike variable
vacuousVariables (TermLike__ attrs hsh termLikeF) = TermLike__ attrs' hsh (vacuousVariablesF termLikeF)
  where
    !attrs' = attrs{termFreeVariables = FreeVariables.emptyFreeVariables}

vacuousVariablesF ::
    forall variable. TermLikeF Concrete (TermLike Concrete) -> TermLikeF variable (TermLike variable)
vacuousVariablesF = runIdentity . traverseVariablesF adjuster . fmap vacuousVariables
  where
    adjuster = AdjSomeVariableName (ElementVariableName absurd) (SetVariableName absurd)

instance (Hashable variable, Ord variable) => From Key (TermLike variable) where
    from = Recursive.unfold worker
      where
        worker :: Key -> CofreeF (TermLikeF variable) (TermAttributes variable) Key
        worker key =
            attrs' :< from @(KeyF _) keyF
          where
            attrs :< keyF = Recursive.project key
            attrs' = fromKeyAttributes attrs

instance InternalVariable variable => Substitute (TermLike variable) where
    type TermType (TermLike variable) = TermLike variable

    type VariableNameType (TermLike variable) = variable

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

    substitute = substituteWorker . Map.map Left
      where
        extractFreeVariables ::
            TermLike variable -> Set (SomeVariableName variable)
        extractFreeVariables = FreeVariables.toNames . freeVariables

        getTargetFreeVariables ::
            Either (TermLike variable) (SomeVariable variable) ->
            Set (SomeVariableName variable)
        getTargetFreeVariables =
            either extractFreeVariables (Set.singleton . variableName)

        renaming ::
            -- Original variable
            SomeVariable variable ->
            -- Renamed variable
            Maybe (SomeVariable variable) ->
            -- Substitution
            Map
                (SomeVariableName variable)
                (Either (TermLike variable) (SomeVariable variable)) ->
            Map
                (SomeVariableName variable)
                (Either (TermLike variable) (SomeVariable variable))
        renaming Variable{variableName} =
            maybe id (Map.insert variableName . Right)

        substituteWorker ::
            Map
                (SomeVariableName variable)
                (Either (TermLike variable) (SomeVariable variable)) ->
            TermLike variable ->
            TermLike variable
        substituteWorker subst termLike =
            substituteNone <|> substituteBinder <|> substituteVariable
                & fromMaybe substituteDefault
          where
            substituteNone :: Maybe (TermLike variable)
            substituteNone
                | Map.null subst' = pure termLike
                | otherwise = empty

            substituteBinder :: Maybe (TermLike variable)
            substituteBinder =
                runIdentity <$> matchWith traverseBinder worker termLike
              where
                worker ::
                    Binder (SomeVariable variable) (TermLike variable) ->
                    Identity (Binder (SomeVariable variable) (TermLike variable))
                worker Binder{binderVariable, binderChild} = do
                    let binderVariable' = avoidCapture binderVariable
                        -- Rename the freshened bound variable in the subterms.
                        subst'' = renaming binderVariable binderVariable' subst'
                    return
                        Binder
                            { binderVariable = fromMaybe binderVariable binderVariable'
                            , binderChild = substituteWorker subst'' binderChild
                            }

            substituteVariable :: Maybe (TermLike variable)
            substituteVariable =
                either id id <$> matchWith traverseVariable worker termLike
              where
                worker ::
                    SomeVariable variable ->
                    Either (TermLike variable) (SomeVariable variable)
                worker Variable{variableName} =
                    -- If the variable is not substituted or renamed, return the
                    -- original pattern.
                    fromMaybe
                        (Left termLike)
                        -- If the variable is renamed, 'Map.lookup' returns a
                        -- 'Right' which @traverseVariable@ embeds into
                        -- @patternType@. If the variable is substituted,
                        -- 'Map.lookup' returns a 'Left' which is used directly as
                        -- the result, exiting early from @traverseVariable@.
                        (Map.lookup variableName subst')

            substituteDefault =
                synthesize termLikeHead'
              where
                _ :< termLikeHead = Recursive.project termLike
                termLikeHead' = substituteWorker subst' <$> termLikeHead

            freeVars = extractFreeVariables termLike

            subst' = Map.intersection subst (Map.fromSet id freeVars)

            originalVariables = Set.difference freeVars (Map.keysSet subst')

            freeVariables' = Set.union originalVariables targetFreeVariables
              where
                targetFreeVariables =
                    foldl'
                        Set.union
                        Set.empty
                        (getTargetFreeVariables <$> subst')

            avoidCapture = refreshVariable freeVariables'

fromKeyAttributes ::
    Ord variable =>
    KeyAttributes ->
    TermAttributes variable
fromKeyAttributes attrs =
    TermAttributes
        { termSort = Attribute.keySort attrs
        , termFreeVariables = mempty
        , termTotal = Attribute.Total True
        , termFunction = Attribute.Function True
        , termDefined = Attribute.Defined True
        , termSimplified = Attribute.fullySimplified
        , termConstructorLike =
            Attribute.ConstructorLike (Just Attribute.ConstructorLikeHead)
        , termCreated = Attribute.Created Nothing
        }

toKeyAttributes :: TermAttributes variable -> Maybe KeyAttributes
toKeyAttributes attrs@(TermAttributes _ _ _ _ _ _ _ _)
    | Attribute.nullFreeVariables termFreeVariables
    , Attribute.isTotal termTotal
    , Attribute.isFunction termFunction
    , Attribute.isDefined termDefined
    , Attribute.isSimplifiedAnyCondition termSimplified
    , Attribute.isConstructorLike termConstructorLike =
        Just $ KeyAttributes termSort
    | otherwise = Nothing
  where
    TermAttributes
        { termSort
        , termFreeVariables
        , termTotal
        , termFunction
        , termDefined
        , termConstructorLike
        , termSimplified
        } = attrs

-- | Ensure that a 'TermLike' is a concrete, constructor-like term.
retractKey :: TermLike variable -> Maybe Key
retractKey =
    Recursive.fold worker
  where
    worker (attrs :< termLikeF) = do
        Monad.guard (Attribute.isConstructorLike attrs)
        attrs' <- toKeyAttributes attrs
        keyF <-
            case termLikeF of
                InternalBoolF internalBool ->
                    sequence (Key.InternalBoolF internalBool)
                InternalBytesF internalBytes ->
                    sequence (Key.InternalBytesF internalBytes)
                InternalIntF internalInt ->
                    sequence (Key.InternalIntF internalInt)
                InternalStringF internalString ->
                    sequence (Key.InternalStringF internalString)
                DomainValueF domainValue ->
                    sequence (Key.DomainValueF domainValue)
                InjF inj ->
                    sequence (Key.InjF inj)
                ApplySymbolF application ->
                    sequence (Key.ApplySymbolF application)
                InternalListF internalList ->
                    sequence (Key.InternalListF internalList)
                InternalMapF internalMap ->
                    sequence (Key.InternalMapF internalMap)
                InternalSetF internalSet ->
                    sequence (Key.InternalSetF internalSet)
                StringLiteralF stringLiteral ->
                    sequence (Key.StringLiteralF stringLiteral)
                _ -> empty
        pure (Recursive.embed (attrs' :< keyF))

instance
    ( AstWithLocation variable
    , AstWithLocation child
    ) =>
    AstWithLocation (TermLikeF variable child)
    where
    locationFromAst =
        \case
            AndF BinaryAnd{andSort} -> locationFromAst andSort
            ApplySymbolF Application{applicationSymbolOrAlias} ->
                locationFromAst applicationSymbolOrAlias
            ApplyAliasF Application{applicationSymbolOrAlias} ->
                locationFromAst applicationSymbolOrAlias
            BottomF Bottom{bottomSort} -> locationFromAst bottomSort
            CeilF Ceil{ceilResultSort} -> locationFromAst ceilResultSort
            DomainValueF domain -> locationFromAst $ domainValueSort domain
            EqualsF Equals{equalsResultSort} ->
                locationFromAst equalsResultSort
            ExistsF Exists{existsSort} -> locationFromAst existsSort
            FloorF Floor{floorResultSort} ->
                locationFromAst floorResultSort
            ForallF Forall{forallSort} -> locationFromAst forallSort
            IffF Iff{iffSort} -> locationFromAst iffSort
            ImpliesF Implies{impliesSort} ->
                locationFromAst impliesSort
            InF In{inResultSort} -> locationFromAst inResultSort
            MuF Mu{muVariable} -> locationFromAst muVariable
            NextF Next{nextSort} -> locationFromAst nextSort
            NotF Not{notSort} -> locationFromAst notSort
            NuF Nu{nuVariable} -> locationFromAst nuVariable
            OrF BinaryOr{orSort} -> locationFromAst orSort
            RewritesF Rewrites{rewritesSort} ->
                locationFromAst rewritesSort
            StringLiteralF _ -> AstLocationUnknown
            TopF Top{topSort} -> locationFromAst topSort
            VariableF (Const variable) -> locationFromAst variable
            InhabitantF Inhabitant{inhSort} -> locationFromAst inhSort
            InjF Inj{injChild} -> locationFromAst injChild
            SignednessF (Const signedness) -> locationFromAst signedness
            EndiannessF (Const endianness) -> locationFromAst endianness
            InternalBoolF (Const InternalBool{internalBoolSort}) ->
                locationFromAst internalBoolSort
            InternalBytesF (Const InternalBytes{internalBytesSort}) ->
                locationFromAst internalBytesSort
            InternalIntF (Const InternalInt{internalIntSort}) ->
                locationFromAst internalIntSort
            InternalStringF (Const InternalString{internalStringSort}) ->
                locationFromAst internalStringSort
            InternalListF InternalList{internalListSort} ->
                locationFromAst internalListSort
            InternalMapF InternalAc{builtinAcSort} ->
                locationFromAst builtinAcSort
            InternalSetF InternalAc{builtinAcSort} ->
                locationFromAst builtinAcSort

instance AstWithLocation variable => AstWithLocation (TermLike variable) where
    locationFromAst = locationFromAst . tailF . Recursive.project

{- | Use the provided traversal to replace all variables in a 'TermLikeF' head.

__Warning__: @traverseVariablesF@ will capture variables if the provided
traversal is not injective!
-}
traverseVariablesF ::
    Applicative f =>
    AdjSomeVariableName (variable1 -> f variable2) ->
    TermLikeF variable1 child ->
    f (TermLikeF variable2 child)
traverseVariablesF adj =
    \case
        -- Non-trivial cases
        ExistsF any0 -> ExistsF <$> traverseVariablesExists any0
        ForallF all0 -> ForallF <$> traverseVariablesForall all0
        MuF any0 -> MuF <$> traverseVariablesMu any0
        NuF any0 -> NuF <$> traverseVariablesNu any0
        VariableF variable -> VariableF <$> traverseConstVariable variable
        -- Trivial cases
        AndF andP -> pure (AndF andP)
        ApplySymbolF applySymbolF -> pure (ApplySymbolF applySymbolF)
        ApplyAliasF applyAliasF -> pure (ApplyAliasF applyAliasF)
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
        InternalBoolF boolP -> pure (InternalBoolF boolP)
        InternalBytesF bytesP -> pure (InternalBytesF bytesP)
        InternalIntF intP -> pure (InternalIntF intP)
        InternalStringF stringP -> pure (InternalStringF stringP)
        InternalListF listP -> pure (InternalListF listP)
        InternalMapF mapP -> pure (InternalMapF mapP)
        InternalSetF setP -> pure (InternalSetF setP)
        TopF topP -> pure (TopF topP)
        InhabitantF s -> pure (InhabitantF s)
        EndiannessF endianness -> pure (EndiannessF endianness)
        SignednessF signedness -> pure (SignednessF signedness)
        InjF inj -> pure (InjF inj)
  where
    trElemVar = traverse $ traverseElementVariableName adj
    trSetVar = traverse $ traverseSetVariableName adj
    traverseConstVariable (Const variable) =
        Const <$> traverseSomeVariable adj variable
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

extractAttributes :: TermLike variable -> TermAttributes variable
extractAttributes (Recursive.project -> attrs :< _) = attrs

instance HasFreeVariables (TermLike variable) variable where
    freeVariables = Attribute.freeVariables . extractAttributes

{- | Use the provided mapping to replace all variables in a 'StepPattern'.

@mapVariables@ is lazy: it descends into its argument only as the result is
demanded. Intermediate allocation from composing multiple transformations with
@mapVariables@ is amortized; the intermediate trees are never fully resident.

See also: 'traverseVariables'
-}
mapVariables ::
    forall variable1 variable2.
    Ord variable1 =>
    FreshPartialOrd variable2 =>
    Hashable variable2 =>
    AdjSomeVariableName (variable1 -> variable2) ->
    TermLike variable1 ->
    TermLike variable2
mapVariables adj termLike =
    runIdentity (traverseVariables ((.) pure <$> adj) termLike)
{-# INLINE mapVariables #-}

{- | Use the provided traversal to replace all variables in a 'TermLike'.

@traverseVariables@ is strict, i.e. its argument is fully evaluated before it
returns. When composing multiple transformations with @traverseVariables@, the
intermediate trees will be fully allocated; @mapVariables@ is more composable in
this respect.

See also: 'mapVariables'
-}
traverseVariables ::
    forall variable1 variable2 m.
    Ord variable1 =>
    FreshPartialOrd variable2 =>
    Hashable variable2 =>
    Monad m =>
    AdjSomeVariableName (variable1 -> m variable2) ->
    TermLike variable1 ->
    m (TermLike variable2)
traverseVariables adj termLike =
    renameFreeVariables
        adj
        (Attribute.freeVariables @_ @variable1 termLike)
        >>= Reader.runReaderT (Recursive.fold worker termLike)
  where
    adjReader = (.) lift <$> adj
    trElemVar = traverse $ traverseElementVariableName adjReader
    trSetVar = traverse $ traverseSetVariableName adjReader
    traverseExists avoiding =
        existsBinder (renameElementBinder trElemVar avoiding)
    traverseForall avoiding =
        forallBinder (renameElementBinder trElemVar avoiding)
    traverseMu avoiding =
        muBinder (renameSetBinder trSetVar avoiding)
    traverseNu avoiding =
        nuBinder (renameSetBinder trSetVar avoiding)

    worker ::
        Base
            (TermLike variable1)
            (RenamingT variable1 variable2 m (TermLike variable2)) ->
        RenamingT variable1 variable2 m (TermLike variable2)
    worker (attrs :< termLikeF) = do
        ~attrs' <- traverseAttributeVariables askSomeVariableName attrs
        let ~avoiding = freeVariables attrs'
        termLikeF' <- case termLikeF of
            VariableF (Const unifiedVariable) -> do
                unifiedVariable' <- askSomeVariable unifiedVariable
                (pure . VariableF) (Const unifiedVariable')
            ExistsF exists -> ExistsF <$> traverseExists avoiding exists
            ForallF forAll -> ForallF <$> traverseForall avoiding forAll
            MuF mu -> MuF <$> traverseMu avoiding mu
            NuF nu -> NuF <$> traverseNu avoiding nu
            _ ->
                sequence termLikeF
                    >>=
                    -- traverseVariablesF will not actually call the traversals
                    -- because all the cases with variables are handled above.
                    traverseVariablesF askSomeVariableName
        (pure . Recursive.embed) (attrs' :< termLikeF')

updateCallStack ::
    forall variable.
    HasCallStack =>
    TermLike variable ->
    TermLike variable
updateCallStack = Lens.set created callstack
  where
    created = termLikeAttributes . Lens.Product.field @"termCreated"
    callstack =
        Attribute.Created
            . Just
            . GHC.popCallStack
            . GHC.popCallStack
            $ GHC.callStack

{- | A 'Lens'' for editing attributes of a 'TermLike'. This is more efficient
 than using 'project' followed by 'embed', because it avoids recomputing
 the hash on `embed`.
-}
termLikeAttributes :: Lens' (TermLike variable) (TermAttributes variable)
termLikeAttributes =
    Lens.lens
        (\(TermLike__ attrs _ _) -> attrs)
        ( \(TermLike__ _ hsh termLikeF) attrs ->
            TermLike__ attrs hsh termLikeF
        )

-- | Construct a variable pattern.
mkVar ::
    HasCallStack =>
    Hashable variable =>
    Ord variable =>
    SomeVariable variable ->
    TermLike variable
mkVar = updateCallStack . synthesize . VariableF . Const

depth :: TermLike variable -> Int
depth = Recursive.fold levelDepth
  where
    levelDepth (_ :< termF) = 1 + foldl' max 0 termF

instance
    (Hashable variable, Ord variable) =>
    From (TermLike variable) (Pattern.Pattern variable (TermAttributes variable))
    where
    from = uninternalize

uninternalize ::
    forall variable.
    Hashable variable =>
    Ord variable =>
    TermLike variable ->
    Pattern.Pattern variable (TermAttributes variable)
uninternalize = Pattern.Pattern . Recursive.cata go
  where
    go ::
        CofreeF
            (TermLikeF variable)
            (TermAttributes variable)
            (Cofree (PatternF.PatternF variable) (TermAttributes variable)) ->
        Cofree (PatternF.PatternF variable) (TermAttributes variable)
    go (attr :< trmLikePat) = case trmLikePat of
        AndF BinaryAnd{andSort, andFirst, andSecond} -> wrap $ PatternF.AndF And{andSort, andChildren = [andFirst, andSecond]}
        ApplySymbolF application ->
            wrap $
                PatternF.ApplicationF $
                    first Symbol.toSymbolOrAlias application
        ApplyAliasF application ->
            wrap $
                PatternF.ApplicationF $
                    first Alias.toSymbolOrAlias application
        BottomF bottom -> wrap $ PatternF.BottomF bottom
        CeilF ceil -> wrap $ PatternF.CeilF ceil
        DomainValueF domainValue -> wrap $ PatternF.DomainValueF domainValue
        EqualsF equals -> wrap $ PatternF.EqualsF equals
        ExistsF exists -> wrap $ PatternF.ExistsF exists
        FloorF floor' -> wrap $ PatternF.FloorF floor'
        ForallF forall' -> wrap $ PatternF.ForallF forall'
        IffF iff -> wrap $ PatternF.IffF iff
        ImpliesF implies -> wrap $ PatternF.ImpliesF implies
        InF in' -> wrap $ PatternF.InF in'
        MuF mu -> wrap $ PatternF.MuF mu
        NextF next -> wrap $ PatternF.NextF next
        NotF not' -> wrap $ PatternF.NotF not'
        NuF nu -> wrap $ PatternF.NuF nu
        OrF BinaryOr{orSort, orFirst, orSecond} -> wrap $ PatternF.OrF Or{orSort, orChildren = [orFirst, orSecond]}
        RewritesF rewrites -> wrap $ PatternF.RewritesF rewrites
        TopF top -> wrap $ PatternF.TopF top
        InhabitantF inhabitant -> wrap $ PatternF.InhabitantF inhabitant
        StringLiteralF stringLiteral -> wrap $ PatternF.StringLiteralF stringLiteral
        InternalBoolF (Const (InternalBool boolSort boolValue)) ->
            uninternalizeInternalValue boolSort $
                if boolValue then "true" else "false"
        InternalBytesF (Const (InternalBytes bytesSort bytesValue)) ->
            uninternalizeInternalValue bytesSort $
                Encoding.decode8Bit $
                    ByteString.fromShort bytesValue
        InternalIntF (Const (InternalInt intSort intValue)) ->
            uninternalizeInternalValue intSort $
                Text.pack $
                    show intValue
        InternalStringF (Const (InternalString stringSort stringValue)) ->
            uninternalizeInternalValue stringSort stringValue
        InternalListF internalList -> uninternalizeInternalList internalList
        InternalMapF internalMap -> uninternalizeInternalAc internalMap
        InternalSetF internalSet -> uninternalizeInternalAc internalSet
        VariableF variable -> wrap $ PatternF.VariableF variable
        EndiannessF (Const endianness) ->
            wrap $
                PatternF.ApplicationF $
                    first Symbol.toSymbolOrAlias $
                        Endianness.toApplication endianness
        SignednessF (Const signedness) ->
            wrap $
                PatternF.ApplicationF $
                    first Symbol.toSymbolOrAlias $
                        Signedness.toApplication signedness
        InjF inj ->
            wrap $
                PatternF.ApplicationF $
                    first Symbol.toSymbolOrAlias $
                        Inj.toApplication inj
      where
        wrap x = CofreeT $ Identity $ attr :< x
        uninternalizeInternalValue sort strVal =
            wrap $
                PatternF.DomainValueF $
                    DomainValue sort $
                        wrap $
                            PatternF.StringLiteralF $
                                Const $
                                    StringLiteral strVal

        -- The uninternalized lists and ac types are all of the following form:
        -- If the structure has no elements, we simply get the application of the unit symbol with no arguments
        -- If the structure has exactly one child, we just get the child.
        -- This child will be an application of internalElement symbol to its arguments
        -- For multiple children we will get a right associative tree of Applications, namely
        --   ApplicationF (Application concatSymbol [
        --       x1,
        --       ApplicationF (Application concatSymbol [
        --           x2,
        --           ...ApplicationF (Application concatSymbol [xn-1, x_n]...
        --         ])
        --    ])
        foldApplication unitSymbol concatSymbol = foldAux
          where
            foldAux = \case
                [] ->
                    wrap $
                        PatternF.ApplicationF $
                            Application (Symbol.toSymbolOrAlias unitSymbol) []
                [x] -> x
                (x : xs) ->
                    wrap $
                        PatternF.ApplicationF $
                            Application (Symbol.toSymbolOrAlias concatSymbol) [x, foldAux xs]

        uninternalizeInternalList InternalList{internalListUnit, internalListConcat, internalListElement, internalListChild} =
            foldApplication internalListUnit internalListConcat children
          where
            uniternalizeListElement e =
                wrap $
                    PatternF.ApplicationF $
                        Application (Symbol.toSymbolOrAlias internalListElement) [e]
            children = map uniternalizeListElement $ toList internalListChild

        uninternalizeInternalAc ::
            forall normalized.
            AcWrapper normalized =>
            InternalAc Key normalized (Cofree (PatternF.PatternF variable) (TermAttributes variable)) ->
            Cofree (PatternF.PatternF variable) (TermAttributes variable)
        uninternalizeInternalAc InternalAc{builtinAcUnit, builtinAcConcat, builtinAcElement, builtinAcChild} =
            foldApplication builtinAcUnit builtinAcConcat children
          where
            NormalizedAc{elementsWithVariables, concreteElements, opaque} = unwrapAc builtinAcChild

            uninternalizeAcElement =
                wrap
                    . PatternF.ApplicationF
                    . Application (Symbol.toSymbolOrAlias builtinAcElement)

            uninternalizedElementsWithVariables =
                map
                    ( uninternalizeAcElement
                        . elementToApplicationArgs
                    )
                    elementsWithVariables

            uninternalizedConcreteElements =
                map
                    ( \(k, v) ->
                        uninternalizeAcElement $
                            uninternalizeKey k : concreteElementToApplicationArgs v
                    )
                    $ HashMap.toList concreteElements

            uninternalizeKey = Pattern.getPattern . uninternalize . from

            children =
                uninternalizedElementsWithVariables ++ uninternalizedConcreteElements ++ opaque
