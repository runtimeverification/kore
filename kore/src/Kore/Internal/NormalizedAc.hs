{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Internal.NormalizedAc (
    AcWrapper (..),
    wrapElement,
    unwrapElement,
    wrapConcreteElement,
    NormalizedAc (..),
    nullAc,
    emptyNormalizedAc,
    retractSingleOpaqueElem,
    toSingleOpaqueElem,
    isSymbolicKeyOfAc,
    lookupSymbolicKeyOfAc,
    removeSymbolicKeyOfAc,
    isConcreteKeyOfAc,
    removeConcreteKeyOfAc,
    getSymbolicKeysOfAc,
    getConcreteKeysOfAc,
    getSymbolicValuesOfAc,
    getConcreteValuesOfAc,
    unparsedChildren,
    InternalAc (..),
    normalizedAcDefined,
    normalizedAcConstructorLike,
    normalizedAcTotal,
    unparseInternalAc,
    AcPair,
    pattern AcPair,
    acPairToPair,
    PairWiseElements (..),
    generatePairWiseElements,
) where

import Control.Lens qualified as Lens
import Control.Lens.Iso (
    Iso',
 )
import Data.Binary (Binary (..))
import Data.HashMap.Strict (
    HashMap,
 )
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (
    HashSet,
 )
import Data.HashSet qualified as HashSet
import Data.Kind (
    Type,
 )
import Data.List qualified as List
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.ConstructorLike
import Kore.Attribute.Pattern.Defined
import Kore.Attribute.Pattern.Total
import Kore.Debug
import Kore.Internal.Symbol hiding (
    isConstructorLike,
 )
import Kore.Sort
import Kore.Unparser
import Prelude.Kore
import Pretty (
    (<+>),
 )
import Pretty qualified

{- | Establishes a bijection between value wrappers and entire-structure
wrappers, with a few utility functions for the two.
-}
class AcWrapper (normalized :: Type -> Type -> Type) where
    -- TODO (thomas.tuegel): Make this a type family?
    data Value normalized child
    data Element normalized child

    unwrapAc :: normalized key child -> NormalizedAc normalized key child
    wrapAc :: NormalizedAc normalized key child -> normalized key child

    -- | Pairs the values in two wrappers as they should be paired for
    --    unification.
    alignValues ::
        Value normalized a ->
        Value normalized b ->
        Value normalized (a, b)

    elementIso ::
        Iso' (Element normalized child) (child, Value normalized child)

    unparseElement ::
        (child -> Pretty.Doc ann) ->
        Element normalized child ->
        Pretty.Doc ann

    unparseConcreteElement ::
        (key -> Pretty.Doc ann) ->
        (child -> Pretty.Doc ann) ->
        (key, Value normalized child) ->
        Pretty.Doc ann

    elementToApplicationArgs :: Element normalized child -> [child]

    concreteElementToApplicationArgs :: Value normalized child -> [child]

instance
    (AcWrapper normalized, From key child) =>
    From (key, Value normalized child) (Element normalized child)
    where
    from (key, value) = wrapElement (from @key @child key, value)

unwrapElement ::
    AcWrapper normalized =>
    Element normalized child ->
    (child, Value normalized child)
unwrapElement = Lens.view elementIso

wrapElement ::
    AcWrapper normalized =>
    (child, Value normalized child) ->
    Element normalized child
wrapElement = Lens.review elementIso

wrapConcreteElement ::
    AcWrapper normalized =>
    From key child =>
    (key, Value normalized child) ->
    Element normalized child
wrapConcreteElement = from

unparsedChildren ::
    forall ann child key normalized.
    AcWrapper normalized =>
    Symbol ->
    (key -> Pretty.Doc ann) ->
    (child -> Pretty.Doc ann) ->
    normalized key child ->
    [Pretty.Doc ann]
unparsedChildren elementSymbol keyUnparser childUnparser wrapped =
    (elementUnparser <$> elementsWithVariables)
        ++ (concreteElementUnparser <$> HashMap.toList concreteElements)
        ++ (child . childUnparser <$> opaque)
  where
    unwrapped :: NormalizedAc normalized key child
    -- Matching needed only for getting compiler notifications when
    -- the NormalizedAc field count changes.
    unwrapped@(NormalizedAc _ _ _) = unwrapAc wrapped

    NormalizedAc{elementsWithVariables} = unwrapped
    NormalizedAc{concreteElements} = unwrapped
    NormalizedAc{opaque} = unwrapped
    element = (<>) ("/* element: */" <+> unparse elementSymbol)
    concreteElement = (<>) ("/* concrete element: */" <+> unparse elementSymbol)
    child = (<+>) "/* opaque child: */"

    elementUnparser :: Element normalized child -> Pretty.Doc ann
    elementUnparser = element . unparseElement childUnparser

    concreteElementUnparser :: (key, Value normalized child) -> Pretty.Doc ann
    concreteElementUnparser =
        concreteElement . unparseConcreteElement keyUnparser childUnparser

{- | Internal representation for associative-commutative domain values.

The valueWrapper is a data type holding the non-key part of elements.
For a set, the valueWapper would be something equivalent to @Data.Empty.T@.
For a map, it would be something equivalent to @Identity@.
-}
data NormalizedAc (collection :: Type -> Type -> Type) key child = NormalizedAc
    { elementsWithVariables :: [Element collection child]
    -- ^ Non-concrete elements of the structure.
    -- These would be of sorts @(Int, String)@ for a map from @Int@ to @String@.
    , concreteElements :: HashMap key (Value collection child)
    -- ^ Concrete elements of the structure.
    -- These would be of sorts @(Int, String)@ for a map from @Int@ to @String@.
    , opaque :: [child]
    -- ^ Unoptimized (i.e. non-element) parts of the structure.
    }
    deriving stock (GHC.Generic)

nullAc :: NormalizedAc normalized key child -> Bool
nullAc normalizedAc =
    null (elementsWithVariables normalizedAc)
        && null (concreteElements normalizedAc)
        && null (opaque normalizedAc)

isSymbolicKeyOfAc ::
    AcWrapper normalized =>
    Eq child =>
    child ->
    normalized key child ->
    Bool
isSymbolicKeyOfAc child normalized =
    child `elem` getSymbolicKeysOfAc normalized

getSymbolicKeysOfAc ::
    AcWrapper normalized =>
    normalized key child ->
    [child]
getSymbolicKeysOfAc
    ( unwrapAc ->
            NormalizedAc{elementsWithVariables}
        ) =
        fst . unwrapElement <$> elementsWithVariables

getSymbolicValuesOfAc ::
    AcWrapper normalized =>
    normalized key child ->
    [Value normalized child]
getSymbolicValuesOfAc
    ( unwrapAc ->
            NormalizedAc{elementsWithVariables}
        ) =
        snd . unwrapElement <$> elementsWithVariables

lookupSymbolicKeyOfAc ::
    AcWrapper normalized =>
    Eq child =>
    child ->
    NormalizedAc normalized key child ->
    Maybe (Value normalized child)
lookupSymbolicKeyOfAc
    child
    NormalizedAc{elementsWithVariables} =
        snd <$> find (\(key, _) -> child == key) elements
      where
        elements = unwrapElement <$> elementsWithVariables

removeSymbolicKeyOfAc ::
    AcWrapper normalized =>
    Ord child =>
    Hashable child =>
    child ->
    NormalizedAc normalized key child ->
    NormalizedAc normalized key child
removeSymbolicKeyOfAc
    child
    normalized@NormalizedAc{elementsWithVariables} =
        normalized
            { elementsWithVariables =
                fmap wrapElement . HashMap.toList $
                    HashMap.delete child unwrappedMap
            }
      where
        unwrappedMap =
            HashMap.fromList $ fmap unwrapElement elementsWithVariables

isConcreteKeyOfAc ::
    AcWrapper normalized =>
    Ord key =>
    key ->
    normalized key child ->
    Bool
isConcreteKeyOfAc key normalized =
    key `elem` getConcreteKeysOfAc normalized

getConcreteKeysOfAc ::
    AcWrapper normalized =>
    normalized key child ->
    [key]
getConcreteKeysOfAc
    ( unwrapAc ->
            NormalizedAc{concreteElements}
        ) =
        HashMap.keys concreteElements

getConcreteValuesOfAc ::
    AcWrapper normalized =>
    normalized key child ->
    [Value normalized child]
getConcreteValuesOfAc
    ( unwrapAc ->
            NormalizedAc{concreteElements}
        ) =
        HashMap.elems concreteElements

removeConcreteKeyOfAc ::
    Ord key =>
    Hashable key =>
    key ->
    NormalizedAc normalized key child ->
    NormalizedAc normalized key child
removeConcreteKeyOfAc
    key
    normalized@NormalizedAc{concreteElements} =
        normalized
            { concreteElements =
                HashMap.delete key concreteElements
            }

deriving stock instance
    ( Eq key
    , Eq child
    , Eq (Element collection child)
    , Eq (Value collection child)
    ) =>
    Eq (NormalizedAc collection key child)

deriving stock instance
    ( Ord key
    , Ord child
    , Ord (Element collection child)
    , Ord (Value collection child)
    ) =>
    Ord (NormalizedAc collection key child)

deriving stock instance
    ( Show key
    , Show child
    , Show (Element collection child)
    , Show (Value collection child)
    ) =>
    Show (NormalizedAc collection key child)

deriving stock instance
    (Functor (Element collection), Functor (Value collection)) =>
    Functor (NormalizedAc collection key)

deriving stock instance
    (Foldable (Element collection), Foldable (Value collection)) =>
    Foldable (NormalizedAc collection key)

deriving stock instance
    (Traversable (Element collection), Traversable (Value collection)) =>
    Traversable (NormalizedAc collection key)

instance
    ( Binary key
    , Hashable key
    , Binary child
    , Binary (Element collection child)
    , Binary (Value collection child)
    ) =>
    Binary (NormalizedAc collection key child)
    where
    put NormalizedAc{elementsWithVariables, concreteElements, opaque} = do
        put elementsWithVariables
        put $ HashMap.toList concreteElements
        put opaque
    get =
        NormalizedAc <$> get <*> fmap HashMap.fromList get <*> get

instance
    ( Hashable key
    , Hashable child
    , Hashable (Element collection child)
    , Hashable (Value collection child)
    ) =>
    Hashable (NormalizedAc collection key child)
    where
    hashWithSalt salt normalized@(NormalizedAc _ _ _) =
        salt
            `hashWithSalt` elementsWithVariables
            `hashWithSalt` HashMap.toList concreteElements
            `hashWithSalt` opaque
      where
        NormalizedAc{elementsWithVariables} = normalized
        NormalizedAc{concreteElements} = normalized
        NormalizedAc{opaque} = normalized

instance
    ( NFData key
    , NFData child
    , NFData (Element collection child)
    , NFData (Value collection child)
    ) =>
    NFData (NormalizedAc collection key child)

instance SOP.Generic (NormalizedAc key valueWrapper child)

instance SOP.HasDatatypeInfo (NormalizedAc key valueWrapper child)

instance
    ( Debug key
    , Debug child
    , Debug (Element collection child)
    , Debug (Value collection child)
    ) =>
    Debug (NormalizedAc collection key child)

instance
    ( Debug key
    , Debug child
    , Debug (Element collection child)
    , Debug (Value collection child)
    , Diff key
    , Diff child
    , Diff (Element collection child)
    , Diff (Value collection child)
    ) =>
    Diff (NormalizedAc collection key child)

emptyNormalizedAc :: NormalizedAc key valueWrapper child
emptyNormalizedAc =
    NormalizedAc
        { elementsWithVariables = []
        , concreteElements = HashMap.empty
        , opaque = []
        }

retractSingleOpaqueElem ::
    NormalizedAc key valueWrapper child ->
    Maybe child
retractSingleOpaqueElem
    NormalizedAc
        { elementsWithVariables
        , concreteElements
        , opaque
        }
        | null elementsWithVariables
        , null concreteElements
        , [singleOpaqueElem] <- opaque =
            Just singleOpaqueElem
        | otherwise = Nothing

toSingleOpaqueElem ::
    child ->
    NormalizedAc key valueWrapper child
toSingleOpaqueElem opaqueElem =
    emptyNormalizedAc{opaque = [opaqueElem]}

-- TODO (thomas.tuegel): Change order of parameters.

-- | Internal representation of associative-commutative builtin terms.
data InternalAc key (normalized :: Type -> Type -> Type) child = InternalAc
    { builtinAcSort :: !Sort
    , builtinAcUnit :: !Symbol
    , builtinAcElement :: !Symbol
    , builtinAcConcat :: !Symbol
    , builtinAcChild :: normalized key child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Foldable, Functor, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance
    Hashable (normalized key child) =>
    Hashable (InternalAc key normalized child)
    where
    hashWithSalt salt builtin =
        hashWithSalt salt builtinAcChild
      where
        InternalAc{builtinAcChild} = builtin

instance
    NFData (normalized key child) =>
    NFData (InternalAc key normalized child)

instance
    Debug (normalized key child) =>
    Debug (InternalAc key normalized child)

instance
    (Debug (normalized key child), Diff (normalized key child)) =>
    Diff (InternalAc key normalized child)

unparseInternalAc ::
    AcWrapper normalized =>
    (key -> Pretty.Doc ann) ->
    (child -> Pretty.Doc ann) ->
    InternalAc key normalized child ->
    Pretty.Doc ann
unparseInternalAc keyUnparser childUnparser builtinAc =
    unparseConcat' (unparse builtinAcUnit) (unparse builtinAcConcat) $
        unparsedChildren builtinAcElement keyUnparser childUnparser builtinAcChild
  where
    InternalAc{builtinAcChild} = builtinAc
    InternalAc{builtinAcUnit} = builtinAc
    InternalAc{builtinAcElement} = builtinAc
    InternalAc{builtinAcConcat} = builtinAc

normalizedAcDefined ::
    (Foldable (Element collection), Foldable (Value collection)) =>
    NormalizedAc collection key Defined ->
    Defined
normalizedAcDefined ac@(NormalizedAc _ _ _) =
    case ac of
        NormalizedAc
            { elementsWithVariables = []
            , opaque = []
            } -> sameAsChildren
        NormalizedAc
            { elementsWithVariables = [_]
            , concreteElements
            , opaque = []
            }
                | HashMap.null concreteElements -> sameAsChildren
        NormalizedAc
            { elementsWithVariables = []
            , concreteElements
            , opaque = [_]
            }
                | HashMap.null concreteElements -> sameAsChildren
        _ -> Defined False
  where
    sameAsChildren = fold ac

normalizedAcTotal ::
    (Foldable (Element collection), Foldable (Value collection)) =>
    NormalizedAc collection key Total ->
    Total
normalizedAcTotal ac@(NormalizedAc _ _ _) =
    case ac of
        NormalizedAc
            { elementsWithVariables = []
            , opaque = []
            } -> sameAsChildren
        NormalizedAc
            { elementsWithVariables = [_]
            , concreteElements
            , opaque = []
            }
                | HashMap.null concreteElements -> sameAsChildren
        NormalizedAc
            { elementsWithVariables = []
            , concreteElements
            , opaque = [_]
            }
                | HashMap.null concreteElements -> sameAsChildren
        _ -> Total False
  where
    sameAsChildren = fold ac

normalizedAcConstructorLike ::
    ( HasConstructorLike key
    , HasConstructorLike (Value collection ConstructorLike)
    ) =>
    NormalizedAc collection key ConstructorLike ->
    ConstructorLike
normalizedAcConstructorLike ac@(NormalizedAc _ _ _) =
    case ac of
        NormalizedAc
            { elementsWithVariables = []
            , concreteElements
            , opaque = []
            }
                | all pairIsConstructorLike concreteElementsList ->
                    ConstructorLike . Just $ ConstructorLikeHead
              where
                concreteElementsList = HashMap.toList concreteElements
                pairIsConstructorLike (key, value) =
                    assertConstructorLike "" key $ isConstructorLike value
        _ -> ConstructorLike Nothing

{- | A representation for associative-commutative collections
with just two elements of the same type.

Invariant: Given @AcPair_ x y@, @x < y == True@

We guarantee this invariant by keeping @AcPair@ abstract and only
exposing accessors @pattern AcPair@ and @acPairToPair@.
-}
data AcPair a = AcPair_ a a
    deriving stock (Eq)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable)

pattern AcPair :: a -> a -> AcPair a
pattern AcPair a1 a2 <- AcPair_ a1 a2
{-# COMPLETE AcPair #-}

acPairToPair :: AcPair a -> (a, a)
acPairToPair (AcPair a1 a2) = (a1, a2)

type ConcreteElement key normalized child = (key, Value normalized child)

{- | 'PairWiseElements' is a representation of the elements of all subcollections
 necessary for proving the definedness of a collection.
-}
data PairWiseElements normalized key child = PairWiseElements
    { symbolicPairs ::
        !(HashSet (AcPair (Element normalized child)))
    , concretePairs ::
        !(HashSet (AcPair (ConcreteElement key normalized child)))
    , opaquePairs ::
        !(HashSet (AcPair child))
    , symbolicConcretePairs ::
        !(HashSet (Element normalized child, ConcreteElement key normalized child))
    , symbolicOpaquePairs ::
        !(HashSet (Element normalized child, child))
    , concreteOpaquePairs ::
        !(HashSet (ConcreteElement key normalized child, child))
    }

-- | Generates the 'PairWiseElements' for a 'AcWrapper' collection.
generatePairWiseElements ::
    AcWrapper normalized =>
    Ord key =>
    Ord child =>
    Ord (Element normalized child) =>
    Ord (Value normalized child) =>
    Hashable key =>
    Hashable child =>
    Hashable (Element normalized child) =>
    Hashable (Value normalized child) =>
    normalized key child ->
    PairWiseElements normalized key child
generatePairWiseElements (unwrapAc -> normalized) =
    PairWiseElements
        { symbolicPairs = pairWiseElemsOfSameType symbolicElems
        , -- Note: We know the elements are concreteElems are distinct,
          -- so we use a special version of pairWiseElemsOfSameType
          -- to handle them more efficiently. If that changes, this will need to be
          -- adjusted. We make no such assumption for symboliccElems or
          -- opaqueElems; do they offer a similar guarantee?
          concretePairs = pairWiseElemsOfSameTypeDistinct concreteElems
        , opaquePairs = pairWiseElemsOfSameType opaqueElems
        , symbolicConcretePairs =
            pairWiseElemsOfDifferentTypes symbolicElems concreteElems
        , symbolicOpaquePairs =
            pairWiseElemsOfDifferentTypes symbolicElems opaqueElems
        , concreteOpaquePairs =
            pairWiseElemsOfDifferentTypes concreteElems opaqueElems
        }
  where
    symbolicElems = elementsWithVariables normalized
    concreteElems = HashMap.toList . concreteElements $ normalized
    opaqueElems = opaque normalized
    -- Pre-sorting the elements cuts comparisons from quadratic to
    -- linearithmic, and reduces allocations by about half.
    pairWiseElemsOfSameTypeDistinct (List.sort -> elems) =
        [AcPair_ x y | x : ys <- List.tails elems, y <- ys]
            & HashSet.fromList
    -- Without assuming unique elements, we additionally pay to drop any
    -- duplicates. If there are many of these, then we can `groupBy` after
    -- sorting, or build a `Set`, to bring this down to linearithmic.
    pairWiseElemsOfSameType (List.sort -> elems) =
        [AcPair_ x y | x : ys <- List.tails elems, y <- dropWhile (== x) ys]
            & HashSet.fromList
    pairWiseElemsOfDifferentTypes elems1 elems2 =
        (,) <$> elems1 <*> elems2
            & HashSet.fromList
