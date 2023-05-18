{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Kore.Builtin.AssociativeCommutative
Description : Handles built-in operations which are associative, commutative,
              with neutral elements, key-based, with unique keys, and which
              return bottom when applied to unique keys.
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.AssociativeCommutative as Ac
@
-}
module Kore.Builtin.AssociativeCommutative (
    asInternalConcrete,
    asPattern,
    ConcatSymbol (..),
    ConcreteElements (..),
    evalConcatNormalizedOrBottom,
    NormalizedOrBottom (..),
    Opaque (..),
    returnAc,
    returnConcreteAc,
    TermWrapper (..),
    renormalize,
    TermNormalizedAc,
    unifyEqualsNormalized,
    matchUnifyEqualsNormalizedAc,
    UnitSymbol (..),
    VariableElements (..),
    UnifyEqualsNormAc (..),
) where

import Control.Error (
    MaybeT,
 )
import Control.Monad qualified as Monad
import Data.Binary (Binary)
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
import Data.List qualified
import Data.List qualified as List
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.Simplified qualified as Attribute (
    Simplified,
 )
import Kore.Attribute.Symbol qualified as Attribute (
    Symbol,
 )
import Kore.Builtin.Builtin qualified as Builtin
import Kore.Builtin.Map.Map qualified as Map
import Kore.Builtin.Set.Set qualified as Set
import Kore.Debug
import Kore.IndexedModule.MetadataTools (
    SmtMetadataTools,
 )
import Kore.Internal.Condition (
    Condition,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.Conditional (
    Conditional,
    andCondition,
    withCondition,
 )
import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.InternalMap
import Kore.Internal.InternalSet
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.SideCondition qualified as SideCondition (
    topTODO,
 )
import Kore.Internal.Symbol (
    Symbol,
 )
import Kore.Internal.TermLike (
    Key,
    TermLike,
    mkElemVar,
    termLikeSort,
    pattern App_,
    pattern ElemVar_,
    pattern InternalMap_,
    pattern InternalSet_,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Log.DebugUnifyBottom (
    debugUnifyBottomAndReturnBottom,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Simplify as Simplifier
import Kore.Sort (
    Sort,
    sameSort,
 )
import Kore.Syntax.Variable
import Kore.Unification.Unify (
    MonadUnify,
 )
import Kore.Unparser (
    unparse,
    unparseToString,
 )
import Kore.Unparser qualified as Unparser
import Logic
import Prelude.Kore
import Pretty qualified

-- | Any @TermWrapper@ may be inside of an 'InternalAc'.
class
    AcWrapper (normalized :: Type -> Type -> Type) =>
    TermWrapper normalized
    where
    -- | Render a normalized value (e.g. 'NormalizedSet') as an 'InternalAc'.
    --
    --    The result sort must be hooked to the builtin normalized sort (e.g. @Set@).
    asInternalBuiltin ::
        SmtMetadataTools Attribute.Symbol ->
        Sort ->
        normalized Key child ->
        InternalAc Key normalized child

    -- TODO (thomas.tuegel): Use From.

    -- | Render a normalized value (e.g. 'NormalizedSet') as a 'TermLike'.
    --
    --    The result sort must be hooked to the builtin normalized sort (e.g. @Set@).
    asInternal ::
        InternalVariable variable =>
        SmtMetadataTools Attribute.Symbol ->
        Sort ->
        normalized Key (TermLike variable) ->
        TermLike variable

    -- | Transforms a @TermLike@ representation into a @NormalizedOrBottom@.
    --
    --     The term may become bottom if we had conflicts between elements that were
    --     not detected before, e.g.
    --
    --     @
    --     concat({1}, concat(X:Set, {1}))
    --     concat(elem(Y:Int), concat({1}, elem(Y:Int)))
    --     concat(X:Set, concat({1}, X:Set))
    --     @
    toNormalized ::
        HasCallStack =>
        Ord variable =>
        Hashable variable =>
        TermLike variable ->
        NormalizedOrBottom normalized variable

    -- | Pattern match on a 'TermLike' to return a 'normalized'.
    --
    --    @matchBuiltin@ returns 'Nothing' if the 'TermLike' does not wrap a
    --    'normalized' value.
    matchBuiltin ::
        TermLike variable ->
        Maybe (normalized Key (TermLike variable))

    simplifiedAttributeValue ::
        Value normalized (TermLike variable) -> Attribute.Simplified

instance TermWrapper NormalizedMap where
    asInternalBuiltin tools builtinAcSort builtinAcChild =
        InternalAc
            { builtinAcSort
            , builtinAcUnit = Builtin.lookupSymbolUnit tools builtinAcSort
            , builtinAcElement = Builtin.lookupSymbolElement tools builtinAcSort
            , builtinAcConcat = Builtin.lookupSymbolConcat tools builtinAcSort
            , builtinAcChild
            }

    asInternal tools sort child =
        TermLike.mkInternalMap (asInternalBuiltin tools sort child)

    matchBuiltin (InternalMap_ internalMap) =
        Just (builtinAcChild internalMap)
    matchBuiltin _ = Nothing
    toNormalized (InternalMap_ InternalAc{builtinAcChild}) =
        maybe Bottom Normalized (renormalize builtinAcChild)
    toNormalized (App_ symbol args)
        | Map.isSymbolUnit symbol =
            case args of
                [] -> (Normalized . wrapAc) emptyNormalizedAc
                _ -> Builtin.wrongArity "MAP.unit"
        | Map.isSymbolElement symbol =
            case args of
                [key, value]
                    | Just key' <- TermLike.retractKey key ->
                        (Normalized . wrapAc)
                            NormalizedAc
                                { elementsWithVariables = []
                                , concreteElements =
                                    HashMap.singleton key' (MapValue value)
                                , opaque = []
                                }
                    | otherwise ->
                        (Normalized . wrapAc)
                            NormalizedAc
                                { elementsWithVariables = [MapElement (key, value)]
                                , concreteElements = HashMap.empty
                                , opaque = []
                                }
                _ -> Builtin.wrongArity "MAP.element"
        | Map.isSymbolConcat symbol =
            case args of
                [map1, map2] -> toNormalized map1 <> toNormalized map2
                _ -> Builtin.wrongArity "MAP.concat"
    toNormalized patt =
        (Normalized . wrapAc)
            NormalizedAc
                { elementsWithVariables = []
                , concreteElements = HashMap.empty
                , opaque = [patt]
                }

    simplifiedAttributeValue = TermLike.simplifiedAttribute . getMapValue

instance TermWrapper NormalizedSet where
    asInternalBuiltin tools builtinAcSort builtinAcChild =
        InternalAc
            { builtinAcSort
            , builtinAcUnit = Builtin.lookupSymbolUnit tools builtinAcSort
            , builtinAcElement = Builtin.lookupSymbolElement tools builtinAcSort
            , builtinAcConcat = Builtin.lookupSymbolConcat tools builtinAcSort
            , builtinAcChild
            }

    asInternal tools sort child =
        TermLike.mkInternalSet (asInternalBuiltin tools sort child)

    matchBuiltin (InternalSet_ internalSet) =
        Just (builtinAcChild internalSet)
    matchBuiltin _ = Nothing
    toNormalized (InternalSet_ InternalAc{builtinAcChild}) =
        maybe Bottom Normalized (renormalize builtinAcChild)
    toNormalized (App_ symbol args)
        | Set.isSymbolUnit symbol =
            case args of
                [] -> (Normalized . wrapAc) emptyNormalizedAc
                _ -> Builtin.wrongArity "SET.unit"
        | Set.isSymbolElement symbol =
            case args of
                [elem1]
                    | Just elem1' <- TermLike.retractKey elem1 ->
                        (Normalized . wrapAc)
                            NormalizedAc
                                { elementsWithVariables = []
                                , concreteElements = HashMap.singleton elem1' SetValue
                                , opaque = []
                                }
                    | otherwise ->
                        (Normalized . wrapAc)
                            NormalizedAc
                                { elementsWithVariables = [SetElement elem1]
                                , concreteElements = HashMap.empty
                                , opaque = []
                                }
                _ -> Builtin.wrongArity "SET.element"
        | Set.isSymbolConcat symbol =
            case args of
                [set1, set2] -> toNormalized set1 <> toNormalized set2
                _ -> Builtin.wrongArity "SET.concat"
    toNormalized patt =
        (Normalized . wrapAc)
            emptyNormalizedAc{opaque = [patt]}

    simplifiedAttributeValue SetValue = mempty

{- | Wrapper for terms that keeps the "concrete" vs "with variable" distinction
after converting @TermLike Concrete@ to @TermLike variable@.
-}
data ConcreteOrWithVariable normalized variable
    = ConcretePat (TermLike variable, Value normalized (TermLike variable))
    | WithVariablePat (TermLike variable, Value normalized (TermLike variable))

instance
    From
        (ConcreteOrWithVariable normalized variable)
        (TermLike variable, Value normalized (TermLike variable))
    where
    from =
        \case
            ConcretePat result -> result
            WithVariablePat result -> result

fromConcreteOrWithVariable ::
    ConcreteOrWithVariable normalized variable ->
    (TermLike variable, Value normalized (TermLike variable))
fromConcreteOrWithVariable = from

-- | Particularizes @NormalizedAc@ to the most common types.
type TermNormalizedAc normalized variable =
    normalized Key (TermLike variable)

{- | A normalized representation of an associative-commutative structure that
also allows bottom values.
-}
data NormalizedOrBottom collection variable
    = Normalized (TermNormalizedAc collection variable)
    | Bottom
    deriving stock (GHC.Generic)

deriving anyclass instance
    Binary (TermNormalizedAc collection variable) =>
    Binary (NormalizedOrBottom collection variable)

deriving stock instance
    Eq (TermNormalizedAc collection variable) =>
    Eq (NormalizedOrBottom collection variable)

deriving stock instance
    Ord (TermNormalizedAc collection variable) =>
    Ord (NormalizedOrBottom collection variable)

deriving stock instance
    Show (TermNormalizedAc collection variable) =>
    Show (NormalizedOrBottom collection variable)

instance SOP.Generic (NormalizedOrBottom collection variable)

instance SOP.HasDatatypeInfo (NormalizedOrBottom collection variable)

instance
    Debug (collection Key (TermLike variable)) =>
    Debug (NormalizedOrBottom collection variable)

instance
    ( Debug (collection Key (TermLike variable))
    , Diff (collection Key (TermLike variable))
    ) =>
    Diff (NormalizedOrBottom collection variable)

-- | The semigroup defined by the `concat` operation.
instance
    (Ord variable, TermWrapper normalized, Hashable variable) =>
    Semigroup (NormalizedOrBottom normalized variable)
    where
    Bottom <> _ = Bottom
    _ <> Bottom = Bottom
    Normalized normalized1 <> Normalized normalized2 =
        maybe Bottom Normalized $ concatNormalized normalized1 normalized2

concatNormalized ::
    forall normalized variable.
    (TermWrapper normalized, Ord variable, Hashable variable) =>
    normalized Key (TermLike variable) ->
    normalized Key (TermLike variable) ->
    Maybe (normalized Key (TermLike variable))
concatNormalized normalized1 normalized2 = do
    Monad.guard disjointConcreteElements
    abstract' <-
        updateAbstractElements $ onBoth (++) elementsWithVariables
    let concrete' = onBoth HashMap.union concreteElements
        opaque' = Data.List.sort $ onBoth (++) opaque
    renormalize $
        wrapAc
            NormalizedAc
                { elementsWithVariables = abstract'
                , concreteElements = concrete'
                , opaque = opaque'
                }
  where
    onBoth ::
        (a -> a -> r) ->
        ( NormalizedAc
            normalized
            Key
            (TermLike variable) ->
          a
        ) ->
        r
    onBoth f g = on f (g . unwrapAc) normalized1 normalized2
    disjointConcreteElements =
        null $ onBoth HashMap.intersection concreteElements

{- | Take a (possibly de-normalized) internal representation to its normal form.

@renormalize@ returns 'Nothing' if the normal form is @\\bottom@.

First, abstract elements are converted to concrete elements wherever
possible. Second, any opaque terms which are actually builtins are combined with
the top-level term, so that the normal form never has children which are
internal representations themselves; this "flattening" step also recurses to
@renormalize@ the previously-opaque children.
-}
renormalize ::
    (TermWrapper normalized, Ord variable, Hashable variable) =>
    normalized Key (TermLike variable) ->
    Maybe (normalized Key (TermLike variable))
renormalize = normalizeAbstractElements >=> flattenOpaque

-- | Insert the @key@-@value@ pair if it is missing from the 'Map'.
insertMissing ::
    Ord key =>
    Hashable key =>
    key ->
    value ->
    HashMap key value ->
    Maybe (HashMap key value)
insertMissing k v m
    | HashMap.member k m = Nothing
    | otherwise = Just (HashMap.insert k v m)

{- | Insert the new concrete elements into the 'Map'.

Return 'Nothing' if there are any duplicate keys.
-}
updateConcreteElements ::
    HashMap Key value ->
    [(Key, value)] ->
    Maybe (HashMap Key value)
updateConcreteElements elems newElems =
    foldrM (uncurry insertMissing) elems newElems

{- | Sort the abstract elements.

Return 'Nothing' if there are any duplicate keys.
-}
updateAbstractElements ::
    (AcWrapper collection, Ord child, Hashable child) =>
    [Element collection child] ->
    Maybe [Element collection child]
updateAbstractElements elements =
    fmap (map wrapElement . HashMap.toList) $
        foldrM (uncurry insertMissing) HashMap.empty $
            map unwrapElement elements

{- | Make any abstract elements into concrete elements if possible.

Return 'Nothing' if there are any duplicate (concrete or abstract) keys.
-}
normalizeAbstractElements ::
    (TermWrapper normalized, Ord variable) =>
    normalized Key (TermLike variable) ->
    Maybe (normalized Key (TermLike variable))
normalizeAbstractElements (unwrapAc -> normalized) = do
    concrete' <- updateConcreteElements concrete newConcrete
    abstract' <- updateAbstractElements newAbstract
    return $
        wrapAc
            NormalizedAc
                { elementsWithVariables = abstract'
                , concreteElements = concrete'
                , opaque = opaque normalized
                }
  where
    abstract = elementsWithVariables normalized
    concrete = concreteElements normalized
    (newConcrete, newAbstract) =
        partitionEithers (extractConcreteElement <$> abstract)

{- | 'Left' if the element's key can be concretized, or 'Right' if it
 remains abstract.
-}
extractConcreteElement ::
    AcWrapper collection =>
    Element collection (TermLike variable) ->
    Either
        (Key, Value collection (TermLike variable))
        (Element collection (TermLike variable))
extractConcreteElement element =
    maybe (Right element) (Left . flip (,) value) (TermLike.retractKey key)
  where
    (key, value) = unwrapElement element

{- | Move any @normalized@ children to the top-level by concatenation.

@flattenOpaque@ recursively flattens the children of children, and so on.
-}
flattenOpaque ::
    (TermWrapper normalized, Ord variable, Hashable variable) =>
    normalized Key (TermLike variable) ->
    Maybe (normalized Key (TermLike variable))
flattenOpaque (unwrapAc -> normalized) = do
    let NormalizedAc{opaque} = normalized
        (builtin, opaque') = partitionEithers (extractBuiltin <$> opaque)
        transparent = wrapAc normalized{opaque = opaque'}
    foldrM concatNormalized transparent builtin
  where
    extractBuiltin termLike =
        maybe (Right termLike) Left (matchBuiltin termLike)

-- | The monoid defined by the `concat` and `unit` operations.
instance
    (Ord variable, TermWrapper normalized, Hashable variable) =>
    Monoid (NormalizedOrBottom normalized variable)
    where
    mempty = Normalized $ wrapAc emptyNormalizedAc

{- | Computes the union of two maps if they are disjoint. Returns @Nothing@
otherwise.
-}
addToMapDisjoint ::
    (Ord a, Foldable t, Hashable a) =>
    HashMap a b ->
    t (a, b) ->
    Maybe (HashMap a b)
addToMapDisjoint = Monad.foldM addElementDisjoint

addElementDisjoint ::
    Ord a =>
    Hashable a =>
    HashMap a b ->
    (a, b) ->
    Maybe (HashMap a b)
addElementDisjoint existing (key, value) =
    if key `HashMap.member` existing
        then Nothing
        else return (HashMap.insert key value existing)

-- | Given a @NormalizedAc@, returns it as a function result.
returnAc ::
    ( InternalVariable variable
    , TermWrapper normalized
    ) =>
    Sort ->
    TermNormalizedAc normalized variable ->
    Simplifier (Pattern variable)
returnAc resultSort ac = do
    tools <- Simplifier.askMetadataTools
    asInternal tools resultSort ac
        & Pattern.fromTermLike
        & return

{- | Converts an Ac of concrete elements to a @NormalizedAc@ and returns it
as a function result.
-}
returnConcreteAc ::
    ( InternalVariable variable
    , TermWrapper normalized
    ) =>
    Sort ->
    HashMap Key (Value normalized (TermLike variable)) ->
    Simplifier (Pattern variable)
returnConcreteAc resultSort concrete =
    (returnAc resultSort . wrapAc)
        NormalizedAc
            { elementsWithVariables = []
            , concreteElements = concrete
            , opaque = []
            }

{- | The same as 'asInternal', but for ac structures made only of concrete
elements.
-}
asInternalConcrete ::
    (InternalVariable variable, TermWrapper normalized) =>
    SmtMetadataTools Attribute.Symbol ->
    Sort ->
    HashMap Key (Value normalized (TermLike variable)) ->
    TermLike variable
asInternalConcrete tools sort1 concreteAc =
    asInternal tools sort1 $
        wrapAc
            NormalizedAc
                { elementsWithVariables = []
                , concreteElements = concreteAc
                , opaque = []
                }

elementListAsInternal ::
    forall normalized variable.
    (InternalVariable variable, TermWrapper normalized) =>
    SmtMetadataTools Attribute.Symbol ->
    Sort ->
    [(TermLike variable, Value normalized (TermLike variable))] ->
    Maybe (TermLike variable)
elementListAsInternal tools sort1 terms =
    asInternal tools sort1 . wrapAc
        <$> elementListAsNormalized terms

elementListAsNormalized ::
    forall normalized variable.
    (InternalVariable variable, TermWrapper normalized) =>
    [(TermLike variable, Value normalized (TermLike variable))] ->
    Maybe (NormalizedAc normalized Key (TermLike variable))
elementListAsNormalized terms = do
    let (withVariables, concrete) = splitVariableConcrete terms
    _checkDisjoinVariables <- disjointMap withVariables
    concreteAc <- disjointMap concrete
    return
        NormalizedAc
            { elementsWithVariables = wrapElement <$> withVariables
            , concreteElements = concreteAc
            , opaque = []
            }

-- | Render a 'NormalizedAc' as an extended domain value pattern.
asPattern ::
    ( InternalVariable variable
    , TermWrapper normalized
    ) =>
    SmtMetadataTools Attribute.Symbol ->
    Sort ->
    TermNormalizedAc normalized variable ->
    Pattern variable
asPattern tools resultSort =
    Pattern.fromTermLike . asInternal tools resultSort

{- | Evaluates a concatenation of two AC structures represented as
NormalizedOrBottom, providind the result in the form of a function result.
-}
evalConcatNormalizedOrBottom ::
    forall normalized variable.
    ( InternalVariable variable
    , TermWrapper normalized
    ) =>
    Sort ->
    NormalizedOrBottom normalized variable ->
    NormalizedOrBottom normalized variable ->
    MaybeT Simplifier (Pattern variable)
evalConcatNormalizedOrBottom resultSort Bottom _ =
    return (Pattern.bottomOf resultSort)
evalConcatNormalizedOrBottom resultSort _ Bottom =
    return (Pattern.bottomOf resultSort)
evalConcatNormalizedOrBottom
    resultSort
    (Normalized normalized1)
    (Normalized normalized2) =
        maybe (return $ Pattern.bottomOf resultSort) (lift . returnAc resultSort) $
            concatNormalized normalized1 normalized2

disjointMap :: Ord a => Hashable a => [(a, b)] -> Maybe (HashMap a b)
disjointMap = addToMapDisjoint HashMap.empty

splitVariableConcrete ::
    forall variable a.
    [(TermLike variable, a)] ->
    ([(TermLike variable, a)], [(Key, a)])
splitVariableConcrete terms =
    partitionEithers (map toConcreteEither terms)
  where
    toConcreteEither ::
        (TermLike variable, a) ->
        Either (TermLike variable, a) (Key, a)
    toConcreteEither (term, a) =
        case TermLike.retractKey term of
            Nothing -> Left (term, a)
            Just result -> Right (result, a)

{- | Simplify the conjunction or equality of two Ac domain values in their
normalized form.

When it is used for simplifying equality, one should separately solve the
case ⊥ = ⊥. One should also throw away the term in the returned pattern.

The domain values are assumed to have the same sort, but this is not checked. If
multiple sorts are hooked to the same builtin domain, the verifier should
reject the definition.
-}
unifyEqualsNormalized ::
    forall normalized unifier.
    ( Traversable (Value normalized)
    , TermWrapper normalized
    , MonadUnify unifier
    ) =>
    HasCallStack =>
    SmtMetadataTools Attribute.Symbol ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    ( TermLike RewritingVariableName ->
      TermLike RewritingVariableName ->
      unifier (Pattern RewritingVariableName)
    ) ->
    InternalAc Key normalized (TermLike RewritingVariableName) ->
    InternalAc Key normalized (TermLike RewritingVariableName) ->
    UnifyEqualsNormAc normalized RewritingVariableName ->
    unifier (Pattern RewritingVariableName)
unifyEqualsNormalized
    tools
    first
    second
    unifyEqualsChildren
    normalized1
    normalized2
    unifyData =
        do
            let InternalAc{builtinAcChild = firstNormalized} =
                    normalized1
                InternalAc{builtinAcChild = secondNormalized} =
                    normalized2

            unifierNormalized <-
                unifyEqualsNormalizedAc
                    tools
                    first
                    second
                    unifyEqualsChildren
                    firstNormalized
                    secondNormalized
                    unifyData
            let unifierNormalizedTerm ::
                    TermNormalizedAc normalized RewritingVariableName
                unifierCondition :: Condition RewritingVariableName
                (unifierNormalizedTerm, unifierCondition) =
                    Conditional.splitTerm unifierNormalized
                normalizedTerm :: TermLike RewritingVariableName
                normalizedTerm = asInternal tools sort1 unifierNormalizedTerm

            -- TODO(virgil): Check whether this normalization is still needed
            -- (the normalizedTerm may already be re-normalized) and remove it if not.
            renormalized <- normalize1 normalizedTerm

            let unifierTerm :: TermLike RewritingVariableName
                unifierTerm = markSimplified $ asInternal tools sort1 renormalized

                markSimplified =
                    TermLike.setSimplified
                        ( foldMap TermLike.simplifiedAttribute opaque
                            <> foldMap TermLike.simplifiedAttribute abstractKeys
                            <> foldMap simplifiedAttributeValue abstractValues
                            <> foldMap simplifiedAttributeValue concreteValues
                        )
                  where
                    unwrapped = unwrapAc renormalized
                    NormalizedAc{opaque} = unwrapped
                    (abstractKeys, abstractValues) =
                        (unzip . map unwrapElement)
                            (elementsWithVariables unwrapped)
                    (_, concreteValues) =
                        (unzip . HashMap.toList)
                            (concreteElements unwrapped)

            return (unifierTerm `Pattern.withCondition` unifierCondition)
      where
        sort1 = termLikeSort first

        normalize1 ::
            HasCallStack =>
            InternalVariable variable =>
            TermLike variable ->
            unifier (TermNormalizedAc normalized variable)
        normalize1 patt =
            case toNormalized patt of
                Bottom ->
                    debugUnifyBottomAndReturnBottom
                        "Duplicated elements in normalization."
                        first
                        second
                Normalized n -> return n

data UnifyEqualsElementListsData normalized = UnifyEqualsElementListsData
    { -- Given normalized data norm1, norm2, norm1 - norm2 and norm2 - norm1
      allElements1, allElements2 :: ![ConcreteOrWithVariable normalized RewritingVariableName]
    , -- Is Just v if v is the sole opaque in norm1 - norm2, else Nothing
      maybeVar :: !(Maybe (ElementVariable RewritingVariableName))
    }

data UnifyEqualsNormAc normalized variable
    = UnifyEqualsElementLists !(UnifyEqualsElementListsData normalized)
    | UnifyOpaqueVar !(UnifyOpVarResult variable)

matchUnifyEqualsNormalizedAc ::
    forall normalized.
    TermWrapper normalized =>
    SmtMetadataTools Attribute.Symbol ->
    InternalAc Key normalized (TermLike RewritingVariableName) ->
    InternalAc Key normalized (TermLike RewritingVariableName) ->
    Maybe (UnifyEqualsNormAc normalized RewritingVariableName)
matchUnifyEqualsNormalizedAc
    tools
    normalized1
    normalized2 =
        case (opaqueDifference1, opaqueDifference2) of
            ([], []) ->
                Just $
                    UnifyEqualsElementLists $
                        UnifyEqualsElementListsData
                            allElements1
                            allElements2
                            Nothing
            ([ElemVar_ v1], _)
                | null opaqueDifference2 ->
                    Just $
                        UnifyEqualsElementLists $
                            UnifyEqualsElementListsData
                                allElements1
                                allElements2
                                (Just v1)
                | null allElements1 ->
                    fmap UnifyOpaqueVar $
                        matchUnifyOpaqueVariable'
                            v1
                            allElements2
                            opaqueDifference2
            _ -> Nothing
      where
        matchUnifyOpaqueVariable' =
            matchUnifyOpaqueVariable tools

        listToMap :: Hashable a => [a] -> HashMap a Int
        listToMap keys = HashMap.fromListWith (+) [(k, 1) | k <- keys]
        mapToList :: HashMap a Int -> [a]
        mapToList =
            HashMap.foldrWithKey
                (\key count result -> replicate count key ++ result)
                []

        NormalizedAc
            { elementsWithVariables = preElementsWithVariables1
            , concreteElements = concreteElements1
            , opaque = opaque1
            } =
                unwrapAc firstNormalized
        NormalizedAc
            { elementsWithVariables = preElementsWithVariables2
            , concreteElements = concreteElements2
            , opaque = opaque2
            } =
                unwrapAc secondNormalized

        InternalAc{builtinAcChild = firstNormalized} =
            normalized1
        InternalAc{builtinAcChild = secondNormalized} =
            normalized2

        opaque1Map = listToMap opaque1
        opaque2Map = listToMap opaque2

        elementsWithVariables1 = unwrapElement <$> preElementsWithVariables1
        elementsWithVariables2 = unwrapElement <$> preElementsWithVariables2
        elementsWithVariables1Map = HashMap.fromList elementsWithVariables1
        elementsWithVariables2Map = HashMap.fromList elementsWithVariables2

        commonElements =
            HashMap.intersectionWith
                (,)
                concreteElements1
                concreteElements2
        commonVariables =
            HashMap.intersectionWith
                (,)
                elementsWithVariables1Map
                elementsWithVariables2Map

        -- Duplicates must be kept in case any of the opaque terms turns out to be
        -- non-empty, in which case one of the terms is bottom, which
        -- means that the unification result is bottom.
        commonOpaqueMap = HashMap.intersectionWith max opaque1Map opaque2Map

        commonOpaqueKeys = HashMap.keysSet commonOpaqueMap

        elementDifference1 =
            HashMap.toList (HashMap.difference concreteElements1 commonElements)
        elementDifference2 =
            HashMap.toList (HashMap.difference concreteElements2 commonElements)
        elementVariableDifference1 =
            HashMap.toList (HashMap.difference elementsWithVariables1Map commonVariables)
        elementVariableDifference2 =
            HashMap.toList (HashMap.difference elementsWithVariables2Map commonVariables)
        opaqueDifference1 =
            mapToList (withoutKeys opaque1Map commonOpaqueKeys)
        opaqueDifference2 =
            mapToList (withoutKeys opaque2Map commonOpaqueKeys)

        withoutKeys ::
            Hashable k =>
            HashMap k v ->
            HashSet k ->
            HashMap k v
        withoutKeys hmap hset = hmap `HashMap.difference` (HashSet.toMap hset)

        allElements1 =
            map WithVariablePat elementVariableDifference1
                ++ map toConcretePat elementDifference1
        allElements2 =
            map WithVariablePat elementVariableDifference2
                ++ map toConcretePat elementDifference2

        toConcretePat ::
            (Key, Value normalized (TermLike RewritingVariableName)) ->
            ConcreteOrWithVariable
                normalized
                RewritingVariableName
        toConcretePat (a, b) =
            ConcretePat (from @Key @(TermLike RewritingVariableName) a, b)

{- | Unifies two AC structs represented as @NormalizedAc@.

Currently allows at most one opaque term in the two arguments taken together.
-}
unifyEqualsNormalizedAc ::
    forall normalized unifier.
    ( Traversable (Value normalized)
    , TermWrapper normalized
    , MonadUnify unifier
    ) =>
    SmtMetadataTools Attribute.Symbol ->
    TermLike RewritingVariableName ->
    TermLike RewritingVariableName ->
    ( TermLike RewritingVariableName ->
      TermLike RewritingVariableName ->
      unifier (Pattern RewritingVariableName)
    ) ->
    TermNormalizedAc normalized RewritingVariableName ->
    TermNormalizedAc normalized RewritingVariableName ->
    UnifyEqualsNormAc normalized RewritingVariableName ->
    unifier
        ( Conditional
            RewritingVariableName
            (TermNormalizedAc normalized RewritingVariableName)
        )
unifyEqualsNormalizedAc
    tools
    first
    second
    unifyEqualsChildren
    normalized1
    normalized2
    unifyData =
        do
            (simpleUnifier, opaques) <- case unifyData of
                UnifyEqualsElementLists unifyData' ->
                    unifyEqualsElementLists'
                        allElements1
                        allElements2
                        maybeVar
                  where
                    UnifyEqualsElementListsData{allElements1, allElements2, maybeVar} = unifyData'
                UnifyOpaqueVar unifyData' ->
                    unifyOpaqueVariable
                        bottomWithExplanation
                        unifyEqualsChildren
                        unifyData'

            let (unifiedElements, unifierCondition) =
                    Conditional.splitTerm simpleUnifier
            do
                -- unifier monad
                -- unify the parts not sent to unifyEqualsNormalizedElements.
                (commonElementsTerms, commonElementsCondition) <-
                    unifyElementList (HashMap.toList commonElements)
                (commonVariablesTerms, commonVariablesCondition) <-
                    unifyElementList (HashMap.toList commonVariables)

                -- simplify results so that things like inj applications that
                -- may have been broken into smaller pieces are being put
                -- back together.
                unifiedSimplified <- mapM simplifyPair unifiedElements
                opaquesSimplified <- mapM simplify opaques

                buildResultFromUnifiers
                    bottomWithExplanation
                    commonElementsTerms
                    commonVariablesTerms
                    commonOpaque
                    unifiedSimplified
                    opaquesSimplified
                    [ unifierCondition
                    , commonElementsCondition
                    , commonVariablesCondition
                    ]
      where
        listToMap :: Hashable a => [a] -> HashMap a Int
        listToMap keys = HashMap.fromListWith (+) [(k, 1) | k <- keys]
        mapToList :: HashMap a Int -> [a]
        mapToList =
            HashMap.foldrWithKey
                (\key count result -> replicate count key ++ result)
                []

        bottomWithExplanation :: Text -> unifier a
        bottomWithExplanation explanation =
            debugUnifyBottomAndReturnBottom explanation first second

        unifyEqualsElementLists' =
            unifyEqualsElementLists
                tools
                first
                second
                unifyEqualsChildren

        NormalizedAc
            { elementsWithVariables = preElementsWithVariables1
            , concreteElements = concreteElements1
            , opaque = opaque1
            } =
                unwrapAc normalized1
        NormalizedAc
            { elementsWithVariables = preElementsWithVariables2
            , concreteElements = concreteElements2
            , opaque = opaque2
            } =
                unwrapAc normalized2

        opaque1Map = listToMap opaque1
        opaque2Map = listToMap opaque2

        elementsWithVariables1 = unwrapElement <$> preElementsWithVariables1
        elementsWithVariables2 = unwrapElement <$> preElementsWithVariables2
        elementsWithVariables1Map = HashMap.fromList elementsWithVariables1
        elementsWithVariables2Map = HashMap.fromList elementsWithVariables2

        commonElements =
            HashMap.intersectionWith
                (,)
                concreteElements1
                concreteElements2
        commonVariables =
            HashMap.intersectionWith
                (,)
                elementsWithVariables1Map
                elementsWithVariables2Map

        -- Duplicates must be kept in case any of the opaque terms turns out to be
        -- non-empty, in which case one of the terms is bottom, which
        -- means that the unification result is bottom.
        commonOpaqueMap = HashMap.intersectionWith max opaque1Map opaque2Map

        commonOpaque = mapToList commonOpaqueMap

        unifyElementList ::
            forall key.
            [ ( key
              , ( Value normalized (TermLike RewritingVariableName)
                , Value normalized (TermLike RewritingVariableName)
                )
              )
            ] ->
            unifier
                ( [(key, Value normalized (TermLike RewritingVariableName))]
                , Condition RewritingVariableName
                )
        unifyElementList elements = do
            result <- mapM (unifyCommonElements unifyEqualsChildren) elements
            let terms :: [(key, Value normalized (TermLike RewritingVariableName))]
                predicates :: [Condition RewritingVariableName]
                (terms, predicates) = unzip (map Conditional.splitTerm result)
                predicate :: Condition RewritingVariableName
                predicate =
                    List.foldl'
                        andCondition
                        Condition.top
                        predicates

            return (terms, predicate)

        simplify ::
            TermLike RewritingVariableName ->
            unifier (Pattern RewritingVariableName)
        simplify term =
            mapLogicT liftSimplifier (simplifyPatternScatter SideCondition.topTODO (Pattern.fromTermLike term))
                & lowerLogicT

        simplifyPair ::
            ( TermLike RewritingVariableName
            , Value normalized (TermLike RewritingVariableName)
            ) ->
            unifier
                ( Conditional
                    RewritingVariableName
                    ( TermLike RewritingVariableName
                    , Value normalized (TermLike RewritingVariableName)
                    )
                )
        simplifyPair (key, value) = do
            simplifiedKey <- simplifyTermLike' key
            let (keyTerm, keyCondition) = Conditional.splitTerm simplifiedKey
            simplifiedValue <- traverse simplifyTermLike' value
            let splitSimplifiedValue ::
                    Value
                        normalized
                        ( TermLike RewritingVariableName
                        , Condition RewritingVariableName
                        )
                splitSimplifiedValue =
                    fmap Conditional.splitTerm simplifiedValue
                simplifiedValueTerm ::
                    Value normalized (TermLike RewritingVariableName)
                simplifiedValueTerm = fmap fst splitSimplifiedValue
                simplifiedValueConditions ::
                    Value normalized (Condition RewritingVariableName)
                simplifiedValueConditions = fmap snd splitSimplifiedValue
                simplifiedValueCondition :: Condition RewritingVariableName
                simplifiedValueCondition =
                    foldr
                        andCondition
                        Condition.top
                        simplifiedValueConditions
            return
                ( (keyTerm, simplifiedValueTerm)
                    `withCondition` keyCondition
                    `andCondition` simplifiedValueCondition
                )
          where
            -- TODO: can this be rewritten?
            simplifyTermLike' ::
                TermLike RewritingVariableName ->
                unifier (Pattern RewritingVariableName)
            simplifyTermLike' term =
                mapLogicT liftSimplifier (simplifyPatternScatter SideCondition.topTODO (Pattern.fromTermLike term))
                    & lowerLogicT

buildResultFromUnifiers ::
    forall normalized unifier variable.
    ( Monad unifier
    , InternalVariable variable
    , TermWrapper normalized
    ) =>
    (forall result. Text -> unifier result) ->
    [(Key, Value normalized (TermLike variable))] ->
    [(TermLike variable, Value normalized (TermLike variable))] ->
    [TermLike variable] ->
    [ Conditional
        variable
        (TermLike variable, Value normalized (TermLike variable))
    ] ->
    [Pattern variable] ->
    [Condition variable] ->
    unifier (Conditional variable (TermNormalizedAc normalized variable))
buildResultFromUnifiers
    bottomWithExplanation
    commonElementsTerms
    commonVariablesTerms
    commonOpaque
    unifiedElementsSimplified
    opaquesSimplified
    predicates =
        do
            -- unifier monad
            let almostResultTerms ::
                    [ ( TermLike variable
                      , Value normalized (TermLike variable)
                      )
                    ]
                almostResultConditions :: [Condition variable]
                (almostResultTerms, almostResultConditions) =
                    unzip (map Conditional.splitTerm unifiedElementsSimplified)
                (withVariableTerms, concreteTerms) =
                    splitVariableConcrete almostResultTerms

                (opaquesTerms, opaquesConditions) =
                    unzip (map Conditional.splitTerm opaquesSimplified)
                opaquesNormalized :: NormalizedOrBottom normalized variable
                opaquesNormalized = foldMap toNormalized opaquesTerms

            NormalizedAc
                { elementsWithVariables = preOpaquesElementsWithVariables
                , concreteElements = opaquesConcreteTerms
                , opaque = opaquesOpaque
                } <- case opaquesNormalized of
                Bottom ->
                    bottomWithExplanation "Duplicated elements after unification."
                Normalized result -> return (unwrapAc result)
            let opaquesElementsWithVariables =
                    unwrapElement <$> preOpaquesElementsWithVariables

            -- Add back all the common objects that were removed before
            -- unification.
            withVariableMap <-
                addAllDisjoint
                    bottomWithExplanation
                    HashMap.empty
                    ( commonVariablesTerms
                        ++ withVariableTerms
                        ++ opaquesElementsWithVariables
                    )
            concreteMap <-
                addAllDisjoint
                    bottomWithExplanation
                    HashMap.empty
                    ( commonElementsTerms
                        ++ concreteTerms
                        ++ HashMap.toList opaquesConcreteTerms
                    )
            let allOpaque = Data.List.sort (commonOpaque ++ opaquesOpaque)
                -- Merge all unification predicates.
                predicate =
                    List.foldl'
                        andCondition
                        Condition.top
                        (almostResultConditions ++ opaquesConditions ++ predicates)
                result ::
                    Conditional
                        variable
                        (normalized Key (TermLike variable))
                result =
                    wrapAc
                        NormalizedAc
                            { elementsWithVariables =
                                wrapElement <$> HashMap.toList withVariableMap
                            , concreteElements = concreteMap
                            , opaque = allOpaque
                            }
                        `Conditional.withCondition` predicate

            return result

addAllDisjoint ::
    (Monad unifier, Ord a, Hashable a) =>
    (forall result. Text -> unifier result) ->
    HashMap a b ->
    [(a, b)] ->
    unifier (HashMap a b)
addAllDisjoint bottomWithExplanation existing elements =
    case addToMapDisjoint existing elements of
        Nothing ->
            bottomWithExplanation "Duplicated elements after AC unification."
        Just result -> return result

unifyCommonElements ::
    forall key normalized unifier variable.
    ( AcWrapper normalized
    , MonadUnify unifier
    , InternalVariable variable
    , Traversable (Value normalized)
    ) =>
    (TermLike variable -> TermLike variable -> unifier (Pattern variable)) ->
    ( key
    , ( Value normalized (TermLike variable)
      , Value normalized (TermLike variable)
      )
    ) ->
    unifier
        ( Conditional
            variable
            (key, Value normalized (TermLike variable))
        )
unifyCommonElements
    unifier
    (key, (firstValue, secondValue)) =
        do
            valuesUnifier <- unifyWrappedValues unifier firstValue secondValue
            let (valuesTerm, valueCondition) = Conditional.splitTerm valuesUnifier

            return ((key, valuesTerm) `withCondition` valueCondition)

unifyWrappedValues ::
    forall normalized unifier variable.
    ( AcWrapper normalized
    , MonadUnify unifier
    , InternalVariable variable
    , Traversable (Value normalized)
    ) =>
    (TermLike variable -> TermLike variable -> unifier (Pattern variable)) ->
    Value normalized (TermLike variable) ->
    Value normalized (TermLike variable) ->
    unifier
        (Conditional variable (Value normalized (TermLike variable)))
unifyWrappedValues unifier firstValue secondValue = do
    let aligned = alignValues firstValue secondValue
    unifiedValues <- traverse (uncurry unifier) aligned
    let splitValues ::
            Value normalized (TermLike variable, Condition variable)
        splitValues = fmap Pattern.splitTerm unifiedValues
        valueUnifierTerm :: Value normalized (TermLike variable)
        valueUnifierTerm = fmap fst splitValues
        valueConditions :: Value normalized (Condition variable)
        valueConditions = fmap snd splitValues
        valueUnifierCondition :: Condition variable
        valueUnifierCondition =
            foldr Conditional.andCondition Condition.top valueConditions

    return (valueUnifierTerm `withCondition` valueUnifierCondition)

{- | Unifies two ac structures given their representation as a list of
@ConcreteOrWithVariable@, with the first structure being allowed an additional
opaque chunk (e.g. a variable) that will be sent to the unifier function
together with some part of the second structure.

The keys of the two structures are assumend to be disjoint.
-}
unifyEqualsElementLists ::
    forall normalized variable unifier.
    ( InternalVariable variable
    , MonadUnify unifier
    , TermWrapper normalized
    , Traversable (Value normalized)
    ) =>
    SmtMetadataTools Attribute.Symbol ->
    TermLike variable ->
    TermLike variable ->
    -- | unifier function
    (TermLike variable -> TermLike variable -> unifier (Pattern variable)) ->
    -- | First structure elements
    [ConcreteOrWithVariable normalized variable] ->
    -- | Second structure elements
    [ConcreteOrWithVariable normalized variable] ->
    -- | Opaque element variable of the first structure
    Maybe (ElementVariable variable) ->
    unifier
        ( Conditional
            variable
            [(TermLike variable, Value normalized (TermLike variable))]
        , [TermLike variable]
        )
unifyEqualsElementLists
    _tools
    first
    second
    unifyEqualsChildren
    firstElements
    secondElements
    Nothing
        | length firstElements /= length secondElements =
            -- Neither the first, not the second ac structure include an opaque term, so
            -- the listed elements form the two structures.
            --
            -- Since the two lists have different counts, their structures can
            -- never unify.
            debugUnifyBottomAndReturnBottom
                "Cannot unify ac structures with different sizes."
                first
                second
        | otherwise = do
            (result, remainder1, remainder2) <-
                unifyWithPermutations firstElements secondElements
            -- The second structure does not include an opaque term so there is nothing
            -- to match whatever is left in remainder1. This should have been caught by
            -- the "length" check above so, most likely, this can be an assertion.
            unless
                (null remainder1)
                (remainderError firstElements secondElements remainder1)
            -- The first structure does not include an opaque term so there is nothing
            -- to match whatever is left in remainder2. This should have been caught by
            -- the "length" check above so, most likely, this can be an assertion.
            unless
                (null remainder2)
                (remainderError firstElements secondElements remainder2)

            return (result, [])
      where
        unifyWithPermutations ::
            -- First structure elements
            [ConcreteOrWithVariable normalized variable] ->
            -- Second structure elements
            [ConcreteOrWithVariable normalized variable] ->
            unifier
                ( Conditional
                    variable
                    [ ( TermLike variable
                      , Value normalized (TermLike variable)
                      )
                    ]
                , [ConcreteOrWithVariable normalized variable]
                , [ConcreteOrWithVariable normalized variable]
                )
        unifyWithPermutations =
            unifyEqualsElementPermutations
                (unifyEqualsConcreteOrWithVariable unifyEqualsChildren)
        remainderError = nonEmptyRemainderError first second
unifyEqualsElementLists
    tools
    first
    second
    unifyEqualsChildren
    firstElements
    secondElements
    (Just opaqueElemVar)
        | length firstElements > length secondElements =
            -- The second structure does not include an opaque term, so all the
            -- elements in the first structure must be matched by elements in the second
            -- one. Since we don't have enough, we return bottom.
            debugUnifyBottomAndReturnBottom
                "Cannot unify ac structures with different sizes."
                first
                second
        | otherwise = do
            (unifier, remainder1, remainder2) <-
                unifyWithPermutations firstElements secondElements
            -- The second structure does not include an opaque term so there is nothing
            -- to match whatever is left in remainder1. This should have been caught by
            -- the "length" check above so, most likely, this can be an assertion.
            unless
                (null remainder1)
                (remainderError firstElements secondElements remainder1)

            let remainder2Terms = map fromConcreteOrWithVariable remainder2

            case elementListAsInternal
                tools
                (sameSort (termLikeSort first) (termLikeSort second))
                remainder2Terms of
                Nothing ->
                    debugUnifyBottomAndReturnBottom
                        "Duplicated element in unification results"
                        first
                        second
                Just remainderTerm
                    | TermLike.isFunctionPattern remainderTerm -> do
                        opaqueUnifier <-
                            unifyEqualsChildren
                                (mkElemVar opaqueElemVar)
                                remainderTerm
                        let (opaqueTerm, opaqueCondition) =
                                Pattern.splitTerm opaqueUnifier
                            result = unifier `andCondition` opaqueCondition
                        return (result, [opaqueTerm])
                _ ->
                    error . show . Pretty.vsep $
                        [ "Unification case that should be handled somewhere else: \
                          \attempting normalized unification with \
                          \non-function maps could lead to infinite loops."
                        , Pretty.indent 2 "first="
                        , Pretty.indent 4 (unparse first)
                        , Pretty.indent 2 "second="
                        , Pretty.indent 4 (unparse second)
                        ]
      where
        unifyWithPermutations =
            unifyEqualsElementPermutations
                (unifyEqualsConcreteOrWithVariable unifyEqualsChildren)
        remainderError = nonEmptyRemainderError first second

data NoCheckUnifyOpaqueChildrenData variable = NoCheckUnifyOpaqueChildrenData
    { -- Given normalized data norm1, norm2, the sole opaque variable in norm1 - norm2
      v1 :: !(TermLike.ElementVariable variable)
    , -- The term to unify against v1
      second :: !(TermLike variable)
    }

data UnifyOpVarResult variable
    = NoCheckUnifyOpaqueChildren !(NoCheckUnifyOpaqueChildrenData variable)
    | BottomWithExplanation

matchUnifyOpaqueVariable ::
    ( TermWrapper normalized
    , InternalVariable variable
    ) =>
    SmtMetadataTools Attribute.Symbol ->
    TermLike.ElementVariable variable ->
    [ConcreteOrWithVariable normalized variable] ->
    [TermLike variable] ->
    Maybe (UnifyOpVarResult variable)
matchUnifyOpaqueVariable _ v1 [] [second@(ElemVar_ _)] =
    Just $ NoCheckUnifyOpaqueChildren NoCheckUnifyOpaqueChildrenData{v1, second}
matchUnifyOpaqueVariable
    tools
    v1
    concreteOrVariableTerms
    opaqueTerms =
        case elementListAsNormalized pairs of
            Nothing -> Just BottomWithExplanation
            Just elementTerm ->
                let secondTerm =
                        asInternal
                            tools
                            sort
                            ( wrapAc
                                elementTerm{opaque = opaqueTerms}
                            )
                 in if TermLike.isFunctionPattern secondTerm
                        then
                            Just $
                                NoCheckUnifyOpaqueChildren $
                                    NoCheckUnifyOpaqueChildrenData v1 secondTerm
                        else Nothing
      where
        sort = variableSort v1
        pairs = map fromConcreteOrWithVariable concreteOrVariableTerms

unifyOpaqueVariable ::
    ( MonadUnify unifier
    , InternalVariable variable
    ) =>
    (forall a. Text -> unifier a) ->
    -- | unifier function
    (TermLike variable -> TermLike variable -> unifier (Pattern variable)) ->
    UnifyOpVarResult variable ->
    unifier
        ( Conditional
            variable
            [(TermLike variable, Value normalized (TermLike variable))]
        , [TermLike variable]
        )
unifyOpaqueVariable
    bottomWithExplanation
    unifyChildren
    unifyData =
        case unifyData of
            NoCheckUnifyOpaqueChildren unifyData' ->
                noCheckUnifyOpaqueChildren unifyChildren v1 second
              where
                NoCheckUnifyOpaqueChildrenData{v1, second} = unifyData'
            _ ->
                bottomWithExplanation
                    "Duplicated element in unification results"

noCheckUnifyOpaqueChildren ::
    ( MonadUnify unifier
    , InternalVariable variable
    ) =>
    (TermLike variable -> TermLike variable -> unifier (Pattern variable)) ->
    TermLike.ElementVariable variable ->
    TermLike variable ->
    unifier
        ( Conditional
            variable
            [(TermLike variable, Value normalized (TermLike variable))]
        , [TermLike variable]
        )
noCheckUnifyOpaqueChildren unifyChildren v1 second = do
    unifier <- unifyChildren (mkElemVar v1) second
    let (opaque, predicate) = Conditional.splitTerm unifier
    return ([] `Conditional.withCondition` predicate, [opaque])

{- | Unifies two patterns represented as @ConcreteOrWithVariable@, making sure
that a concrete pattern (if any) is sent on the first position of the unify
function.

We prefer having a concrete pattern on the first position because the
unifier prefers returning it when it does not know what to use, e.g.

@
unify 10 (f A) ==> 10 and (10 == f A)
unify (f A) 10 ==> (f A) and (10 == f A)
@

and it would probably be more useful to have a concrete term as the
unification term. Also, tests are easier to write.
-}
unifyEqualsConcreteOrWithVariable ::
    ( AcWrapper normalized
    , MonadUnify unifier
    , Traversable (Value normalized)
    , InternalVariable variable
    ) =>
    (TermLike variable -> TermLike variable -> unifier (Pattern variable)) ->
    ConcreteOrWithVariable normalized variable ->
    ConcreteOrWithVariable normalized variable ->
    unifier
        ( Conditional
            variable
            (TermLike variable, Value normalized (TermLike variable))
        )
unifyEqualsConcreteOrWithVariable
    unifier
    (ConcretePat concrete1)
    (ConcretePat concrete2) =
        unifyEqualsPair unifier concrete1 concrete2
unifyEqualsConcreteOrWithVariable
    unifier
    (ConcretePat concrete1)
    (WithVariablePat withVariable2) =
        unifyEqualsPair unifier concrete1 withVariable2
unifyEqualsConcreteOrWithVariable
    unifier
    (WithVariablePat withVariable)
    (ConcretePat concrete2) =
        unifyEqualsPair unifier concrete2 withVariable
unifyEqualsConcreteOrWithVariable
    unifier
    (WithVariablePat withVariable)
    (WithVariablePat withVariable2) =
        unifyEqualsPair unifier withVariable withVariable2

unifyEqualsPair ::
    forall normalized unifier variable.
    ( AcWrapper normalized
    , MonadUnify unifier
    , InternalVariable variable
    , Traversable (Value normalized)
    ) =>
    (TermLike variable -> TermLike variable -> unifier (Pattern variable)) ->
    (TermLike variable, Value normalized (TermLike variable)) ->
    (TermLike variable, Value normalized (TermLike variable)) ->
    unifier
        ( Conditional
            variable
            (TermLike variable, Value normalized (TermLike variable))
        )
unifyEqualsPair
    unifier
    (firstKey, firstValue)
    (secondKey, secondValue) =
        do
            keyUnifier <- unifier firstKey secondKey

            valueUnifier <- unifyWrappedValues unifier firstValue secondValue

            let valueUnifierTerm :: Value normalized (TermLike variable)
                valueUnifierCondition :: Condition variable
                (valueUnifierTerm, valueUnifierCondition) =
                    Conditional.splitTerm valueUnifier

            let (keyUnifierTerm, keyUnifierCondition) = Pattern.splitTerm keyUnifier

            return
                ( (keyUnifierTerm, valueUnifierTerm)
                    `withCondition` keyUnifierCondition
                    `andCondition` valueUnifierCondition
                )

{- | Given a unify function and two lists of unifiable things, returns
all possible ways to unify disjoint pairs of the two that use all items
from at least one of the lists.

Also returns the non-unified part os the lists (one of the two will be empty).
-}
unifyEqualsElementPermutations ::
    ( Alternative unifier
    , Monad unifier
    , InternalVariable variable
    ) =>
    (a -> b -> unifier (Conditional variable c)) ->
    [a] ->
    [b] ->
    unifier
        ( Conditional variable [c]
        , [a]
        , [b]
        )
unifyEqualsElementPermutations unifier firsts seconds = do
    (unifiers, remainderFirst, remainderSecond) <-
        if length firsts < length seconds
            then do
                (u, r) <-
                    kPermutationsBacktracking (flip unifier) seconds firsts
                return (u, [], r)
            else do
                (u, r) <-
                    kPermutationsBacktracking unifier firsts seconds
                return (u, r, [])
    let (terms, predicates) = unzip (map Conditional.splitTerm unifiers)
        predicate = foldr andCondition Condition.top predicates
    return (terms `withCondition` predicate, remainderFirst, remainderSecond)

{- | Given two lists generates k-permutation pairings and merges them using the
provided merge function.

k is the length of the second list, which means that, if the @[b]@ list is
longer than the @[a]@ list, this will not generate any k-permutations.
However, it will probably take a long time to generate nothing.

If the pairing function fails (i.e. returns empty), the entire function will
stop exploring future branches that would include the given pair.

Note that this does not mean that we won't try a failing pair again with a
different set of previous choices, so this function could be optimized to
at least cache pairing results.
-}
kPermutationsBacktracking ::
    forall a b c m.
    Alternative m =>
    (a -> b -> m c) ->
    [a] ->
    [b] ->
    m ([c], [a])
kPermutationsBacktracking _ first [] = pure ([], first)
kPermutationsBacktracking transform firstList secondList =
    generateKPermutationsWorker firstList [] secondList
  where
    generateKPermutationsWorker :: [a] -> [a] -> [b] -> m ([c], [a])
    generateKPermutationsWorker _ (_ : _) [] =
        error "Unexpected non-empty skipped list with empty pair opportunities"
    generateKPermutationsWorker [] [] [] = pure ([], [])
    generateKPermutationsWorker [] _ _ = empty
    generateKPermutationsWorker first [] [] = pure ([], first)
    generateKPermutationsWorker (first : firsts) skipped (second : seconds) =
        pickElement <|> skipElement
      where
        pickElement =
            addToFirst
                <$> transform first second
                <*> generateKPermutationsWorker (skipped ++ firsts) [] seconds

        addToFirst :: x -> ([x], y) -> ([x], y)
        addToFirst x (xs, y) = (x : xs, y)

        skipElement =
            generateKPermutationsWorker
                firsts
                (first : skipped)
                (second : seconds)

nonEmptyRemainderError ::
    forall a normalized variable.
    ( HasCallStack
    , InternalVariable variable
    , AcWrapper normalized
    ) =>
    TermLike variable ->
    TermLike variable ->
    [ConcreteOrWithVariable normalized variable] ->
    [ConcreteOrWithVariable normalized variable] ->
    [ConcreteOrWithVariable normalized variable] ->
    a
nonEmptyRemainderError first second input1 input2 remainder =
    (error . unlines)
        [ "Unexpected unused elements, should have been caught"
        , "by checks above:"
        , "first=" ++ unparseToString first
        , "second=" ++ unparseToString second
        , "input1=" ++ unlines (map unparseWrapped input1)
        , "input2=" ++ unlines (map unparseWrapped input2)
        , "remainder=" ++ unlines (map unparseWrapped remainder)
        ]
  where
    unparseWrapped =
        Unparser.renderDefault . unparsePair . fromConcreteOrWithVariable
    unparsePair = unparseElement unparse . wrapElement

-- | Wrapper for giving names to arguments.
newtype UnitSymbol = UnitSymbol {getUnitSymbol :: Symbol}

-- | Wrapper for giving names to arguments.
newtype ConcatSymbol = ConcatSymbol {getConcatSymbol :: Symbol}

-- | Wrapper for giving names to arguments.
newtype ConcreteElements variable = ConcreteElements {getConcreteElements :: [TermLike variable]}

-- | Wrapper for giving names to arguments.
newtype VariableElements variable = VariableElements {getVariableElements :: [TermLike variable]}

-- | Wrapper for giving names to arguments.
newtype Opaque variable = Opaque {getOpaque :: [TermLike variable]}
