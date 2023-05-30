{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Internal.SideCondition (
    SideCondition, -- Constructor not exported on purpose
    addAssumption,
    addAssumptions,
    assumeTrue,
    constructReplacements,
    fromConditionWithReplacements,
    fromPredicateWithReplacements,
    addConditionWithReplacements,
    mapVariables,
    top,
    topTODO,
    toPredicate,
    toRepresentation,
    replaceTerm,
    cannotReplaceTerm,
    replacePredicate,
    cannotReplacePredicate,
    assumeDefined,
    isDefined,
    fromDefinedTerms,
    generateNormalizedAcs,
    simplifyConjunctionByAssumption,
    cacheSimplifiedFunctions,
    isSimplifiedFunction,
    fromSimplifiedFunctions,
) where

import Changed
import Control.Lens qualified as Lens
import Control.Monad.State.Strict (
    StateT,
    runStateT,
 )
import Control.Monad.State.Strict qualified as State
import Data.Bifunctor qualified as Bifunctor
import Data.Binary (Binary (..), Get, Put)
import Data.Functor.Foldable qualified as Recursive
import Data.Generics.Product (
    field,
 )
import Data.HashMap.Strict (
    HashMap,
 )
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (
    HashSet,
 )
import Data.HashSet qualified as HashSet
import Data.List (
    sortOn,
 )
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.Defined qualified as Attribute
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables (..),
 )
import Kore.Attribute.Synthetic (
    synthesize,
 )
import Kore.Internal.Condition (
    Condition,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.InternalList (
    InternalList (..),
 )
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import Kore.Internal.MultiAnd qualified as MultiAnd
import Kore.Internal.NormalizedAc (
    AcWrapper (..),
    InternalAc (..),
    NormalizedAc (..),
    PairWiseElements (..),
    emptyNormalizedAc,
    generatePairWiseElements,
    getConcreteKeysOfAc,
    getConcreteValuesOfAc,
    getSymbolicKeysOfAc,
    getSymbolicValuesOfAc,
    unwrapElement,
    pattern AcPair,
 )
import Kore.Internal.Predicate (
    Predicate,
    makeFalsePredicate,
    makeTruePredicate,
    pattern PredicateEquals,
    pattern PredicateExists,
    pattern PredicateForall,
    pattern PredicateNot,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition.SideCondition as SideCondition
import Kore.Internal.Symbol (
    Symbol,
    isConstructor,
    isFunction,
    isTotal,
 )
import Kore.Internal.TermLike (
    Application,
    Key,
    TermLike,
    isConstructorLike,
    pattern App_,
    pattern Exists_,
    pattern Forall_,
    pattern Inj_,
    pattern InternalBool_,
    pattern InternalBytes_,
    pattern InternalInt_,
    pattern InternalList_,
    pattern InternalMap_,
    pattern InternalSet_,
    pattern InternalString_,
    pattern Mu_,
    pattern Nu_,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Internal.Variable (
    InternalVariable,
 )
import Kore.Syntax.Variable
import Kore.Unparser (
    Unparse (..),
 )
import Pair
import Partial (
    Partial (..),
    getPartial,
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified
import SQL qualified

{- | Side condition used in the evaluation context.

It contains:
* a predicate assumed to be true
* a table of term replacements which is used when simplifying terms
* a set of terms which are assumed to be defined
* a set of terms with function application at the top which are known to have been
  simplified as much as possible during the current rewrite step

Warning! When simplifying a pattern, extra care should be taken that the
'SideCondition' sent to the simplifier isn't created from the same 'Condition'
which is sent to be simplified.

The predicate is usually used to remove infeasible branches, but it may also
be used for other purposes, say, to remove redundant parts of the result predicate.
-}
data SideCondition variable = SideCondition
    { assumedTrue :: !(MultiAnd (Predicate variable))
    , replacementsTermLike ::
        !(HashMap (TermLike variable) (TermLike variable))
    , replacementsPredicate ::
        !(HashMap (Predicate variable) (Predicate variable))
    , definedTerms :: !(HashSet (TermLike variable))
    , simplifiedFunctions ::
        !(HashSet (Application Symbol (TermLike variable)))
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance (Eq variable, Hashable variable, Binary variable) =>
    Binary (SideCondition variable) where
    put SideCondition
            { assumedTrue
            , replacementsTermLike
            , replacementsPredicate
            , definedTerms
            , simplifiedFunctions
            } = put assumedTrue >>
                putMap replacementsTermLike >>
                putMap replacementsPredicate >>
                putSet definedTerms >>
                putSet simplifiedFunctions
    get =
        SideCondition
            <$> get <*> getMap  <*> getMap  <*> getSet  <*> getSet

putSet :: (Hashable e, Binary e) => HashSet e -> Put
putSet = put . HashSet.toList

getSet :: (Hashable e, Binary e) => Get (HashSet e)
getSet = fmap HashSet.fromList get

putMap :: (Binary k, Hashable k, Binary v) => HashMap k v -> Put
putMap = put . HashMap.toList

getMap :: (Binary k, Hashable k, Binary v) => Get (HashMap k v)
getMap = fmap HashMap.fromList get

instance InternalVariable variable => SQL.Column (SideCondition variable) where
    defineColumn = SQL.defineTextColumn
    toColumn = SQL.toColumn . Pretty.renderText . Pretty.layoutOneLine . pretty

instance Ord variable => HasFreeVariables (SideCondition variable) variable where
    freeVariables sideCondition@(SideCondition _ _ _ _ _) =
        freeVariables assumedTrue
            <> foldMap freeVariables definedTerms
      where
        SideCondition
            { assumedTrue
            , definedTerms
            } = sideCondition

instance InternalVariable variable => Pretty (SideCondition variable) where
    pretty
        SideCondition
            { assumedTrue
            , definedTerms
            , replacementsTermLike
            , replacementsPredicate
            } =
            (Pretty.vsep . concat)
                [ ["Assumed true condition:"]
                , (pure . Pretty.indent 4 . Pretty.pretty)
                    (Predicate.fromMultiAnd assumedTrue)
                , ["TermLike replacements:"]
                , HashMap.foldlWithKey' (acc unparse) [] replacementsTermLike
                , ["Predicate replacements:"]
                , HashMap.foldlWithKey'
                    (acc Pretty.pretty)
                    []
                    replacementsPredicate
                , ["Assumed to be defined:"]
                , unparse <$> HashSet.toList definedTerms
                ]
          where
            acc showFunc result key value =
                result
                    <> [ Pretty.indent 4 "Key:"
                       , Pretty.indent 6 $ showFunc key
                       , Pretty.indent 4 "Value:"
                       , Pretty.indent 6 $ showFunc value
                       ]

instance From (SideCondition variable) (MultiAnd (Predicate variable)) where
    from condition@(SideCondition _ _ _ _ _) = assumedTrue condition
    {-# INLINE from #-}

instance
    InternalVariable variable =>
    From (SideCondition variable) (Predicate variable)
    where
    from = toPredicate
    {-# INLINE from #-}

instance
    InternalVariable variable =>
    From (SideCondition variable) (Condition variable)
    where
    from = Condition.fromPredicate . toPredicate
    {-# INLINE from #-}

{- | Smart constructor for creating a 'SideCondition' by just assuming
 a conjunction of predicates to be true.
-}
assumeTrue ::
    MultiAnd (Predicate variable) ->
    SideCondition variable
assumeTrue assumedTrue =
    SideCondition
        { assumedTrue
        , replacementsTermLike = HashMap.empty
        , replacementsPredicate = HashMap.empty
        , definedTerms = HashSet.empty
        , simplifiedFunctions = HashSet.empty
        }

{- | Assumes a single 'Predicate' to be true in the context of another
 'SideCondition'.
 Does not modify the replacement table or the set of defined terms.
-}
addAssumption ::
    Ord variable =>
    Predicate variable ->
    SideCondition variable ->
    SideCondition variable
addAssumption predicate =
    addAssumptions (MultiAnd.singleton predicate)

{- | Assumes a conjunction of 'Predicate's to be true in the context
 of another 'SideCondition'.
 Does not modify the replacement table or the set of defined terms.
-}
addAssumptions ::
    Ord variable =>
    MultiAnd (Predicate variable) ->
    SideCondition variable ->
    SideCondition variable
addAssumptions predicates sideCondition =
    sideCondition
        { assumedTrue =
            predicates <> assumedTrue sideCondition
        }

areIncludedIn ::
    Eq variable =>
    Foldable f =>
    f (Predicate variable) ->
    SideCondition variable ->
    Bool
areIncludedIn predicates sideCondition =
    all (flip isIncludedIn sideCondition) predicates

isIncludedIn ::
    Eq variable =>
    Predicate variable ->
    SideCondition variable ->
    Bool
isIncludedIn predicate SideCondition{assumedTrue} =
    predicate `elem` assumedTrue

{- | Assumes a 'Condition' to be true in the context of another
'SideCondition' and recalculates the term replacements table
from the combined predicate.
-}
addConditionWithReplacements ::
    InternalVariable variable =>
    Condition variable ->
    SideCondition variable ->
    SideCondition variable
addConditionWithReplacements
    (from @(Condition _) @(MultiAnd _) -> newCondition)
    sideCondition
        | newCondition `areIncludedIn` sideCondition =
            sideCondition
        | otherwise =
            let combinedConditions = oldCondition <> newCondition
                (assumedTrue, assumptions) =
                    simplifyConjunctionByAssumption combinedConditions
                        & extract
                Assumptions replacementsTermLike replacementsPredicate = assumptions
             in SideCondition
                    { assumedTrue
                    , replacementsTermLike
                    , replacementsPredicate
                    , definedTerms
                    , simplifiedFunctions
                    }
      where
        SideCondition
            { assumedTrue = oldCondition
            , definedTerms
            , simplifiedFunctions
            } = sideCondition

{- | Smart constructor for creating a 'SideCondition' by just constructing
 the replacement table from a conjunction of predicates.
-}
constructReplacements ::
    InternalVariable variable =>
    MultiAnd (Predicate variable) ->
    SideCondition variable
constructReplacements predicates =
    let (_, assumptions) =
            simplifyConjunctionByAssumption predicates
                & extract
        Assumptions replacementsTermLike replacementsPredicate =
            assumptions
     in SideCondition
            { assumedTrue = MultiAnd.top
            , replacementsTermLike
            , replacementsPredicate
            , definedTerms = HashSet.empty
            , simplifiedFunctions = HashSet.empty
            }

{- | Smart constructor for creating a `SideCondition` by assuming
 a 'Condition' to be true and building its term replacement table.
-}
fromConditionWithReplacements ::
    InternalVariable variable =>
    Condition variable ->
    SideCondition variable
fromConditionWithReplacements (from -> predicates) =
    constructReplacements predicates
        & Lens.set (field @"assumedTrue") predicates

{- | Smart constructor for creating a `SideCondition` by assuming
 a 'Predicate' to be true and building its term replacement table.
-}
fromPredicateWithReplacements ::
    InternalVariable variable =>
    Predicate variable ->
    SideCondition variable
fromPredicateWithReplacements (from -> predicates) =
    constructReplacements predicates
        & Lens.set (field @"assumedTrue") predicates

top :: InternalVariable variable => SideCondition variable
top =
    SideCondition
        { assumedTrue = MultiAnd.top
        , replacementsTermLike = mempty
        , replacementsPredicate = mempty
        , definedTerms = mempty
        , simplifiedFunctions = mempty
        }

-- TODO(ana.pantilie): Should we look into removing this?

-- | A 'top' 'Condition' for refactoring which should eventually be removed.
topTODO :: InternalVariable variable => SideCondition variable
topTODO = top

toPredicate ::
    InternalVariable variable =>
    SideCondition variable ->
    Predicate variable
toPredicate condition@(SideCondition _ _ _ _ _) =
    Predicate.makeAndPredicate
        assumedTruePredicate
        definedPredicate
  where
    SideCondition{assumedTrue, definedTerms} = condition
    assumedTruePredicate = Predicate.fromMultiAnd assumedTrue
    definedPredicate =
        definedTerms
            & HashSet.toList
            & fmap Predicate.makeCeilPredicate
            & MultiAnd.make
            & Predicate.fromMultiAnd

mapVariables ::
    (InternalVariable variable1, InternalVariable variable2) =>
    AdjSomeVariableName (variable1 -> variable2) ->
    SideCondition variable1 ->
    SideCondition variable2
mapVariables adj condition@(SideCondition _ _ _ _ _) =
    let assumedTrue' =
            MultiAnd.map (Predicate.mapVariables adj) assumedTrue
        replacementsTermLike' =
            mapKeysAndValues (TermLike.mapVariables adj) replacementsTermLike
        replacementsPredicate' =
            mapKeysAndValues (Predicate.mapVariables adj) replacementsPredicate
        definedTerms' =
            HashSet.map (TermLike.mapVariables adj) definedTerms
        simplifiedFunctions' =
            (HashSet.map . fmap) (TermLike.mapVariables adj) simplifiedFunctions
     in SideCondition
            { assumedTrue = assumedTrue'
            , replacementsTermLike = replacementsTermLike'
            , replacementsPredicate = replacementsPredicate'
            , definedTerms = definedTerms'
            , simplifiedFunctions = simplifiedFunctions'
            }
  where
    SideCondition
        { assumedTrue
        , replacementsTermLike
        , replacementsPredicate
        , definedTerms
        , simplifiedFunctions
        } = condition

-- | Utility function for mapping on the keys and values of a 'HashMap'.
mapKeysAndValues ::
    Hashable b =>
    (a -> b) ->
    HashMap a a ->
    HashMap b b
mapKeysAndValues f hashMap =
    HashMap.fromList $
        Bifunctor.bimap f f
            <$> HashMap.toList hashMap

fromDefinedTerms ::
    InternalVariable variable =>
    HashSet (TermLike variable) ->
    SideCondition variable
fromDefinedTerms definedTerms =
    top{definedTerms}

{- | Prepares the 'SideCondition' for storing in the term attributes.
 Any metadata information used only in particular places during execution
 is erased.
-}
toRepresentation ::
    InternalVariable variable =>
    Binary variable =>
    SideCondition variable ->
    SideCondition.Representation
toRepresentation sideCondition =
    let sideCondition' =
            sideCondition{simplifiedFunctions = HashSet.empty}
     in mkRepresentation sideCondition'

-- | Looks up the term in the table of replacements.
replaceTerm ::
    InternalVariable variable =>
    SideCondition variable ->
    TermLike variable ->
    Maybe (TermLike variable)
replaceTerm SideCondition{replacementsTermLike} original =
    HashMap.lookup original replacementsTermLike

{- | If the term isn't a key in the table of replacements
then it cannot be replaced.
-}
cannotReplaceTerm ::
    InternalVariable variable =>
    SideCondition variable ->
    TermLike variable ->
    Bool
cannotReplaceTerm SideCondition{replacementsTermLike} term =
    HashMap.lookup term replacementsTermLike & isNothing

-- | Looks up the predicate in the table of replacements.
replacePredicate ::
    InternalVariable variable =>
    SideCondition variable ->
    Predicate variable ->
    Maybe (Predicate variable)
replacePredicate SideCondition{replacementsPredicate} original =
    HashMap.lookup original replacementsPredicate

{- | If the predicate isn't a key in the table of replacements
then it cannot be replaced.
-}
cannotReplacePredicate ::
    InternalVariable variable =>
    SideCondition variable ->
    Predicate variable ->
    Bool
cannotReplacePredicate SideCondition{replacementsPredicate} term =
    HashMap.lookup term replacementsPredicate & isNothing

data Assumptions variable = Assumptions
    { termLikeMap :: HashMap (TermLike variable) (TermLike variable)
    , predicateMap :: HashMap (Predicate variable) (Predicate variable)
    }
    deriving stock (Eq, GHC.Generic, Show)

{- | Simplify the conjunction of 'Predicate' clauses by assuming each is true.
The conjunction is simplified by the identity:
@
A ∧ P(A) = A ∧ P(⊤)
@
-}
simplifyConjunctionByAssumption ::
    forall variable.
    InternalVariable variable =>
    MultiAnd (Predicate variable) ->
    Changed
        ( MultiAnd (Predicate variable)
        , Assumptions variable
        )
simplifyConjunctionByAssumption (toList -> andPredicates) =
    (fmap . Bifunctor.first) MultiAnd.make $
        flip runStateT (Assumptions HashMap.empty HashMap.empty) $
            for (sortBySize andPredicates) $
                \original -> do
                    result <- applyAssumptions original
                    assume result
                    return result
  where
    -- Sorting by size ensures that every clause is considered before any clause
    -- which could contain it, because the containing clause is necessarily
    -- larger.
    sortBySize :: [Predicate variable] -> [Predicate variable]
    sortBySize = sortOn predSize

    size :: TermLike variable -> Int
    size =
        Recursive.fold $ \(_ :< termLikeF) -> 1 + sum termLikeF

    predSize :: Predicate variable -> Int
    predSize =
        Recursive.fold $ \(_ :< predF) ->
            case predF of
                Predicate.CeilF ceil_ -> 1 + sum (size <$> ceil_)
                Predicate.EqualsF equals_ -> 1 + sum (size <$> equals_)
                Predicate.FloorF floor_ -> 1 + sum (size <$> floor_)
                Predicate.InF in_ -> 1 + sum (size <$> in_)
                _ -> 1 + sum predF

    assume ::
        Predicate variable ->
        StateT (Assumptions variable) Changed ()
    assume predicate =
        State.modify' (assumeEqualTerms . assumePredicate)
      where
        assumePredicate =
            case predicate of
                PredicateNot notChild ->
                    -- Infer that the predicate is \bottom.
                    Lens.over (field @"predicateMap") $
                        HashMap.insert notChild makeFalsePredicate
                _ ->
                    -- Infer that the predicate is \top.
                    Lens.over (field @"predicateMap") $
                        HashMap.insert predicate makeTruePredicate
        assumeEqualTerms =
            case predicate of
                PredicateEquals t1 t2 ->
                    case retractLocalFunction (Predicate.makeEqualsPredicate t1 t2) of
                        Just (Pair t1' t2') ->
                            Lens.over (field @"termLikeMap") $
                                HashMap.insert t1' t2'
                        _ -> id
                _ -> id

    applyAssumptions ::
        Predicate variable ->
        StateT (Assumptions variable) Changed (Predicate variable)
    applyAssumptions replaceIn = do
        assumptions <- State.get
        lift (applyAssumptionsWorker assumptions replaceIn)

    applyAssumptionsWorker ::
        Assumptions variable ->
        Predicate variable ->
        Changed (Predicate variable)
    applyAssumptionsWorker assumptions original
        | Just result <- HashMap.lookup original (predicateMap assumptions) = Changed result
        | HashMap.null (termLikeMap assumptions')
        , HashMap.null (predicateMap assumptions') =
            Unchanged original
        | otherwise =
            case replaceIn of
                Predicate.CeilF ceil_ ->
                    Predicate.CeilF <$> traverse applyTermAssumptions ceil_
                Predicate.FloorF floor_ ->
                    Predicate.FloorF <$> traverse applyTermAssumptions floor_
                Predicate.EqualsF equals_ ->
                    Predicate.EqualsF <$> traverse applyTermAssumptions equals_
                Predicate.InF in_ ->
                    Predicate.InF <$> traverse applyTermAssumptions in_
                _ -> traverse applyPredicateAssumptions replaceIn
                & getChanged
                -- The next line ensures that if the result is Unchanged, any allocation
                -- performed while computing that result is collected.
                & maybe (Unchanged original) (Changed . synthesize)
      where
        _ :< replaceIn = Recursive.project original

        applyTermAssumptions =
            applyAssumptionsWorkerTerm (termLikeMap assumptions')
        applyPredicateAssumptions = applyAssumptionsWorker assumptions'

        assumptions'
            | PredicateExists var _ <- original = restrictAssumptions (inject var)
            | PredicateForall var _ <- original = restrictAssumptions (inject var)
            | otherwise = assumptions

        restrictAssumptions Variable{variableName} =
            Lens.over
                (field @"termLikeMap")
                (HashMap.filterWithKey (\term _ -> wouldNotCaptureTerm term))
                $ Lens.over
                    (field @"predicateMap")
                    (HashMap.filterWithKey (\predicate _ -> wouldNotCapture predicate))
                    assumptions
          where
            wouldNotCapture = not . Predicate.hasFreeVariable variableName
            wouldNotCaptureTerm = not . TermLike.hasFreeVariable variableName

    applyAssumptionsWorkerTerm ::
        HashMap (TermLike variable) (TermLike variable) ->
        TermLike variable ->
        Changed (TermLike variable)
    applyAssumptionsWorkerTerm assumptions original
        | Just result <- HashMap.lookup original assumptions = Changed result
        | HashMap.null assumptions' = Unchanged original
        | otherwise =
            traverse (applyAssumptionsWorkerTerm assumptions') replaceIn
                & getChanged
                -- The next line ensures that if the result is Unchanged, any allocation
                -- performed while computing that result is collected.
                & maybe (Unchanged original) (Changed . synthesize)
      where
        _ :< replaceIn = Recursive.project original

        assumptions'
            | Exists_ _ var _ <- original = restrictAssumptions (inject var)
            | Forall_ _ var _ <- original = restrictAssumptions (inject var)
            | Mu_ var _ <- original = restrictAssumptions (inject var)
            | Nu_ var _ <- original = restrictAssumptions (inject var)
            | otherwise = assumptions

        restrictAssumptions Variable{variableName} =
            HashMap.filterWithKey
                (\termLike _ -> wouldNotCapture termLike)
                assumptions
          where
            wouldNotCapture = not . TermLike.hasFreeVariable variableName

{- | Get a local function definition from a 'TermLike'.
A local function definition is a predicate that we can use to evaluate a
function locally (based on the side conditions) when none of the functions
global definitions (axioms) apply. We are looking for a 'TermLike' of the form
@
\equals(f(...), C(...))
@
where @f@ is a function and @C@ is a constructor, sort injection or builtin.
@retractLocalFunction@ will match an @\equals@ predicate with its arguments
in either order, but the function pattern is always returned first in the
'Pair'.
-}
retractLocalFunction ::
    Predicate variable ->
    Maybe (Pair (TermLike variable))
retractLocalFunction =
    \case
        PredicateEquals term1 term2 -> go term1 term2 <|> go term2 term1
        _ -> Nothing
  where
    go term1@(App_ symbol1 _) term2
        | isFunction symbol1 =
            -- TODO (thomas.tuegel): Add tests.
            case term2 of
                App_ symbol2 _
                    | isConstructor symbol2 -> Just (Pair term1 term2)
                Inj_ _ -> Just (Pair term1 term2)
                InternalInt_ _ -> Just (Pair term1 term2)
                InternalBytes_ _ _ -> Just (Pair term1 term2)
                InternalString_ _ -> Just (Pair term1 term2)
                InternalBool_ _ -> Just (Pair term1 term2)
                InternalList_ _ -> Just (Pair term1 term2)
                InternalMap_ _ -> Just (Pair term1 term2)
                InternalSet_ _ -> Just (Pair term1 term2)
                _ -> Nothing
    go _ _ = Nothing

{- | Assumes a 'TermLike' to be defined. If not always defined,
it will be stored in the `SideCondition` together with any subterms
resulting from the implication that the original term is defined.

For maps and sets: this will generate and store a minimal set
of sub-collections from which the definedness of any sub-collection
can be inferred.
-}
assumeDefined ::
    forall variable.
    InternalVariable variable =>
    TermLike variable ->
    Maybe (SideCondition variable)
assumeDefined =
    fmap fromDefinedTerms
        . getPartial
        . assumeDefinedWorker
  where
    assumeDefinedWorker ::
        TermLike variable ->
        Partial (HashSet (TermLike variable))
    assumeDefinedWorker term' =
        case term' of
            TermLike.And_ _ child1 child2 ->
                let result1 = assumeDefinedWorker child1
                    result2 = assumeDefinedWorker child2
                 in asSet term' <> result1 <> result2
            TermLike.App_ symbol children ->
                checkTotal symbol term'
                    <> foldMap assumeDefinedWorker children
            TermLike.Ceil_ _ _ child ->
                asSet term' <> assumeDefinedWorker child
            TermLike.InternalList_ internalList ->
                asSet term'
                    <> foldMap assumeDefinedWorker (internalListChild internalList)
            TermLike.InternalMap_ internalMap ->
                let definedElems =
                        getDefinedElementsOfAc internalMap
                    definedMaps =
                        generateNormalizedAcs internalMap
                            & HashSet.map TermLike.mkInternalMap
                            & Defined
                 in foldMap assumeDefinedWorker definedElems
                        <> definedMaps
            TermLike.InternalSet_ internalSet ->
                let definedElems =
                        getDefinedElementsOfAc internalSet
                    definedSets =
                        generateNormalizedAcs internalSet
                            & HashSet.map TermLike.mkInternalSet
                            & Defined
                 in foldMap assumeDefinedWorker definedElems
                        <> definedSets
            TermLike.Forall_ _ _ child ->
                asSet term' <> assumeDefinedWorker child
            TermLike.In_ _ _ child1 child2 ->
                let result1 = assumeDefinedWorker child1
                    result2 = assumeDefinedWorker child2
                 in asSet term' <> result1 <> result2
            TermLike.Bottom_ _ -> Bottom
            _ -> asSet term'
    asSet newTerm
        | isDefinedInternal newTerm = Defined HashSet.empty
        | otherwise = Defined $ HashSet.singleton newTerm
    checkTotal symbol newTerm
        | isTotal symbol = Defined HashSet.empty
        | otherwise = asSet newTerm

    getDefinedElementsOfAc ::
        forall normalized.
        AcWrapper normalized =>
        Foldable (Value normalized) =>
        InternalAc Key normalized (TermLike variable) ->
        HashSet (TermLike variable)
    getDefinedElementsOfAc (builtinAcChild -> normalizedAc) =
        let symbolicKeys = getSymbolicKeysOfAc normalizedAc
            values =
                getSymbolicValuesOfAc normalizedAc
                    <> getConcreteValuesOfAc normalizedAc
                    & foldMap toList
            opaqueElems = opaque (unwrapAc normalizedAc)
         in HashSet.fromList $
                symbolicKeys
                    <> opaqueElems
                    <> values

{- | Checks if a 'TermLike' is defined. It may always be defined,
or be defined in the context of the `SideCondition`.
-}
isDefined ::
    forall variable.
    InternalVariable variable =>
    SideCondition variable ->
    TermLike variable ->
    Bool
isDefined sideCondition@SideCondition{definedTerms} term =
    isDefinedInternal term
        || isTotalSymbol term
        || HashSet.member term definedTerms
        || isDefinedAc
  where
    isDefinedAc =
        case term of
            TermLike.InternalMap_ internalMap ->
                let subMaps =
                        generateNormalizedAcs internalMap
                            & HashSet.map TermLike.mkInternalMap
                 in isSymbolicSingleton internalMap
                        || subMaps `isNonEmptySubset` definedTerms
            TermLike.InternalSet_ internalSet ->
                let subSets =
                        generateNormalizedAcs internalSet
                            & HashSet.map TermLike.mkInternalSet
                 in isSymbolicSingleton internalSet
                        || subSets `isNonEmptySubset` definedTerms
            _ -> False

    isNonEmptySubset subset set
        | null subset = False
        | otherwise = all (`HashSet.member` set) subset

    isTotalSymbol (App_ symbol children)
        | isTotal symbol =
            all (isDefined sideCondition) children
    isTotalSymbol _ = False

    isSymbolicSingleton ::
        AcWrapper normalized =>
        Foldable (Value normalized) =>
        InternalAc Key normalized (TermLike variable) ->
        Bool
    isSymbolicSingleton InternalAc{builtinAcChild}
        | numberOfElements == 1 =
            all (isDefined sideCondition) symbolicKeys
                && all (isDefined sideCondition) opaqueElems
                && all (isDefined sideCondition) values
        | otherwise = False
      where
        symbolicKeys = getSymbolicKeysOfAc builtinAcChild
        concreteKeys = getConcreteKeysOfAc builtinAcChild
        opaqueElems = opaque . unwrapAc $ builtinAcChild
        values =
            getSymbolicValuesOfAc builtinAcChild
                <> getConcreteValuesOfAc builtinAcChild
                & foldMap toList
        numberOfElements =
            length symbolicKeys
                + length concreteKeys
                + length opaqueElems

{- | Generates the minimal set of defined collections
from which definedness of any sub collection can be inferred.
The resulting set will not contain the input collection itself.
-}
generateNormalizedAcs ::
    forall normalized variable.
    InternalVariable variable =>
    Ord (Element normalized (TermLike variable)) =>
    Ord (Value normalized (TermLike variable)) =>
    Ord (normalized Key (TermLike variable)) =>
    Hashable (Element normalized (TermLike variable)) =>
    Hashable (Value normalized (TermLike variable)) =>
    Hashable (normalized Key (TermLike variable)) =>
    AcWrapper normalized =>
    InternalAc Key normalized (TermLike variable) ->
    HashSet (InternalAc Key normalized (TermLike variable))
generateNormalizedAcs internalAc =
    if alwaysDefined internalAc
        then mempty
        else
            [ HashSet.map symbolicToAc symbolicPairs
            , HashSet.map concreteToAc concretePairs
            , HashSet.map opaqueToAc opaquePairs
            , HashSet.map symbolicConcreteToAc symbolicConcretePairs
            , HashSet.map symbolicOpaqueToAc symbolicOpaquePairs
            , HashSet.map concreteOpaqueToAc concreteOpaquePairs
            ]
                & fold
                & HashSet.filter (not . alwaysDefined)
  where
    alwaysDefined InternalAc{builtinAcChild = normalized} =
        all isConstructorLike children && null opaques
      where
        children =
            fst . unwrapElement <$> elementsWithVariables (unwrapAc normalized)
        opaques =
            opaque (unwrapAc normalized)
    InternalAc
        { builtinAcChild
        , builtinAcSort
        , builtinAcUnit
        , builtinAcConcat
        , builtinAcElement
        } = internalAc
    ~PairWiseElements
        { symbolicPairs
        , concretePairs
        , opaquePairs
        , symbolicConcretePairs
        , symbolicOpaquePairs
        , concreteOpaquePairs
        } = generatePairWiseElements builtinAcChild
    symbolicToAc (AcPair symbolic1 symbolic2) =
        let symbolicAc =
                emptyNormalizedAc
                    { elementsWithVariables = [symbolic1, symbolic2]
                    }
                    & wrapAc
         in toInternalAc symbolicAc
    concreteToAc (AcPair concrete1 concrete2) =
        let concreteAc =
                emptyNormalizedAc
                    { concreteElements = [concrete1, concrete2] & HashMap.fromList
                    }
                    & wrapAc
         in toInternalAc concreteAc
    opaqueToAc (AcPair opaque1 opaque2) =
        let opaqueAc =
                emptyNormalizedAc
                    { opaque = [opaque1, opaque2]
                    }
                    & wrapAc
         in toInternalAc opaqueAc
    symbolicConcreteToAc (symbolic, concrete) =
        let symbolicConcreteAc =
                emptyNormalizedAc
                    { elementsWithVariables = [symbolic]
                    , concreteElements = [concrete] & HashMap.fromList
                    }
                    & wrapAc
         in toInternalAc symbolicConcreteAc
    symbolicOpaqueToAc (symbolic, opaque') =
        let symbolicOpaqueAc =
                emptyNormalizedAc
                    { elementsWithVariables = [symbolic]
                    , opaque = [opaque']
                    }
                    & wrapAc
         in toInternalAc symbolicOpaqueAc
    concreteOpaqueToAc (concrete, opaque') =
        let concreteOpaqueAc =
                emptyNormalizedAc
                    { concreteElements = [concrete] & HashMap.fromList
                    , opaque = [opaque']
                    }
                    & wrapAc
         in toInternalAc concreteOpaqueAc
    toInternalAc normalized =
        InternalAc
            { builtinAcChild = normalized
            , builtinAcUnit
            , builtinAcElement
            , builtinAcSort
            , builtinAcConcat
            }

{- | Checks if a term is defined by only looking at the attributes.
 Should not be used outside this module.
-}
isDefinedInternal :: TermLike variable -> Bool
isDefinedInternal =
    Attribute.isDefined . TermLike.termDefined . TermLike.extractAttributes

fromSimplifiedFunctions ::
    InternalVariable variable =>
    HashSet (Application Symbol (TermLike variable)) ->
    SideCondition variable
fromSimplifiedFunctions simplifiedFunctions =
    top{simplifiedFunctions}

{- | Stores all non-constructor function symbols appearing in a term.
 Inside a rewrite step, this information is used to avoid trying to
 reevaluate functions which could not be evaluated during the 'Simplify'
 stage of execution.

 See 'isSimplifiedFunction'.
-}
cacheSimplifiedFunctions ::
    forall variable.
    InternalVariable variable =>
    TermLike variable ->
    SideCondition variable
cacheSimplifiedFunctions =
    fromSimplifiedFunctions
        . extractSimplifiedFunctions
  where
    extractSimplifiedFunctions ::
        TermLike variable ->
        HashSet (Application Symbol (TermLike variable))
    extractSimplifiedFunctions (Recursive.project -> _ :< termF) =
        case termF of
            TermLike.ApplySymbolF symbolApp ->
                let symbol = TermLike.applicationSymbolOrAlias symbolApp
                    children = TermLike.applicationChildren symbolApp
                    childrenSet = foldMap extractSimplifiedFunctions children
                 in if isFunction symbol && not (isConstructor symbol)
                        then HashSet.singleton symbolApp <> childrenSet
                        else childrenSet
            TermLike.AndF and' ->
                foldMap extractSimplifiedFunctions and'
            TermLike.ApplyAliasF aliasApp ->
                foldMap extractSimplifiedFunctions aliasApp
            TermLike.BottomF bottom ->
                foldMap extractSimplifiedFunctions bottom
            TermLike.CeilF ceil ->
                foldMap extractSimplifiedFunctions ceil
            TermLike.DomainValueF dv ->
                foldMap extractSimplifiedFunctions dv
            TermLike.EqualsF equals ->
                foldMap extractSimplifiedFunctions equals
            TermLike.ExistsF exists ->
                foldMap extractSimplifiedFunctions exists
            TermLike.FloorF floor' ->
                foldMap extractSimplifiedFunctions floor'
            TermLike.ForallF forall' ->
                foldMap extractSimplifiedFunctions forall'
            TermLike.IffF iff ->
                foldMap extractSimplifiedFunctions iff
            TermLike.ImpliesF implies ->
                foldMap extractSimplifiedFunctions implies
            TermLike.InF in' ->
                foldMap extractSimplifiedFunctions in'
            TermLike.MuF mu ->
                foldMap extractSimplifiedFunctions mu
            TermLike.NextF next ->
                foldMap extractSimplifiedFunctions next
            TermLike.NotF not' ->
                foldMap extractSimplifiedFunctions not'
            TermLike.NuF nu ->
                foldMap extractSimplifiedFunctions nu
            TermLike.OrF or' ->
                foldMap extractSimplifiedFunctions or'
            TermLike.RewritesF rewrites ->
                foldMap extractSimplifiedFunctions rewrites
            TermLike.TopF top' ->
                foldMap extractSimplifiedFunctions top'
            TermLike.InhabitantF inh ->
                foldMap extractSimplifiedFunctions inh
            TermLike.StringLiteralF stringLit ->
                foldMap extractSimplifiedFunctions stringLit
            TermLike.InternalBoolF bool ->
                foldMap extractSimplifiedFunctions bool
            TermLike.InternalBytesF bytes ->
                foldMap extractSimplifiedFunctions bytes
            TermLike.InternalIntF int ->
                foldMap extractSimplifiedFunctions int
            TermLike.InternalStringF string ->
                foldMap extractSimplifiedFunctions string
            TermLike.InternalListF list ->
                foldMap extractSimplifiedFunctions list
            TermLike.InternalMapF map' ->
                foldMap extractSimplifiedFunctions map'
            TermLike.InternalSetF set ->
                foldMap extractSimplifiedFunctions set
            TermLike.VariableF var ->
                foldMap extractSimplifiedFunctions var
            TermLike.EndiannessF end ->
                foldMap extractSimplifiedFunctions end
            TermLike.SignednessF sign ->
                foldMap extractSimplifiedFunctions sign
            TermLike.InjF inj ->
                foldMap extractSimplifiedFunctions inj

{- | Decides whether a function can be further simplified or not.
 See also 'cacheSimplifiedFunctions'.
-}
isSimplifiedFunction ::
    InternalVariable variable =>
    Application Symbol (TermLike variable) ->
    SideCondition variable ->
    Bool
isSimplifiedFunction app SideCondition{simplifiedFunctions} =
    HashSet.member app simplifiedFunctions
