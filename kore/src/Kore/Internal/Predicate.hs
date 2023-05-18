{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Internal.Predicate (
    Predicate, -- Constructor not exported on purpose
    PredicateF (..),
    unparseWithSort,
    unparse2WithSort,
    fromPredicate,
    makePredicate,
    makeTruePredicate,
    makeFalsePredicate,
    makeNotPredicate,
    makeAndPredicate,
    makeOrPredicate,
    makeImpliesPredicate,
    makeIffPredicate,
    makeCeilPredicate,
    makeFloorPredicate,
    makeInPredicate,
    makeEqualsPredicate,
    makeExistsPredicate,
    makeExistsPredicateN,
    makeForallPredicate,
    makeMultipleAndPredicate,
    makeMultipleOrPredicate,
    makeMultipleExists,
    makeMultipleForall,
    getMultiAndPredicate,
    getMultiOrPredicate,
    NotPredicate,
    isPredicate,
    simplifiedAttribute,
    isSimplified,
    isSimplifiedSomeCondition,
    isSimplifiedAnyCondition,
    isFreeOf,
    freeElementVariables,
    hasFreeVariable,
    mapVariables,
    depth,
    markSimplified,
    markSimplifiedConditional,
    markSimplifiedMaybeConditional,
    setSimplified,
    forgetSimplified,
    forgetSimplifiedSafe,
    wrapPredicate,
    containsSymbolWithIdPred,
    refreshExists,
    toMultiAnd,
    fromMultiAnd,
    pattern PredicateTrue,
    pattern PredicateFalse,
    pattern PredicateAnd,
    pattern PredicateOr,
    pattern PredicateNot,
    pattern PredicateCeil,
    pattern PredicateFloor,
    pattern PredicateEquals,
    pattern PredicateIn,
    pattern PredicateExists,
    pattern PredicateForall,
) where

import Control.Comonad.Trans.Env qualified as Env
import Data.Bifunctor qualified as Bifunctor
import Data.Binary (Binary (..))
import Data.Either qualified as Either
import Data.Foldable qualified as Foldable
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
import Data.List.Extra (
    nubOrd,
 )
import Data.Map.Strict qualified as Map
import Data.Semigroup
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables qualified as Attribute
import Kore.Attribute.Pattern.FreeVariables qualified as Attribute.FreeVariables (
    toNames,
    toSet,
 )
import Kore.Attribute.Pattern.Simplified qualified as Attribute
import Kore.Attribute.PredicatePattern (
    PredicatePattern,
 )
import Kore.Attribute.PredicatePattern qualified as PredicatePattern
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import Kore.Internal.MultiAnd qualified as MultiAnd
import Kore.Internal.SideCondition.SideCondition qualified as SideCondition (
    Representation,
 )
import Kore.Internal.TermLike hiding (
    AndF,
    BottomF,
    CeilF,
    EqualsF,
    ExistsF,
    FloorF,
    ForallF,
    IffF,
    ImpliesF,
    InF,
    NotF,
    OrF,
    TopF,
    depth,
    extractAttributes,
    forgetSimplified,
    hasFreeVariable,
    isSimplified,
    isSimplifiedSomeCondition,
    mapVariables,
    markSimplified,
    markSimplifiedConditional,
    markSimplifiedMaybeConditional,
    setSimplified,
    simplifiedAttribute,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Substitute
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser
import Kore.Variables.Fresh (
    refreshElementVariable,
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified
import SQL qualified

data PredicateF variable child
    = AndF !(And () child)
    | BottomF !(Bottom () child)
    | CeilF !(Ceil () (TermLike variable))
    | EqualsF !(Equals () (TermLike variable))
    | ExistsF !(Exists () variable child)
    | FloorF !(Floor () (TermLike variable))
    | ForallF !(Forall () variable child)
    | IffF !(Iff () child)
    | ImpliesF !(Implies () child)
    | InF !(In () (TermLike variable))
    | NotF !(Not () child)
    | OrF !(Or () child)
    | TopF !(Top () child)
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Ord variable => Synthetic (Attribute.FreeVariables variable) (PredicateF variable) where
    synthetic = \case
        AndF and' -> synthetic and'
        BottomF bottom -> synthetic bottom
        CeilF ceil -> synthetic (Attribute.freeVariables <$> ceil)
        EqualsF equals -> synthetic (Attribute.freeVariables <$> equals)
        ExistsF exists -> synthetic exists
        FloorF floor' -> synthetic (Attribute.freeVariables <$> floor')
        ForallF forall' -> synthetic forall'
        IffF iff -> synthetic iff
        ImpliesF implies -> synthetic implies
        InF in' -> synthetic (Attribute.freeVariables <$> in')
        NotF not' -> synthetic not'
        OrF or' -> synthetic or'
        TopF top -> synthetic top

instance Synthetic Attribute.Simplified (PredicateF variable) where
    synthetic = \case
        AndF and' -> synthetic and'
        BottomF bottom -> synthetic bottom
        CeilF ceil -> synthetic (TermLike.simplifiedAttribute <$> ceil)
        EqualsF equals -> synthetic (TermLike.simplifiedAttribute <$> equals)
        ExistsF exists -> synthetic exists
        FloorF floor' -> synthetic (TermLike.simplifiedAttribute <$> floor')
        ForallF forall' -> synthetic forall'
        IffF iff -> synthetic iff
        ImpliesF implies -> synthetic implies
        InF in' -> synthetic (TermLike.simplifiedAttribute <$> in')
        NotF not' -> synthetic not'
        OrF or' -> synthetic or'
        TopF top -> synthetic top

instance From (Ceil () (TermLike variable)) (PredicateF variable child) where
    from = CeilF
    {-# INLINE from #-}

instance From (Floor () (TermLike variable)) (PredicateF variable child) where
    from = FloorF
    {-# INLINE from #-}

instance From (Top () child) (PredicateF variable child) where
    from = TopF
    {-# INLINE from #-}

instance From (Bottom () child) (PredicateF variable child) where
    from = BottomF
    {-# INLINE from #-}

instance From (Equals () (TermLike variable)) (PredicateF variable child) where
    from = EqualsF
    {-# INLINE from #-}

instance From (In () (TermLike variable)) (PredicateF variable child) where
    from = InF
    {-# INLINE from #-}

instance From (Exists () variable child) (PredicateF variable child) where
    from = ExistsF
    {-# INLINE from #-}

instance From (Forall () variable child) (PredicateF variable child) where
    from = ForallF
    {-# INLINE from #-}

instance From (And () child) (PredicateF variable child) where
    from = AndF
    {-# INLINE from #-}

instance From (Or () child) (PredicateF variable child) where
    from = OrF
    {-# INLINE from #-}

instance From (Implies () child) (PredicateF variable child) where
    from = ImpliesF
    {-# INLINE from #-}

instance From (Iff () child) (PredicateF variable child) where
    from = IffF
    {-# INLINE from #-}

instance From (Not () child) (PredicateF variable child) where
    from = NotF
    {-# INLINE from #-}

-- | @Predicate@ is the internal form of Kore predicates.
newtype Predicate variable = Predicate
    { getPredicate ::
        Cofree (PredicateF variable) (PredicatePattern variable)
    }
    deriving stock (GHC.Generic, Show)

instance Binary variable => Binary (Predicate variable) where
    put (Recursive.project -> attrs :< predF) =
        put attrs >> put predF
    get =
        fmap Recursive.embed $ (:<) <$> get <*> get

instance SOP.Generic (Predicate variable)

instance SOP.HasDatatypeInfo (Predicate variable)

instance Debug variable => Debug (Predicate variable)

instance (Debug variable, Diff variable) => Diff (Predicate variable) where
    diffPrec
        pred1@(Recursive.project -> attrs1 :< predF1)
        pred2@(Recursive.project -> _ :< predF2) =
            -- If the patterns differ, do not display the difference in the
            -- attributes, which would overload the user with redundant information.
            diffPrecGeneric
                (Recursive.embed (attrs1 :< predF1))
                (Recursive.embed (attrs1 :< predF2))
                <|> diffPrecGeneric pred1 pred2

instance Eq variable => Eq (Predicate variable) where
    (==)
        (Recursive.project -> _ :< pat1)
        (Recursive.project -> _ :< pat2) =
            pat1 == pat2

instance Ord variable => Ord (Predicate variable) where
    compare
        (Recursive.project -> _ :< pat1)
        (Recursive.project -> _ :< pat2) =
            compare pat1 pat2

instance Hashable variable => Hashable (Predicate variable) where
    hashWithSalt salt (Recursive.project -> _ :< pat) = hashWithSalt salt pat
    {-# INLINE hashWithSalt #-}

instance NFData variable => NFData (Predicate variable) where
    rnf (Recursive.project -> annotation :< pat) =
        rnf annotation `seq` rnf pat

instance InternalVariable variable => Pretty (Predicate variable) where
    pretty = unparse . fromPredicate (mkSortVariable "_")

instance InternalVariable variable => SQL.Column (Predicate variable) where
    defineColumn = SQL.defineTextColumn
    toColumn = SQL.toColumn . Pretty.renderText . Pretty.layoutOneLine . pretty

type instance
    Base (Predicate variable) =
        CofreeF (PredicateF variable) (PredicatePattern variable)

-- This instance implements all class functions for the Predicate newtype
-- because the their implementations for the inner type may be specialized.
instance Recursive (Predicate variable) where
    project = \(Predicate embedded) ->
        case Recursive.project embedded of
            Compose (Identity projected) -> Predicate <$> projected
    {-# INLINE project #-}

    -- This specialization is particularly important: The default implementation
    -- of 'cata' in terms of 'project' would involve an extra call to 'fmap' at
    -- every level of the tree due to the implementation of 'project' above.
    cata alg = \(Predicate fixed) ->
        Recursive.cata
            (\(Compose (Identity base)) -> alg base)
            fixed
    {-# INLINE cata #-}

    para alg = \(Predicate fixed) ->
        Recursive.para
            ( \(Compose (Identity base)) ->
                alg (Bifunctor.first Predicate <$> base)
            )
            fixed
    {-# INLINE para #-}

    gpara dist alg = \(Predicate fixed) ->
        Recursive.gpara
            (\(Compose (Identity base)) -> Compose . Identity <$> dist base)
            (\(Compose (Identity base)) -> alg (Env.local Predicate <$> base))
            fixed
    {-# INLINE gpara #-}

    prepro pre alg = \(Predicate fixed) ->
        Recursive.prepro
            (\(Compose (Identity base)) -> (Compose . Identity) (pre base))
            (\(Compose (Identity base)) -> alg base)
            fixed
    {-# INLINE prepro #-}

    gprepro dist pre alg = \(Predicate fixed) ->
        Recursive.gprepro
            (\(Compose (Identity base)) -> Compose . Identity <$> dist base)
            (\(Compose (Identity base)) -> (Compose . Identity) (pre base))
            (\(Compose (Identity base)) -> alg base)
            fixed
    {-# INLINE gprepro #-}

-- This instance implements all class functions for the Predicate newtype
-- because the their implementations for the inner type may be specialized.
instance Corecursive (Predicate variable) where
    embed = \projected ->
        (Predicate . Recursive.embed . Compose . Identity)
            (getPredicate <$> projected)
    {-# INLINE embed #-}

    ana coalg = Predicate . ana0
      where
        ana0 =
            Recursive.ana (Compose . Identity . coalg)
    {-# INLINE ana #-}

    apo coalg = Predicate . apo0
      where
        apo0 =
            Recursive.apo
                ( \a ->
                    (Compose . Identity)
                        (Bifunctor.first getPredicate <$> coalg a)
                )
    {-# INLINE apo #-}

    postpro post coalg = Predicate . postpro0
      where
        postpro0 =
            Recursive.postpro
                (\(Compose (Identity base)) -> (Compose . Identity) (post base))
                (Compose . Identity . coalg)
    {-# INLINE postpro #-}

    gpostpro dist post coalg = Predicate . gpostpro0
      where
        gpostpro0 =
            Recursive.gpostpro
                (Compose . Identity . dist . (<$>) (runIdentity . getCompose))
                (\(Compose (Identity base)) -> (Compose . Identity) (post base))
                (Compose . Identity . coalg)
    {-# INLINE gpostpro #-}

instance TopBottom (Predicate variable) where
    isTop PredicateTrue = True
    isTop _ = False
    isBottom PredicateFalse = True
    isBottom _ = False

instance InternalVariable variable => Substitute (Predicate variable) where
    type TermType (Predicate variable) = TermLike variable

    type VariableNameType (Predicate variable) = variable

    rename = substitute . fmap mkVar
    {-# INLINE rename #-}

    substitute subst predicate =
        substituteNone <|> substituteBinder <|> substituteTermLike
            & fromMaybe substituteDefault
      where
        freeVars =
            Attribute.FreeVariables.toNames
                (Attribute.freeVariables predicate)
        subst' = Map.intersection subst (Map.fromSet id freeVars)
        originalVariables = Set.difference freeVars (Map.keysSet subst')
        targetFreeVariables =
            (Foldable.foldl' Set.union Set.empty)
                ( Attribute.FreeVariables.toNames
                    . Attribute.freeVariables
                    <$> subst'
                )
        freeVariables' = Set.union originalVariables targetFreeVariables
        avoidCapture = refreshElementVariable freeVariables'

        substituteNone
            | Map.null subst' = pure predicate
            | otherwise = empty

        substituteBinder = case predF of
            ExistsF exists'@Exists{existsVariable = var, existsChild = child} -> do
                var' <- avoidCapture var
                let subst'' =
                        Map.insert
                            (inject (variableName var))
                            (TermLike.mkVar $ mkSomeVariable var')
                            subst'
                (return . synthesize . ExistsF)
                    exists'
                        { existsVariable = var'
                        , existsChild = substitute subst'' child
                        }
            ForallF forall'@Forall{forallVariable = var, forallChild = child} -> do
                var' <- avoidCapture var
                let subst'' =
                        Map.insert
                            (inject (variableName var))
                            (TermLike.mkVar $ mkSomeVariable var')
                            subst'
                (return . synthesize . ForallF)
                    forall'
                        { forallVariable = var'
                        , forallChild = substitute subst'' child
                        }
            _ -> empty
          where
            _ :< predF = Recursive.project predicate

        substituteTermLike = case predF of
            CeilF ceilF ->
                (pure . synthesize . CeilF) (substitute subst' <$> ceilF)
            EqualsF equalsF ->
                (pure . synthesize . EqualsF) (substitute subst' <$> equalsF)
            FloorF floorF ->
                (pure . synthesize . FloorF) (substitute subst' <$> floorF)
            InF inF ->
                (pure . synthesize . InF) (substitute subst' <$> inF)
            _ -> empty
          where
            _ :< predF = Recursive.project predicate

        substituteDefault = synthesize (substitute subst' <$> predF)
          where
            _ :< predF = Recursive.project predicate

instance
    InternalVariable variable =>
    From (MultiAnd (Predicate variable)) (Predicate variable)
    where
    from = fromMultiAnd
    {-# INLINE from #-}

instance
    InternalVariable variable =>
    From (Predicate variable) (MultiAnd (Predicate variable))
    where
    from = toMultiAnd
    {-# INLINE from #-}

unparseWithSort ::
    forall variable ann.
    InternalVariable variable =>
    Sort ->
    Predicate variable ->
    Pretty.Doc ann
unparseWithSort sort predicate =
    unparseAssoc'
        ("\\and" <> parameters [sort])
        (unparse (mkTop sort :: TermLike variable))
        (worker <$> getMultiAndPredicate predicate)
  where
    worker = unparse . fromPredicate sort

unparse2WithSort ::
    InternalVariable variable =>
    Sort ->
    Predicate variable ->
    Pretty.Doc ann
unparse2WithSort sort = unparse2 . fromPredicate sort

-- | 'PredicateFalse' is a pattern for matching 'bottom' predicates.
pattern PredicateFalse :: Predicate variable
pattern PredicateFalse <- (Recursive.project -> _ :< BottomF _)

-- | 'PredicateTrue' is a pattern for matching 'top' predicates.
pattern PredicateTrue :: Predicate variable
pattern PredicateTrue <- (Recursive.project -> _ :< TopF _)

pattern PredicateAnd ::
    Predicate variable -> Predicate variable -> Predicate variable
pattern PredicateAnd p1 p2 <-
    (Recursive.project -> _ :< AndF (And () p1 p2))

pattern PredicateOr ::
    Predicate variable -> Predicate variable -> Predicate variable
pattern PredicateOr p1 p2 <-
    (Recursive.project -> _ :< OrF (Or () p1 p2))

pattern PredicateNot :: Predicate variable -> Predicate variable
pattern PredicateNot p <-
    (Recursive.project -> _ :< NotF (Not () p))

pattern PredicateCeil :: TermLike variable -> Predicate variable
pattern PredicateCeil t <- (Recursive.project -> _ :< CeilF (Ceil () () t))

pattern PredicateFloor :: TermLike variable -> Predicate variable
pattern PredicateFloor t <- (Recursive.project -> _ :< FloorF (Floor () () t))

pattern PredicateEquals :: TermLike variable -> TermLike variable -> Predicate variable
pattern PredicateEquals t1 t2 <- (Recursive.project -> _ :< EqualsF (Equals () () t1 t2))

pattern PredicateIn :: TermLike variable -> TermLike variable -> Predicate variable
pattern PredicateIn t1 t2 <- (Recursive.project -> _ :< InF (In () () t1 t2))

pattern PredicateExists :: ElementVariable variable -> Predicate variable -> Predicate variable
pattern PredicateExists var p <- (Recursive.project -> _ :< ExistsF (Exists () var p))

pattern PredicateForall :: ElementVariable variable -> Predicate variable -> Predicate variable
pattern PredicateForall var p <- (Recursive.project -> _ :< ForallF (Forall () var p))

-- | Return the 'TermLike' corresponding to the given 'Predicate'.
fromPredicate ::
    InternalVariable variable =>
    -- | Sort of resulting pattern
    Sort ->
    Predicate variable ->
    TermLike variable
fromPredicate sort = Recursive.fold worker
  where
    worker (pat :< predF) =
        TermLike.setSimplified
            (PredicatePattern.simplifiedAttribute pat)
            $ case predF of
                AndF (And () t1 t2) -> TermLike.mkAnd t1 t2
                BottomF _ -> TermLike.mkBottom sort
                CeilF (Ceil () () t) -> TermLike.mkCeil sort t
                EqualsF (Equals () () t1 t2) -> TermLike.mkEquals sort t1 t2
                ExistsF (Exists () v t) -> TermLike.mkExists v t
                FloorF (Floor () () t) -> TermLike.mkFloor sort t
                ForallF (Forall () v t) -> TermLike.mkForall v t
                IffF (Iff () t1 t2) -> TermLike.mkIff t1 t2
                ImpliesF (Implies () t1 t2) -> TermLike.mkImplies t1 t2
                InF (In () () t1 t2) -> TermLike.mkIn sort t1 t2
                NotF (Not () t) -> TermLike.mkNot t
                OrF (Or () t1 t2) -> TermLike.mkOr t1 t2
                TopF _ -> TermLike.mkTop sort

{- | Simple type used to track whether a predicate building function performed
    a simplification that changed the shape of the resulting term. This is
    needed when these functions are called while traversing the Predicate tree.
-}
data HasChanged = Changed | NotChanged
    deriving stock (Show, Eq)

instance Semigroup HasChanged where
    NotChanged <> x = x
    Changed <> _ = Changed

instance Monoid HasChanged where
    mempty = NotChanged

makeTruePredicate' ::
    InternalVariable variable =>
    (Predicate variable, HasChanged)
makeTruePredicate' = (synthesize (TopF Top{topSort = ()}), NotChanged)

makeTruePredicate :: InternalVariable variable => Predicate variable
makeTruePredicate = fst makeTruePredicate'

makeFalsePredicate' ::
    InternalVariable variable =>
    (Predicate variable, HasChanged)
makeFalsePredicate' =
    ( synthesize
        (BottomF Bottom{bottomSort = ()})
    , NotChanged
    )

makeFalsePredicate :: InternalVariable variable => Predicate variable
makeFalsePredicate = fst makeFalsePredicate'

makeNotPredicate' ::
    InternalVariable variable =>
    Predicate variable ->
    (Predicate variable, HasChanged)
makeNotPredicate' p
    | isTop p = (makeFalsePredicate, Changed)
    | isBottom p = (makeTruePredicate, Changed)
    | otherwise =
        ( synthesize $
            NotF
                Not
                    { notSort = ()
                    , notChild = p
                    }
        , NotChanged
        )

makeNotPredicate ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable
makeNotPredicate = fst . makeNotPredicate'

{- | 'makeAndPredicate' combines two Predicates with an 'and', doing some
simplification.
-}
makeAndPredicate' ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable ->
    (Predicate variable, HasChanged)
makeAndPredicate' p1 p2
    | isBottom p1 = (p1, Changed)
    | isBottom p2 = (p2, Changed)
    | isTop p1 = (p2, Changed)
    | isTop p2 = (p1, Changed)
    | p1 == p2 = (p1, Changed)
    | otherwise =
        ( synthesize $
            AndF
                And
                    { andSort = ()
                    , andFirst = p1
                    , andSecond = p2
                    }
        , NotChanged
        )

makeAndPredicate ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable ->
    Predicate variable
makeAndPredicate p1 p2 = fst (makeAndPredicate' p1 p2)

makeOrPredicate' ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable ->
    (Predicate variable, HasChanged)
makeOrPredicate' p1 p2
    | isTop p1 = (p1, Changed)
    | isTop p2 = (p2, Changed)
    | isBottom p1 = (p2, Changed)
    | isBottom p2 = (p1, Changed)
    | p1 == p2 = (p1, Changed)
    | otherwise =
        ( synthesize $
            OrF
                Or
                    { orSort = ()
                    , orFirst = p1
                    , orSecond = p2
                    }
        , NotChanged
        )

makeOrPredicate ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable ->
    Predicate variable
makeOrPredicate p1 p2 = fst (makeOrPredicate' p1 p2)

makeImpliesPredicate' ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable ->
    (Predicate variable, HasChanged)
makeImpliesPredicate' p1 p2
    | isBottom p1 = (makeTruePredicate, Changed)
    | isTop p2 = (p2, Changed)
    | isTop p1 = (p2, Changed)
    | isBottom p2 = (makeNotPredicate p1, Changed)
    | otherwise =
        ( synthesize $
            ImpliesF
                Implies
                    { impliesSort = ()
                    , impliesFirst = p1
                    , impliesSecond = p2
                    }
        , NotChanged
        )

makeImpliesPredicate ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable ->
    Predicate variable
makeImpliesPredicate p1 p2 = fst (makeImpliesPredicate' p1 p2)

makeIffPredicate' ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable ->
    (Predicate variable, HasChanged)
makeIffPredicate' p1 p2
    | isBottom p1 = (makeNotPredicate p2, Changed)
    | isTop p1 = (p2, Changed)
    | isBottom p2 = (makeNotPredicate p1, Changed)
    | isTop p2 = (p1, Changed)
    | otherwise =
        ( synthesize $
            IffF
                Iff
                    { iffSort = ()
                    , iffFirst = p1
                    , iffSecond = p2
                    }
        , NotChanged
        )

makeIffPredicate ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable ->
    Predicate variable
makeIffPredicate p1 p2 = fst (makeIffPredicate' p1 p2)

makeCeilPredicate' ::
    InternalVariable variable =>
    TermLike variable ->
    (Predicate variable, HasChanged)
makeCeilPredicate' t =
    ( synthesize $
        CeilF
            Ceil
                { ceilOperandSort = ()
                , ceilResultSort = ()
                , ceilChild = t
                }
    , NotChanged
    )

makeCeilPredicate ::
    InternalVariable variable =>
    TermLike variable ->
    Predicate variable
makeCeilPredicate = fst . makeCeilPredicate'

makeFloorPredicate' ::
    InternalVariable variable =>
    TermLike variable ->
    (Predicate variable, HasChanged)
makeFloorPredicate' t =
    ( synthesize $
        FloorF
            Floor
                { floorOperandSort = ()
                , floorResultSort = ()
                , floorChild = t
                }
    , NotChanged
    )

makeFloorPredicate ::
    InternalVariable variable =>
    TermLike variable ->
    Predicate variable
makeFloorPredicate = fst . makeFloorPredicate'

makeInPredicate' ::
    InternalVariable variable =>
    TermLike variable ->
    TermLike variable ->
    (Predicate variable, HasChanged)
makeInPredicate' t1 t2 =
    (TermLike.checkSortsAgree makeInWorker t1 t2, NotChanged)
  where
    makeInWorker t1' t2' _ = synthesize $ InF $ In () () t1' t2'

makeInPredicate ::
    InternalVariable variable =>
    TermLike variable ->
    TermLike variable ->
    Predicate variable
makeInPredicate t1 t2 = fst (makeInPredicate' t1 t2)

makeEqualsPredicate' ::
    InternalVariable variable =>
    TermLike variable ->
    TermLike variable ->
    (Predicate variable, HasChanged)
makeEqualsPredicate' t1 t2 =
    (TermLike.checkSortsAgree makeEqualsWorker t1 t2, NotChanged)
  where
    makeEqualsWorker t1' t2' _ = synthesize $ EqualsF $ Equals () () t1' t2'

makeEqualsPredicate ::
    InternalVariable variable =>
    TermLike variable ->
    TermLike variable ->
    Predicate variable
makeEqualsPredicate t1 t2 = fst (makeEqualsPredicate' t1 t2)

makeExistsPredicate' ::
    InternalVariable variable =>
    ElementVariable variable ->
    Predicate variable ->
    (Predicate variable, HasChanged)
makeExistsPredicate' v p
    | isTop p = (p, Changed)
    | isBottom p = (p, Changed)
    | otherwise =
        ( synthesize $
            ExistsF
                Exists
                    { existsSort = ()
                    , existsVariable = v
                    , existsChild = p
                    }
        , NotChanged
        )

makeExistsPredicate ::
    InternalVariable variable =>
    ElementVariable variable ->
    Predicate variable ->
    Predicate variable
makeExistsPredicate v p = fst (makeExistsPredicate' v p)

makeExistsPredicateN ::
    InternalVariable variable =>
    Foldable foldable =>
    foldable (ElementVariable variable) ->
    Predicate variable ->
    Predicate variable
makeExistsPredicateN = appEndo . foldMap (Endo . makeExistsPredicate)

makeForallPredicate' ::
    InternalVariable variable =>
    ElementVariable variable ->
    Predicate variable ->
    (Predicate variable, HasChanged)
makeForallPredicate' v p
    | isTop p = (p, Changed)
    | isBottom p = (p, Changed)
    | otherwise =
        ( synthesize $
            ForallF
                Forall
                    { forallSort = ()
                    , forallVariable = v
                    , forallChild = p
                    }
        , NotChanged
        )

makeForallPredicate ::
    InternalVariable variable =>
    ElementVariable variable ->
    Predicate variable ->
    Predicate variable
makeForallPredicate v p = fst (makeForallPredicate' v p)

makeMultipleAndPredicate ::
    InternalVariable variable =>
    [Predicate variable] ->
    Predicate variable
makeMultipleAndPredicate =
    foldl' makeAndPredicate makeTruePredicate . nubOrd

makeMultipleOrPredicate ::
    InternalVariable variable =>
    [Predicate variable] ->
    Predicate variable
makeMultipleOrPredicate =
    foldl' makeOrPredicate makeFalsePredicate . nubOrd

makeMultipleExists ::
    (Foldable f, InternalVariable variable) =>
    f (ElementVariable variable) ->
    Predicate variable ->
    Predicate variable
makeMultipleExists vars p =
    foldr makeExistsPredicate p vars

makeMultipleForall ::
    (Foldable f, InternalVariable variable) =>
    f (ElementVariable variable) ->
    Predicate variable ->
    Predicate variable
makeMultipleForall vars p =
    foldr makeForallPredicate p vars

getMultiAndPredicate ::
    Predicate variable ->
    [Predicate variable]
getMultiAndPredicate = \case
    PredicateAnd p1 p2 -> concatMap getMultiAndPredicate [p1, p2]
    p -> [p]

getMultiOrPredicate ::
    Predicate variable ->
    [Predicate variable]
getMultiOrPredicate = \case
    PredicateOr p1 p2 -> concatMap getMultiOrPredicate [p1, p2]
    p -> [p]

newtype NotPredicate variable
    = NotPredicate (TermLikeF variable (Predicate variable))
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance
    InternalVariable variable =>
    Pretty (NotPredicate variable)
    where
    pretty (NotPredicate termLikeF) =
        Pretty.vsep
            [ "Expected a predicate, but found:"
            , Pretty.indent
                4
                (unparse $ fromPredicate (mkSortVariable "_") <$> termLikeF)
            ]

makePredicate ::
    forall variable.
    HasCallStack =>
    InternalVariable variable =>
    TermLike variable ->
    Either (NotPredicate variable) (Predicate variable)
makePredicate t = fst <$> makePredicateWorker t
  where
    makePredicateWorker ::
        TermLike variable ->
        Either (NotPredicate variable) (Predicate variable, HasChanged)
    makePredicateWorker =
        Recursive.elgot makePredicateBottomUp makePredicateTopDown

    makePredicateBottomUp ::
        Base
            (TermLike variable)
            (Either (NotPredicate variable) (Predicate variable, HasChanged)) ->
        Either (NotPredicate variable) (Predicate variable, HasChanged)
    makePredicateBottomUp termE = do
        termWithChanged <- sequence termE
        let dropChanged ::
                (Predicate variable, HasChanged) -> Predicate variable
            dropChanged = fst

            dropPredicate :: (Predicate variable, HasChanged) -> HasChanged
            dropPredicate = snd

            att :< patE = dropChanged <$> termWithChanged

            childChanged :: HasChanged
            childChanged = foldMap dropPredicate termWithChanged

            oldSimplified = TermLike.attributeSimplifiedAttribute att
        (predicate, topChanged) <- case patE of
            TermLike.TopF _ -> return makeTruePredicate'
            TermLike.BottomF _ -> return makeFalsePredicate'
            TermLike.AndF p ->
                return $
                    makeAndPredicate' (andFirst p) (andSecond p)
            TermLike.OrF p ->
                return $
                    makeOrPredicate' (orFirst p) (orSecond p)
            TermLike.IffF p ->
                return $
                    makeIffPredicate' (iffFirst p) (iffSecond p)
            TermLike.ImpliesF p ->
                return $
                    makeImpliesPredicate' (impliesFirst p) (impliesSecond p)
            TermLike.NotF p -> return $ makeNotPredicate' (notChild p)
            TermLike.ExistsF p ->
                return $
                    makeExistsPredicate' (existsVariable p) (existsChild p)
            TermLike.ForallF p ->
                return $
                    makeForallPredicate' (forallVariable p) (forallChild p)
            p -> Left (NotPredicate p)
        return $ case topChanged <> childChanged of
            Changed -> (predicate, Changed)
            NotChanged ->
                (setSimplified oldSimplified predicate, NotChanged)

    makePredicateTopDown ::
        TermLike variable ->
        Either
            (Either (NotPredicate variable) (Predicate variable, HasChanged))
            (Base (TermLike variable) (TermLike variable))
    makePredicateTopDown (Recursive.project -> projected@(att :< pat)) =
        case pat of
            TermLike.CeilF Ceil{ceilChild} ->
                setSmp $
                    makeCeilPredicate' ceilChild
            TermLike.FloorF Floor{floorChild} ->
                setSmp $
                    makeFloorPredicate' floorChild
            TermLike.EqualsF Equals{equalsFirst, equalsSecond} ->
                setSmp $
                    makeEqualsPredicate' equalsFirst equalsSecond
            TermLike.InF In{inContainedChild, inContainingChild} ->
                setSmp $
                    makeInPredicate' inContainedChild inContainingChild
            _ -> Right projected
      where
        setSmp (p, Changed) = Left $ pure (p, Changed)
        setSmp (p, NotChanged) =
            Left $
                pure
                    (setSimplified oldSimplified p, NotChanged)

        oldSimplified = TermLike.attributeSimplifiedAttribute att

isPredicate :: InternalVariable variable => TermLike variable -> Bool
isPredicate = Either.isRight . makePredicate

extractAttributes :: Predicate variable -> PredicatePattern variable
extractAttributes (Recursive.project -> attr :< _) = attr

instance Attribute.HasFreeVariables (Predicate variable) variable where
    freeVariables = Attribute.freeVariables . extractAttributes

simplifiedAttribute :: Predicate variable -> Attribute.Simplified
simplifiedAttribute = PredicatePattern.simplifiedAttribute . extractAttributes

{- | Is the 'Predicate' fully simplified under the given side condition?

See also: 'isSimplifiedSomeCondition'.
-}
isSimplified :: SideCondition.Representation -> Predicate variable -> Bool
isSimplified condition = PredicatePattern.isSimplified condition . extractAttributes

{- | Is the 'Predicate' fully simplified under some side condition?

See also: 'isSimplified'.
-}
isSimplifiedSomeCondition :: Predicate variable -> Bool
isSimplifiedSomeCondition =
    PredicatePattern.isSimplifiedSomeCondition . extractAttributes

{- | Is the 'Predicate' fully simplified under any side condition?

See also: 'isSimplified'.
-}
isSimplifiedAnyCondition :: Predicate variable -> Bool
isSimplifiedAnyCondition =
    PredicatePattern.isSimplifiedAnyCondition . extractAttributes

cannotSimplifyNotSimplifiedError ::
    (HasCallStack, InternalVariable variable) =>
    PredicateF variable (Predicate variable) ->
    a
cannotSimplifyNotSimplifiedError predF =
    error
        ( "Unexpectedly marking term with NotSimplified children as simplified:\n"
            ++ show predF
            ++ "\n"
            ++ (show . pretty $ synthesize predF)
        )

simplifiedFromChildren ::
    HasCallStack =>
    PredicateF variable (Predicate variable) ->
    Attribute.Simplified
simplifiedFromChildren predF =
    case mergedSimplified of
        Attribute.NotSimplified -> Attribute.NotSimplified
        _ -> mergedSimplified `Attribute.simplifiedTo` Attribute.fullySimplified
  where
    mergedSimplified = case predF of
        CeilF ceil' -> foldMap TermLike.simplifiedAttribute ceil'
        FloorF floor' -> foldMap TermLike.simplifiedAttribute floor'
        EqualsF equals' -> foldMap TermLike.simplifiedAttribute equals'
        InF in' -> foldMap TermLike.simplifiedAttribute in'
        _ -> foldMap simplifiedAttribute predF

checkedSimplifiedFromChildren ::
    (HasCallStack, InternalVariable variable) =>
    PredicateF variable (Predicate variable) ->
    Attribute.Simplified
checkedSimplifiedFromChildren predF =
    case simplifiedFromChildren predF of
        Attribute.NotSimplified -> cannotSimplifyNotSimplifiedError predF
        simplified -> simplified

markSimplified ::
    (HasCallStack, InternalVariable variable) =>
    Predicate variable ->
    Predicate variable
markSimplified (Recursive.project -> attrs :< predF) =
    Recursive.embed
        ( PredicatePattern.setSimplified
            (checkedSimplifiedFromChildren predF)
            attrs
            :< predF
        )

markSimplifiedConditional ::
    (HasCallStack, InternalVariable variable) =>
    SideCondition.Representation ->
    Predicate variable ->
    Predicate variable
markSimplifiedConditional
    condition
    (Recursive.project -> attrs :< predF) =
        Recursive.embed
            ( PredicatePattern.setSimplified
                ( checkedSimplifiedFromChildren predF
                    <> Attribute.simplifiedConditionally condition
                )
                attrs
                :< predF
            )

markSimplifiedMaybeConditional ::
    (HasCallStack, InternalVariable variable) =>
    Maybe SideCondition.Representation ->
    Predicate variable ->
    Predicate variable
markSimplifiedMaybeConditional Nothing = markSimplified
markSimplifiedMaybeConditional (Just condition) =
    markSimplifiedConditional condition

setSimplified ::
    (HasCallStack, InternalVariable variable) =>
    Attribute.Simplified ->
    Predicate variable ->
    Predicate variable
setSimplified
    simplified
    (Recursive.project -> attrs :< predF) =
        Recursive.embed
            ( PredicatePattern.setSimplified mergedSimplified attrs
                :< predF
            )
      where
        childSimplified = simplifiedFromChildren predF
        mergedSimplified = case (childSimplified, simplified) of
            (Attribute.NotSimplified, Attribute.NotSimplified) ->
                Attribute.NotSimplified
            (Attribute.NotSimplified, _) ->
                cannotSimplifyNotSimplifiedError predF
            (_, Attribute.NotSimplified) ->
                Attribute.NotSimplified
            _ -> childSimplified <> simplified

{- | Forget the 'simplifiedAttribute' associated with a 'Predicate'.
This is not safe to be used inside the simplifier, see 'forgetSimplifiedSafe',
but the following will always hold:

@
isSimplified (forgetSimplified _) == False
@
This is not always true for 'forgetSimplifiedSafe'.
-}
forgetSimplified ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable
forgetSimplified = Recursive.fold worker
  where
    worker (_ :< predF) = case predF of
        CeilF ceil' ->
            synthesize $
                CeilF
                    (TermLike.forgetSimplified <$> ceil')
        FloorF floor' ->
            synthesize $
                FloorF
                    (TermLike.forgetSimplified <$> floor')
        EqualsF equals' ->
            synthesize $
                EqualsF
                    (TermLike.forgetSimplified <$> equals')
        InF in' ->
            synthesize $
                InF
                    (TermLike.forgetSimplified <$> in')
        _ -> synthesize predF

{- | Forget the 'simplifiedAttribute' associated with a 'Predicate', with
some special handling of specific subterms.
This is safe to be used inside the simplifier.
See 'Kore.Internal.TermLike.forgetSimplifiedIgnorePredicates'.
-}
forgetSimplifiedSafe ::
    InternalVariable variable =>
    Predicate variable ->
    Predicate variable
forgetSimplifiedSafe = Recursive.fold worker
  where
    worker (_ :< predF) = case predF of
        CeilF ceil' ->
            synthesize $
                CeilF
                    (TermLike.forgetSimplifiedIgnorePredicates <$> ceil')
        FloorF floor' ->
            synthesize $
                FloorF
                    (TermLike.forgetSimplifiedIgnorePredicates <$> floor')
        EqualsF equals' ->
            synthesize $
                EqualsF
                    (TermLike.forgetSimplifiedIgnorePredicates <$> equals')
        InF in' ->
            synthesize $
                InF
                    (TermLike.forgetSimplifiedIgnorePredicates <$> in')
        _ -> synthesize predF

mapVariables ::
    forall variable1 variable2.
    InternalVariable variable1 =>
    InternalVariable variable2 =>
    AdjSomeVariableName (variable1 -> variable2) ->
    Predicate variable1 ->
    Predicate variable2
mapVariables adj predicate =
    let termPredicate =
            TermLike.mapVariables adj
                -- TODO (Andrei B): Try to avoid TermLike conversion
                . fromPredicate (mkSortVariable "_")
                $ predicate
     in either
            errorMappingVariables
            id
            (makePredicate termPredicate)
  where
    errorMappingVariables termPredicate =
        error . show . Pretty.vsep $
            [ "Error when mapping the variables of predicate:"
            , Pretty.pretty predicate
            , "The resulting term is not a predicate:"
            , Pretty.pretty termPredicate
            ]

-- | Is the predicate free of the given variables?
isFreeOf ::
    Ord variable =>
    Predicate variable ->
    Set (SomeVariable variable) ->
    Bool
isFreeOf predicate =
    Set.disjoint
        ( Attribute.FreeVariables.toSet $
            Attribute.freeVariables predicate
        )

freeElementVariables :: Predicate variable -> [ElementVariable variable]
freeElementVariables =
    PredicatePattern.getFreeElementVariables
        . Attribute.freeVariables

hasFreeVariable ::
    Ord variable =>
    SomeVariableName variable ->
    Predicate variable ->
    Bool
hasFreeVariable variableName =
    PredicatePattern.isFreeVariable variableName
        . Attribute.freeVariables

-- !!  TODO The following is just a temporary solution and  !!
-- !!  the code using wrapPredicate should be refactored    !!

wrapPredicate ::
    HasCallStack =>
    InternalVariable variable =>
    TermLike variable ->
    Predicate variable
wrapPredicate =
    either
        (error "Term cannot be wrapped.\nInput TermLike is not a Predicate despite being supposed to be\n")
        id
        . makePredicate

depth :: Predicate variable -> Int
depth = Recursive.fold levelDepth
  where
    levelDepth (_ :< predF) = case predF of
        CeilF x -> 1 + foldl' max 0 (TermLike.depth <$> x)
        FloorF x -> 1 + foldl' max 0 (TermLike.depth <$> x)
        EqualsF x -> 1 + foldl' max 0 (TermLike.depth <$> x)
        InF x -> 1 + foldl' max 0 (TermLike.depth <$> x)
        _ -> 1 + foldl' max 0 predF

containsSymbolWithIdPred :: String -> Predicate variable -> Bool
containsSymbolWithIdPred symId (Recursive.project -> _ :< predicate) =
    case predicate of
        EqualsF x -> any (containsSymbolWithId symId) x
        InF x -> any (containsSymbolWithId symId) x
        CeilF x -> any (containsSymbolWithId symId) x
        FloorF x -> any (containsSymbolWithId symId) x
        _ -> any (containsSymbolWithIdPred symId) predicate

fromMultiAnd ::
    InternalVariable variable =>
    MultiAnd (Predicate variable) ->
    Predicate variable
fromMultiAnd predicates =
    case toList predicates of
        [] -> makeTruePredicate
        _ -> foldr1 makeAndPredicate predicates

toMultiAnd ::
    Ord variable =>
    Predicate variable ->
    MultiAnd (Predicate variable)
toMultiAnd = MultiAnd.make . getMultiAndPredicate
