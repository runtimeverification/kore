{- |
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

-}
module Kore.Step.Simplification.TermLike
    ( simplify
    , simplifyToOr
    , simplifyInternal
    ) where

import qualified Control.Error as Error
import qualified Control.Monad as Monad
import qualified Control.Monad.Morph as Monad.Morph
import qualified Control.Monad.Trans as Monad.Trans
import           Data.Function
import qualified Data.Functor.Foldable as Recursive

import           Control.Monad.Stabilize
import           Kore.Internal.OrPattern
                 ( OrPattern )
import qualified Kore.Internal.OrPattern as OrPattern
import           Kore.Internal.Pattern
                 ( Pattern )
import qualified Kore.Internal.Pattern as Pattern
import           Kore.Internal.Predicate
                 ( Predicate )
import qualified Kore.Internal.Predicate as Predicate
import           Kore.Internal.TermLike
import qualified Kore.Step.Function.Evaluator as Evaluator
import qualified Kore.Step.Simplification.And as And
                 ( simplify )
import qualified Kore.Step.Simplification.Application as Application
                 ( simplify )
import qualified Kore.Step.Simplification.Bottom as Bottom
                 ( simplify )
import qualified Kore.Step.Simplification.Builtin as Builtin
                 ( simplify )
import qualified Kore.Step.Simplification.Ceil as Ceil
                 ( simplify )
import qualified Kore.Step.Simplification.CharLiteral as CharLiteral
                 ( simplify )
import           Kore.Step.Simplification.Data
import qualified Kore.Step.Simplification.DomainValue as DomainValue
                 ( simplify )
import qualified Kore.Step.Simplification.Equals as Equals
                 ( simplify )
import qualified Kore.Step.Simplification.Exists as Exists
                 ( simplify )
import qualified Kore.Step.Simplification.Floor as Floor
                 ( simplify )
import qualified Kore.Step.Simplification.Forall as Forall
                 ( simplify )
import qualified Kore.Step.Simplification.Iff as Iff
                 ( simplify )
import qualified Kore.Step.Simplification.Implies as Implies
                 ( simplify )
import qualified Kore.Step.Simplification.In as In
                 ( simplify )
import qualified Kore.Step.Simplification.Inhabitant as Inhabitant
                 ( simplify )
import qualified Kore.Step.Simplification.Mu as Mu
                 ( simplify )
import qualified Kore.Step.Simplification.Next as Next
                 ( simplify )
import qualified Kore.Step.Simplification.Not as Not
                 ( simplify )
import qualified Kore.Step.Simplification.Nu as Nu
                 ( simplify )
import qualified Kore.Step.Simplification.Or as Or
                 ( simplify )
import qualified Kore.Step.Simplification.Rewrites as Rewrites
                 ( simplify )
import qualified Kore.Step.Simplification.StringLiteral as StringLiteral
                 ( simplify )
import qualified Kore.Step.Simplification.Top as Top
                 ( simplify )
import qualified Kore.Step.Simplification.Variable as Variable
                 ( simplify )
import           Kore.Step.Substitution
                 ( normalize )
import           Kore.Unparser
import           Kore.Variables.Fresh

-- TODO(virgil): Add a Simplifiable class and make all pattern types
-- instances of that.

{-|'simplify' simplifies a `TermLike`, returning a 'Pattern'.
-}
simplify
    ::  ( SortedVariable variable
        , Show variable
        , Ord variable
        , Unparse variable
        , FreshVariable variable
        )
    => TermLike variable
    -> Simplifier (Pattern variable)
simplify patt = do
    orPatt <- simplifyToOr patt
    return (OrPattern.toPattern orPatt)

{-|'simplifyToOr' simplifies a TermLike variable, returning an
'OrPattern'.
-}
simplifyToOr
    ::  forall variable simplifier
    .   (FreshVariable variable, SortedVariable variable)
    =>  (Show variable, Unparse variable)
    =>  MonadSimplify simplifier
    =>  TermLike variable
    ->  simplifier (OrPattern variable)
simplifyToOr =
    gatherPatterns . stabilize worker . Pattern.fromTermLike
  where
    -- | A single stabilizable simplification step. The step stabilizes after
    -- the internal simplifier when no user-defined axioms apply.
    worker
        :: Pattern variable
        -> StabilizeT (BranchT simplifier) (Pattern variable)
    worker input = do
        let (termLike, predicate) = Pattern.splitTerm input
        evaluated <-
            Evaluator.evaluateOnce predicate termLike
            & Monad.Morph.hoist Monad.Trans.lift
            & Error.maybeT (stable input) unstable
        Monad.Trans.lift $ simplifyPatternInternal evaluated

simplifyPatternInternal
    ::  forall variable simplifier
    .   ( SortedVariable variable
        , Show variable
        , Unparse variable
        , FreshVariable variable
        , MonadSimplify simplifier
        )
    => Pattern variable
    -> BranchT simplifier (Pattern variable)
simplifyPatternInternal (Pattern.splitTerm -> (termLike, predicate)) = do
    predicate' <- normalize predicate
    simplifyInternalExt predicate' termLike >>= scatter

simplifyInternal
    ::  forall variable simplifier
    .   ( SortedVariable variable
        , Show variable
        , Unparse variable
        , FreshVariable variable
        , MonadSimplify simplifier
        )
    => TermLike variable
    -> simplifier (OrPattern variable)
simplifyInternal = simplifyInternalExt Predicate.top

{- | Simplify the 'TermLike' in the context of the 'Predicate'.

@simplifyInternalExt@ 'Recursive.project's one layer of the 'TermLike' and
dispatches to one of the @Kore.Step.Simplification.*@ modules, after delegating
child simplification to 'simplifyTerm'.

 -}
-- TODO (thomas.tuegel): Actually use the context during simplification.
simplifyInternalExt
    ::  forall variable simplifier
    .   ( SortedVariable variable
        , Show variable
        , Unparse variable
        , FreshVariable variable
        , MonadSimplify simplifier
        )
    => Predicate variable
    -> TermLike variable
    -> simplifier (OrPattern variable)
simplifyInternalExt predicate =
    Monad.liftM (fmap andPredicate) . simplifyInternalWorker
  where
    andPredicate = flip Pattern.andCondition predicate

    simplifyChildren
        :: Traversable t
        => t (TermLike variable)
        -> simplifier (t (OrPattern variable))
    simplifyChildren =
        -- Simplify the /children/ of the pattern by delegating to the
        -- 'TermSimplifier' carried by the 'MonadSimplify' constraint.
        traverse simplifyTerm

    simplifyInternalWorker termLike =
        let doNotSimplify = return (OrPattern.fromTermLike termLike)
            (_ :< termLikeF) = Recursive.project termLike
        in case termLikeF of
            -- Unimplemented cases
            ApplyAliasF _ -> doNotSimplify
            -- Do not simplify evaluated patterns.
            EvaluatedF  _ -> doNotSimplify
            --
            AndF andF ->
                And.simplify =<< simplifyChildren andF
            ApplySymbolF applySymbolF ->
                Application.simplify =<< simplifyChildren applySymbolF
            CeilF ceilF ->
                Ceil.simplify =<< simplifyChildren ceilF
            EqualsF equalsF ->
                Equals.simplify =<< simplifyChildren equalsF
            ExistsF existsF ->
                Exists.simplify =<< simplifyChildren existsF
            IffF iffF ->
                Iff.simplify =<< simplifyChildren iffF
            ImpliesF impliesF ->
                Implies.simplify =<< simplifyChildren impliesF
            InF inF ->
                In.simplify =<< simplifyChildren inF
            NotF notF ->
                Not.simplify =<< simplifyChildren notF
            --
            BottomF bottomF -> Bottom.simplify <$> simplifyChildren bottomF
            BuiltinF builtinF -> Builtin.simplify <$> simplifyChildren builtinF
            DomainValueF domainValueF ->
                DomainValue.simplify <$> simplifyChildren domainValueF
            FloorF floorF -> Floor.simplify <$> simplifyChildren floorF
            ForallF forallF -> Forall.simplify <$> simplifyChildren forallF
            InhabitantF inhF -> Inhabitant.simplify <$> simplifyChildren inhF
            MuF muF -> Mu.simplify <$> simplifyChildren muF
            NuF nuF -> Nu.simplify <$> simplifyChildren nuF
            -- TODO(virgil): Move next up through patterns.
            NextF nextF -> Next.simplify <$> simplifyChildren nextF
            OrF orF -> Or.simplify <$> simplifyChildren orF
            RewritesF rewritesF ->
                Rewrites.simplify <$> simplifyChildren rewritesF
            StringLiteralF stringLiteralF ->
                StringLiteral.simplify <$> simplifyChildren stringLiteralF
            CharLiteralF charLiteralF ->
                CharLiteral.simplify <$> simplifyChildren charLiteralF
            TopF topF -> Top.simplify <$> simplifyChildren topF
            --
            VariableF variableF -> return $ Variable.simplify variableF
