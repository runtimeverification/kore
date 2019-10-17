module Kore.Unification.UnifierT
    ( UnifierT (..)
    , throwUnificationOrSubstitutionError
    , lowerExceptT
    , runUnifierT
    , maybeUnifierT
    -- * Substitution simplifiers
    , unification
    , original
    -- * Re-exports
    , module Kore.Unification.Unify
    ) where

import Control.Applicative
    ( Alternative
    )
import Control.Error
import Control.Monad
    ( MonadPlus
    )
import qualified Control.Monad.Except as Error
import qualified Control.Monad.Morph as Morph
import Control.Monad.Trans.Class
    ( MonadTrans (..)
    )
import Data.Function
    ( (&)
    )
import Data.Map.Strict
    ( Map
    )

import Branch
    ( BranchT
    )
import qualified Branch as BranchT
import Kore.Internal.OrPredicate
    ( OrPredicate
    )
import qualified Kore.Internal.OrPredicate as OrPredicate
import Kore.Internal.Predicate
    ( Predicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.TermLike
    ( TermLike
    )
import Kore.Logger
    ( LogMessage
    , WithLog (..)
    )
import qualified Kore.Predicate.Predicate as Syntax
    ( Predicate
    )
import qualified Kore.Predicate.Predicate as Syntax.Predicate
import Kore.Profiler.Data
    ( MonadProfiler
    )
import qualified Kore.Step.Simplification.Predicate as PredicateSimplifier
import Kore.Step.Simplification.Simplify
    ( MonadSimplify (..)
    , PredicateSimplifier (..)
    , SimplifierVariable
    )
import Kore.Step.Simplification.SubstitutionSimplifier
    ( SubstitutionSimplifier (..)
    )
import Kore.Substitute
    ( SubstitutionVariable
    )
import Kore.Unification.Error
import Kore.Unification.Substitution
    ( Substitution
    )
import Kore.Unification.SubstitutionNormalization
    ( normalizeSubstitution
    )
import qualified Kore.Unification.UnifierImpl as UnifierImpl
import Kore.Unification.Unify
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable
    )
import SMT
    ( MonadSMT (..)
    )

newtype UnifierT (m :: * -> *) a =
    UnifierT
        { getUnifierT :: BranchT (ExceptT UnificationOrSubstitutionError m) a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadTrans UnifierT where
    lift = UnifierT . lift . lift
    {-# INLINE lift #-}

deriving instance WithLog LogMessage m => WithLog LogMessage (UnifierT m)

deriving instance MonadSMT m => MonadSMT (UnifierT m)

deriving instance MonadProfiler m => MonadProfiler (UnifierT m)

instance MonadSimplify m => MonadSimplify (UnifierT m) where
    localSimplifierTermLike locally =
        \(UnifierT branchT) ->
            UnifierT
                (BranchT.mapBranchT
                    (Morph.hoist (localSimplifierTermLike locally))
                    branchT
                )
    {-# INLINE localSimplifierTermLike #-}

    localSimplifierAxioms locally =
        \(UnifierT branchT) ->
            UnifierT
                (BranchT.mapBranchT
                    (Morph.hoist (localSimplifierAxioms locally))
                    branchT
                )
    {-# INLINE localSimplifierAxioms #-}

    simplifyPredicate = simplifyPredicate'
      where
        PredicateSimplifier simplifyPredicate' =
            PredicateSimplifier.create unification
    {-# INLINE simplifyPredicate #-}

{- | A 'SubstitutionSimplifier' to use during unification.

If the 'Substitution' cannot be normalized, this simplifier uses
'Unifier.throwSubstitutionError'.

 -}
unification
    :: forall unifier
    .  MonadUnify unifier
    => SubstitutionSimplifier unifier
unification =
    SubstitutionSimplifier worker
  where
    worker
        :: forall variable
        .  SubstitutionVariable variable
        => Substitution variable
        -> unifier (OrPredicate variable)
    worker substitution =
        fmap OrPredicate.fromPredicates . gather $ do
            deduplicated <- UnifierImpl.deduplicateSubstitution substitution
            normalize1 deduplicated

    normalizeSubstitution'
        :: forall variable
        .  SubstitutionVariable variable
        => Map (UnifiedVariable variable) (TermLike variable)
        -> unifier (Predicate variable)
    normalizeSubstitution' =
        either throwSubstitutionError return . normalizeSubstitution

    normalize1
        ::  forall variable
        .   SubstitutionVariable variable
        =>  ( Syntax.Predicate variable
            , Map (UnifiedVariable variable) (TermLike variable)
            )
        ->  unifier (Predicate variable)
    normalize1 (predicate, deduplicated) = do
        normalized <- normalizeSubstitution' deduplicated
        return $ Predicate.fromPredicate predicate <> normalized

instance MonadSimplify m => MonadUnify (UnifierT m) where
    throwSubstitutionError =
        UnifierT
        . lift
        . Error.throwError
        . SubstitutionError
    {-# INLINE throwSubstitutionError #-}

    throwUnificationError =
        UnifierT
        . lift
        . Error.throwError
        . UnificationError
    {-# INLINE throwUnificationError #-}

    gather = UnifierT . lift . BranchT.gather . getUnifierT
    {-# INLINE gather #-}

    scatter = UnifierT . BranchT.scatter
    {-# INLINE scatter #-}

-- | Lower an 'ExceptT UnificationOrSubstitutionError' into a 'MonadUnify'.
lowerExceptT
    :: MonadUnify unifier
    => ExceptT UnificationOrSubstitutionError unifier a
    -> unifier a
lowerExceptT e =
    runExceptT e >>= either throwUnificationOrSubstitutionError pure

throwUnificationOrSubstitutionError
    :: MonadUnify unifier
    => UnificationOrSubstitutionError
    -> unifier a
throwUnificationOrSubstitutionError (SubstitutionError s) =
    throwSubstitutionError s
throwUnificationOrSubstitutionError (UnificationError u) =
    throwUnificationError u

runUnifierT
    :: MonadSimplify m
    => UnifierT m a
    -> m (Either UnificationOrSubstitutionError [a])
runUnifierT = runExceptT . BranchT.gather . getUnifierT

{- | Run a 'Unifier', returning 'Nothing' upon error.
 -}
maybeUnifierT :: MonadSimplify m => UnifierT m a -> MaybeT m [a]
maybeUnifierT = hushT . BranchT.gather . getUnifierT

original
    :: forall simplifier
    .  MonadSimplify simplifier
    => SubstitutionSimplifier simplifier
original =
    SubstitutionSimplifier worker
  where
    worker substitution = maybeUnwrapSubstitution substitution $ do
        -- We collect all the results here because we should promote the
        -- substitution to the predicate when there is an error on *any* branch.
        results <-
            UnifierImpl.normalizeOnce (Predicate.fromSubstitution substitution)
            & maybeUnifierT
        return (OrPredicate.fromPredicates results)

    maybeUnwrapSubstitution substitution =
        let unwrapped =
                OrPredicate.fromPredicate
                . Predicate.fromPredicate
                . Syntax.Predicate.markSimplified
                -- TODO (thomas.tuegel): Promoting the entire substitution
                -- to the predicate is a problem. We should only promote the
                -- part which has cyclic dependencies.
                $ Syntax.Predicate.fromSubstitution substitution
        in maybeT (return unwrapped) return
