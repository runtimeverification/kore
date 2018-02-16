{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Kore.Variables.Fresh.Class where

import           Control.Monad.Reader                 (ReaderT, lift)
import qualified Control.Monad.State                  as State

import           Data.Kore.AST
import           Data.Kore.Variables.Fresh.IntCounter
import           Data.Kore.Variables.Int

{-|'FreshVariablesClass' links a `VariableClass` representing a type of
variables with a 'Monad' containing state needed to generate fresh variables
and provides several functions to generate new variables.
-}
class (Monad m, VariableClass var) => FreshVariablesClass m var where
    {-|Given an existing variable, generate a fresh one of
    the same type and sort.
    -}
    freshVariable :: IsMeta a => var a -> m (var a)
    {-|Given an existing unified variable, generate a fresh one of
    the same type and sort.
    -}
    freshUnifiedVariable :: UnifiedVariable var -> m (UnifiedVariable var)
    freshUnifiedVariable = transformUnifiedVariable
        (\v -> asUnifiedVariable <$> freshVariable v)
    {-|Given an existing 'UnifiedVariable' and a predicate, generate a
    fresh 'UnifiedVariable' of the same type and sort satisfying the predicate.
    -}
    freshVariableSuchThat
        :: UnifiedVariable var
        -> (UnifiedVariable var -> Bool)
        -> m (UnifiedVariable var)
    freshVariableSuchThat var p = do
        var' <- freshUnifiedVariable var
        if p var'
            then return var'
            else error "Cannot generate variable satisfying predicate"

instance FreshVariablesClass m var
    => FreshVariablesClass (ReaderT a m) var where
    freshVariable = lift . freshVariable

instance (Monad m, IntVariable var)
    => FreshVariablesClass (IntCounterT m) var where
    freshVariable var = do
        counter <- State.get
        State.modify (+1)
        return (intVariable var counter)
