{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Rewrite.Result (
    Result (..),
    mapRule,
    Results (..),
    remainder,
    hasResults,
    withoutRemainders,
    gatherResults,
    mergeResults,
    transitionResult,
    transitionResults,
    mapRules,
    mapConfigs,
    traverseConfigs,
    toAttemptedAxiom,
) where

import Control.Lens qualified as Lens
import Data.Binary (Binary)
import Data.Generics.Product (
    field,
 )
import Data.Sequence (
    Seq,
 )
import GHC.Generics qualified as GHC
import Kore.Internal.MultiOr (
    MultiOr,
 )
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Rewrite.Transition (
    TransitionT,
 )
import Kore.Rewrite.Transition qualified as Transition
import Kore.Simplify.Simplify (
    AttemptedAxiom (..),
    AttemptedAxiomResults (AttemptedAxiomResults),
    InternalVariable,
 )
import Kore.TopBottom (
    TopBottom,
 )
import Prelude.Kore

-- | The result of applying a single rule.
data Result rule config = Result
    { appliedRule :: !rule
    , result :: !(MultiOr config)
    }
    deriving stock (Eq, Foldable, GHC.Generic, Ord, Show)
    deriving anyclass (Binary)

-- | Apply a function to the 'appliedRule' of a 'Result'.
mapRule :: (rule1 -> rule2) -> Result rule1 config -> Result rule2 config
mapRule f r@Result{appliedRule} = r{appliedRule = f appliedRule}

-- | Apply a function to the 'result' of a 'Result'.
mapResult ::
    Ord config2 =>
    TopBottom config2 =>
    (config1 -> config2) ->
    Result rule config1 ->
    Result rule config2
mapResult f = Lens.over (field @"result") (MultiOr.map f)

traverseResult ::
    Ord config2 =>
    TopBottom config2 =>
    Applicative f =>
    (config1 -> f config2) ->
    Result rule config1 ->
    f (Result rule config2)
traverseResult f = Lens.traverseOf (field @"result") (MultiOr.traverse f)

{- | The results of applying many rules.

The rules may be applied in sequence or in parallel and the 'remainders' vary
accordingly.
-}
data Results rule config = Results
    { results :: !(Seq (Result rule config))
    , remainders :: !(MultiOr config)
    }
    deriving stock (Eq, GHC.Generic, Ord, Show)
    deriving anyclass (Binary)

instance (Ord config, TopBottom config) => Semigroup (Results rule config) where
    (<>) results1 results2 =
        Results
            { results = on (<>) results results1 results2
            , remainders = on (<>) remainders results1 results2
            }

instance (Ord config, TopBottom config) => Monoid (Results rule config) where
    mempty = Results{results = mempty, remainders = mempty}
    mappend = (<>)

-- | 'True' if any rule applied.
hasResults :: Results rule config -> Bool
hasResults = not . null . results

mergeResults ::
    (Ord config, TopBottom config) =>
    [Results rule config] ->
    Results rule config
mergeResults = fold

-- | Take the 'Results' without any 'remainders'.
withoutRemainders ::
    Ord config =>
    Results rule config ->
    Results rule config
withoutRemainders results = results{remainders = mempty}

-- | 'Results' consisting of one remainder and no results.
remainder :: (Ord config, TopBottom config) => config -> Results rule config
remainder config = mempty{remainders = MultiOr.singleton config}

-- | Gather all the final configurations from the 'Results'.
gatherResults ::
    Ord config =>
    Results rule config ->
    MultiOr config
gatherResults = foldMap result . results

-- | Distribute the 'Result' over a transition rule.
transitionResult :: Result rule config -> TransitionT rule m config
transitionResult Result{appliedRule, result} = do
    Transition.addRule appliedRule
    asum (return <$> toList result)

-- | Distribute the 'Results' over a transition rule.
transitionResults :: Results rule config -> TransitionT rule m config
transitionResults Results{results, remainders} =
    transitionResultsResults <|> transitionResultsRemainders
  where
    transitionResultsResults = asum (transitionResult <$> results)
    transitionResultsRemainders =
        asum (return <$> toList remainders)

{- | Apply a function to the rules of the 'results'.

See also: 'mapRule'
-}
mapRules :: (rule1 -> rule2) -> Results rule1 config -> Results rule2 config
mapRules f rs@Results{results} = rs{results = mapRule f <$> results}

-- | Apply functions to the configurations of the 'results' and 'remainders'.
mapConfigs ::
    Ord config2 =>
    TopBottom config2 =>
    -- | map 'results'
    (config1 -> config2) ->
    -- | map 'remainders'
    (config1 -> config2) ->
    Results rule config1 ->
    Results rule config2
mapConfigs mapResults mapRemainders Results{results, remainders} =
    Results
        { results = mapResult mapResults <$> results
        , remainders = MultiOr.map mapRemainders remainders
        }

{- | Apply 'Applicative' transformations to the configurations of the 'results'
and 'remainders'.
-}
traverseConfigs ::
    Ord config2 =>
    TopBottom config2 =>
    Applicative f =>
    -- | traverse 'results'
    (config1 -> f config2) ->
    -- | traverse 'remainders'
    (config1 -> f config2) ->
    Results rule config1 ->
    f (Results rule config2)
traverseConfigs traverseResults traverseRemainders rs =
    Results
        <$> (traverse . traverseResult) traverseResults (results rs)
        <*> MultiOr.traverse traverseRemainders (remainders rs)

toAttemptedAxiom ::
    InternalVariable variable =>
    Results rule (Pattern variable) ->
    AttemptedAxiom variable
toAttemptedAxiom results
    | hasResults results =
        Applied $ AttemptedAxiomResults (gatherResults results) (remainders results)
    | otherwise = NotApplicable
