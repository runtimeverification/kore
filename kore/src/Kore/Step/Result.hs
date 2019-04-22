{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
 -}

module Kore.Step.Result
    ( Result (..)
    , mapRule
    , Results (..)
    , remainder
    , withoutRemainders
    , gatherResults
    , transitionResult
    , transitionResults
    , mapRules
    ) where

import           Control.Applicative
                 ( Alternative ((<|>)) )
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import           Data.Sequence
                 ( Seq )
import qualified GHC.Generics as GHC

import           Kore.Step.Representation.MultiOr
                 ( MultiOr )
import qualified Kore.Step.Representation.MultiOr as MultiOr
import           Kore.Step.Transition
                 ( TransitionT )
import qualified Kore.Step.Transition as Transition
import           Kore.TopBottom
                 ( TopBottom )

-- | The result of applying a single rule.
data Result rule config =
    Result
        { appliedRule :: !rule
        , result      :: !(MultiOr config)
        }
    deriving (Eq, Foldable, Functor, GHC.Generic, Ord, Show, Traversable)

{- | Apply a function to the 'appliedRule' of a 'Result'.

See also: 'mapRules'

 -}
mapRule :: (rule1 -> rule2) -> Result rule1 config -> Result rule2 config
mapRule f r@Result { appliedRule } = r { appliedRule = f appliedRule }

{- | The results of applying many rules.

The rules may be applied in sequence or in parallel and the 'remainders' vary
accordingly.

 -}
data Results rule config =
    Results
        { results :: !(Seq (Result rule config))
        , remainders :: !(MultiOr config)
        }
    deriving (Eq, GHC.Generic, Ord, Show)

instance (Ord config, TopBottom config) => Semigroup (Results rule config) where
    (<>) results1 results2 =
        Results
            { results = Function.on (<>) results results1 results2
            , remainders = Function.on (<>) remainders results1 results2
            }

instance (Ord config, TopBottom config) => Monoid (Results rule config) where
    mempty = Results { results = mempty, remainders = mempty }
    mappend = (<>)

{- | Take the 'Results' without any 'remainders'.
 -}
withoutRemainders
    :: (Ord config, TopBottom config)
    => Results rule config
    -> Results rule config
withoutRemainders results = results { remainders = mempty }

{- | 'Results' consisting of one remainder and no results.
 -}
remainder :: (Ord config, TopBottom config) => config -> Results rule config
remainder config = mempty { remainders = MultiOr.singleton config }

{- | Gather all the final configurations from the 'Results'.
 -}
gatherResults
    :: (Ord config, TopBottom config)
    => Results rule config
    -> MultiOr config
gatherResults = Foldable.fold . fmap result . results

{- | Distribute the 'Result' over a transition rule.
 -}
transitionResult :: Result rule config -> TransitionT rule m config
transitionResult Result { appliedRule, result } = do
    Transition.addRule appliedRule
    Foldable.asum (return <$> result)

{- | Distribute the 'Results' over a transition rule.
 -}
transitionResults :: Results rule config -> TransitionT rule m config
transitionResults Results { results, remainders } =
    transitionResultsResults <|> transitionResultsRemainders
  where
    transitionResultsResults = Foldable.asum (transitionResult <$> results)
    transitionResultsRemainders = Foldable.asum (return <$> remainders)

{- | Apply a function to the rules of the 'results'.

See also: 'mapRule'

 -}
mapRules :: (rule1 -> rule2) -> Results rule1 config -> Results rule2 config
mapRules f rs@Results { results } = rs { results = mapRule f <$> results }
