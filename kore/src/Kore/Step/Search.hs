{-|
Module      : Kore.Step.Search
Description : Search functionality matching krun API
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
-}
module Kore.Step.Search
    ( Config (..)
    , SearchType (..)
    , searchGraph
    , matchWith
    ) where

import Control.Error
       ( MaybeT (..) )
import Control.Error.Util
       ( hushT, nothing )
import Control.Monad.Trans.Class
       ( lift )
import Data.Maybe
       ( catMaybes )
import Data.Reflection
       ( give )
import Numeric.Natural
       ( Natural )

import           Data.Limit
                 ( Limit (..) )
import qualified Data.Limit as Limit
import           Kore.AST.MetaOrObject
import           Kore.Attribute.Symbol
                 ( StepperAttributes )
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import qualified Kore.Step.Condition.Evaluator as Predicate
                 ( evaluate )
import           Kore.Step.OrPredicate
                 ( OrPredicate )
import           Kore.Step.Pattern
                 ( Pattern, Predicate )
import qualified Kore.Step.Pattern as Conditional
import qualified Kore.Step.Representation.MultiOr as MultiOr
                 ( traverseWithPairs )
import           Kore.Step.Simplification.Data
                 ( PredicateSimplifier, Simplifier, TermLikeSimplifier )
import qualified Kore.Step.Strategy as Strategy
import           Kore.Step.Substitution
                 ( mergePredicatesAndSubstitutions )
import           Kore.Syntax.Variable
                 ( SortedVariable )
import           Kore.TopBottom
                 ( TopBottom (..) )
import           Kore.Unification.Procedure
                 ( unificationProcedure )
import           Kore.Unification.Unifier
                 ( UnificationProof (..) )
import qualified Kore.Unification.Unify as Monad.Unify
import           Kore.Unparser
import           Kore.Variables.Fresh
                 ( FreshVariable )

{-| Which configurations are considered for matching?

See also: 'searchGraph'

 -}
data SearchType
    = ONE
    -- ^ Reachable in exactly one execution step
    | FINAL
    -- ^ Reachable configurations which cannot be rewritten anymore
    | STAR
    -- ^ All reachable configurations
    | PLUS
    -- ^ All configurations reachable in at least one step
 deriving (Eq, Show)

-- | Search options
data Config =
    Config
    { bound :: !(Limit Natural)
    -- ^ maximum number of solutions
    , searchType :: !SearchType
    }

{- | Construct a list of solutions to the execution search problem.

The execution tree can be generated with 'Kore.Step.Strategy.runStrategy' or any
of the related functions in "Kore.Step.Step".

The matching criterion returns a substitution which takes its argument to the
search goal (see 'matchWith'). The 'searchType' is used to restrict which states
may be considered for matching.

@searchGraph@ returns a list of substitutions which take the initial
configuration to the goal defined by the matching criterion. The number of
solutions returned is limited by 'bound'.

See also: 'Kore.Step.Strategy.runStrategy', 'matchWith'

-}
searchGraph
    :: Config  -- ^ Search options
    -> (config -> MaybeT Simplifier substitution)
        -- ^ Matching criterion
    -> Strategy.ExecutionGraph config rule
        -- ^ Execution tree
    -> Simplifier [substitution]
searchGraph Config { searchType, bound } match executionGraph = do
    let selectedConfigs = pick executionGraph
    matches <- catMaybes <$> traverse (runMaybeT . match) selectedConfigs
    return (Limit.takeWithin bound matches)
  where
    pick =
        case searchType of
            ONE -> Strategy.pickOne
            PLUS -> Strategy.pickPlus
            STAR -> Strategy.pickStar
            FINAL -> Strategy.pickFinal

matchWith
    :: forall variable .
        ( SortedVariable variable
        , FreshVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier Object
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap Object
    -- ^ Map from symbol IDs to defined functions
    -> Pattern variable
    -> Pattern variable
    -> MaybeT Simplifier (OrPredicate variable)
matchWith tools substitutionSimplifier simplifier axiomIdToSimplifier e1 e2 = do
    (unifier, _proof) <-
        hushT . Monad.Unify.getUnifier $
            unificationProcedure
                tools
                substitutionSimplifier
                simplifier
                axiomIdToSimplifier
                t1
                t2
    let
        mergeAndEvaluate
            :: Predicate variable
            -> Simplifier
                ( Predicate variable
                , UnificationProof Object variable
                )
        mergeAndEvaluate predSubst = do
            (merged, _proof) <-
                mergePredicatesAndSubstitutions
                    tools
                    substitutionSimplifier
                    simplifier
                    axiomIdToSimplifier
                    [ Conditional.predicate predSubst
                    , Conditional.predicate e1
                    , Conditional.predicate e2
                    ]
                    [ Conditional.substitution predSubst]
            (evaluated, _proof) <-
                give tools
                $ Predicate.evaluate substitutionSimplifier simplifier
                $ Conditional.predicate merged
            mergePredicatesAndSubstitutions
                tools
                substitutionSimplifier
                simplifier
                axiomIdToSimplifier
                [ Conditional.predicate evaluated
                ]
                [ Conditional.substitution merged
                , Conditional.substitution evaluated
                ]
    (result, _proof) <-
        lift $ MultiOr.traverseWithPairs mergeAndEvaluate unifier
    if isBottom result
        then nothing
        else return result
  where
    t1 = Conditional.term e1
    t2 = Conditional.term e2
