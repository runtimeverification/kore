{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}

module Kore.ModelChecker.Bounded
    ( CheckResult (..)
    , bmcStrategy
    , check
    ) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable as Foldable
import           Data.Limit
                 ( Limit )
import qualified Data.Limit as Limit
import           Debug.Trace

import           Kore.AST.Common
                 ( SymbolOrAlias (..), Variable )
import           Kore.AST.Identifier
                 ( Id (..) )
import           Kore.AST.MetaOrObject
                 ( MetaOrObject )
import           Kore.AST.Valid
import           Kore.Attribute.Symbol
                 ( StepperAttributes )
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import           Kore.ModelChecker.Step
                 ( BMCTransition, CommonModalPattern, CommonProofState,
                 ModalPattern (..), Prim (..), defaultOneStepStrategy )
import qualified Kore.ModelChecker.Step as ProofState
                 ( ProofState (..) )
import qualified Kore.ModelChecker.Step as ModelChecker
                 ( transitionRule )
import           Kore.OnePath.Verification
                 ( Axiom (Axiom) )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Pattern
                 ( Conditional (Conditional) )
import           Kore.Step.Pattern as Conditional
                 ( Conditional (..) )
import           Kore.Step.Rule
                 ( ImplicationRule (ImplicationRule), RewriteRule,
                 RulePattern (..) )
import           Kore.Step.Simplification.Data
                 ( PredicateSimplifier, Simplifier, TermLikeSimplifier )
import           Kore.Step.Strategy
                 ( Strategy, pickFinal, runStrategy )
import           Numeric.Natural
                 ( Natural )

data CheckResult
    = Proved
    -- ^ Property is proved within the bound.
    | Failed
    -- ^ Counter example is found within the bound.
    | Unknown
    -- ^ Result is unknown within the bound.
    deriving (Show)

check
    :: MetaOrObject level
    => SmtMetadataTools StepperAttributes
    -> TermLikeSimplifier level
    -- ^ Simplifies normal patterns through, e.g., function evaluation
    -> PredicateSimplifier level
    -- ^ Simplifies predicates
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions
    ->  (  CommonModalPattern level
        -> [Strategy
            (Prim
                (CommonModalPattern level)
                (RewriteRule level Variable)
            )
           ]
        )
    -- ^ Creates a one-step strategy from a target pattern. See
    -- 'defaultStrategy'.
    -> [(ImplicationRule level Variable, Limit Natural)]
    -- ^ List of claims, together with a maximum number of verification steps
    -- for each.
    -> Simplifier [CheckResult]
check
    metadataTools
    simplifier
    substitutionSimplifier
    axiomIdToSimplifier
    strategyBuilder
  =
    mapM
        (checkClaim
            metadataTools
            simplifier
            substitutionSimplifier
            axiomIdToSimplifier
            strategyBuilder
        )

bmcStrategy
    :: forall level . (MetaOrObject level)
    => [Axiom level]
    -> CommonModalPattern level
    -> [Strategy
        (Prim
            (CommonModalPattern level)
            (RewriteRule level Variable)
        )
       ]
bmcStrategy
    axioms
    goal
  =  repeat (defaultOneStepStrategy goal rewrites)
  where
    rewrites :: [RewriteRule level Variable]
    rewrites = map unwrap axioms
      where
        unwrap (Axiom a) = a

checkClaim
    :: forall level . (MetaOrObject level)
    => SmtMetadataTools StepperAttributes
    -> TermLikeSimplifier level
    -> PredicateSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions
    ->  (  CommonModalPattern level
        -> [Strategy
            (Prim
                (CommonModalPattern level)
                (RewriteRule level Variable)
            )
           ]
        )
    -> (ImplicationRule level Variable, Limit Natural)
    -> Simplifier CheckResult
checkClaim
    metadataTools
    simplifier
    substitutionSimplifier
    axiomIdToSimplifier
    strategyBuilder
    (ImplicationRule RulePattern { left, right }, stepLimit)
  = do
        let
            App_ SymbolOrAlias { symbolOrAliasConstructor = symbol } [prop] = right
            goalPattern = ModalPattern { modalOp = getId symbol, term = prop }
            strategy =
                Limit.takeWithin
                    stepLimit
                    (strategyBuilder goalPattern)
            startState :: CommonProofState level
            startState =
                ProofState.GoalLHS
                    Conditional
                        {term = left, predicate = Predicate.makeTruePredicate, substitution = mempty}
        executionGraph <- State.evalStateT
                            (runStrategy transitionRule' strategy startState)
                            (Just ())
        let
            finalResult = (checkFinalNodes . pickFinal) executionGraph
        trace (show finalResult) (return finalResult)
  where
    transitionRule'
        :: Prim (CommonModalPattern level) (RewriteRule level Variable)
        -> (CommonProofState level)
        -> BMCTransition (CommonProofState level)
    transitionRule' =
        ModelChecker.transitionRule
            metadataTools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier

    checkFinalNodes
        :: [CommonProofState level]
        -> CheckResult
    checkFinalNodes nodes
      = Foldable.foldl' checkFinalNodesHelper Proved nodes
      where
        checkFinalNodesHelper Proved  ProofState.Proven = Proved
        checkFinalNodesHelper Proved  ProofState.Unprovable = Failed
        checkFinalNodesHelper Proved  _ = Unknown
        checkFinalNodesHelper Unknown ProofState.Unprovable = Failed
        checkFinalNodesHelper Unknown _ = Unknown
        checkFinalNodesHelper Failed  _ = Failed
