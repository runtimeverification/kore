{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause

State machine definition for tracing @'Kore.Rewrite.Step.unifyRules'@.

* @Rules@ marks the beginning of @unifyRule@.

* Each rule yields a unification attempt going through different
  stages (@Rule@, @Init@, etc.)

* The attempt may fail at one of the several unification stages, or
  succeed (@Success@).

* @EndRules@ marks the end of the unification, after all rules have
  been tried.

Rules -> Rule
         A |
        /  |
       /   |
      /    V
     /    Init
    /      |
   /       |
   |       |
   |\      V
   | ----Unify-----
   |       |       \
   |       |        |
   |       V        |
   |    MakeCeil    |
   |       |  A     |
   |       |  |     |
   |\      V  |     |
   | --CombineCeil- |
   |       |       \|
   |       |        |
   |\      V        |
   | --CheckSide--- |
   |       |       \|
   |       |        |
   |       |        |
   \       V        V
    ---Success --> EndRules
-}
module Kore.Util.TSM.UnifyTag (
    UnifyTag(..),
    CheckImplTag(..),
) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Kore.Util.TSM (TimingStateMachine (..), (-->))
import Prelude.Kore

data UnifyTag
    = -- | starting to unify term with a set of rules
      Rules
    | -- | starting work on one rule (Logic.scatter)
      Rule
    | -- | starting work on one rule (worker function)
      Init
    | -- | starting unification for one rule
      Unify
    | -- | make ceil condition of unified term
      MakeCeil
    | -- | combine ceil with unification condition
      CombineCeil
    | -- | checking side conditions for one rule
      CheckSide
    | -- | successful unification using one rule
      Success
    | -- | ending term unification
      EndRules
    deriving stock (Eq, Ord, Enum, Bounded, Show)

{- ORMOLU_DISABLE -}
-- since it destroys the alignment of transitionLabels
instance TimingStateMachine UnifyTag where
    readTag t
        | "unify" : tag : _rest <- Text.splitOn ":" t =
            Map.lookup tag tagMap
        | otherwise =
            Nothing

    transitionLabels =
        Map.fromList
            [ Rules       --> Rule        $ "Starting"
            , Rule        --> Init        $ "InRule"
            , Init        --> Unify       $ "Init"
            , Unify       --> MakeCeil    $ "TermsUnified"
            , Unify       --> Rule        $ "UnifyFailed"
            , Unify       --> EndRules    $ "UnifyFailed"
            , MakeCeil    --> CombineCeil $ "MadeCeil"
            , MakeCeil    --> Rule        $ "CeilFailed"
            , MakeCeil    --> EndRules    $ "CeilFailed"
            , CombineCeil --> MakeCeil    $ "NextAlternative"
            , CombineCeil --> CheckSide   $ "Unified"
            , CombineCeil --> Rule        $ "WCeilFailed"
            , CombineCeil --> EndRules    $ "WCeilFailed"
            , CheckSide   --> Success     $ "SideChecked"
            , CheckSide   --> Rule        $ "SideFailed"
            , CheckSide   --> EndRules    $ "SideFailed"
            , Success     --> Rule        $ "Success"
            , Success     --> EndRules    $ "Success"
            , EndRules    --> Rules       $ Text.empty -- technical edge, filtered out
            ]
{- ORMOLU_ENABLE -}

{-
State machine definition for tracing @'Kore.Reachability.Claim.checkImplicationWorker'@.

StartCheck -----> Implied
     |              A
     V              |
 CouldUnify -------/|
     |              |
     V              /
 ReadyToRefute ----/
     |
     V
 CouldNotRefute --> NotImplied
-}

data CheckImplTag
    = -- | Starting to check an implication
      StartCheck
    | -- | Attempt to unify
      AttemptUnify
    | -- | Terms were unified
      CouldUnify
    | -- | The negated conjuncts were constructed
      BuiltToRefute
    | -- | (Part of) term to refute simplified and ready
      ReadyToRefute
    | -- | The SMT solver could not refute some of the terms
      CouldNotRefute
    | -- | Implication check failed
      NotImplied
    | -- | Check successful (no unification, or all conditions refuted)
      Implied
    deriving stock (Eq, Ord, Enum, Bounded, Show)

{- ORMOLU_DISABLE -}
-- since it destroys the alignment of transitionLabels
instance TimingStateMachine CheckImplTag where
    readTag t
        | "check-implication" : tag : _rest <- Text.splitOn ":" t =
            Map.lookup tag tagMap
        | otherwise =
            Nothing

    transitionLabels =
        Map.fromList
            [ StartCheck     --> AttemptUnify   $ "Starting"
            , AttemptUnify   --> Implied        $ "Success (unifier)"
            , AttemptUnify   --> CouldUnify     $ "Unifier found"
            , CouldUnify     --> Implied        $ "Success (simplifier)"
            , CouldUnify     --> BuiltToRefute  $ "Negated conjunct built"
            , BuiltToRefute  --> ReadyToRefute  $ "Conjunct simplified"
            , ReadyToRefute  --> Implied        $ "Success (SMT solver)"
            , ReadyToRefute  --> CouldNotRefute $ "SMT: counterexample"
            , CouldNotRefute --> NotImplied     $ "Examined"
            , Implied        --> StartCheck     $ Text.empty -- starting over
            , NotImplied     --> StartCheck     $ Text.empty -- starting over
            ]
{- ORMOLU_ENABLE -}
