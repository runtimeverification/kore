{-# LANGUAGE QuasiQuotes #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Booster.Pattern.Rewrite (
    test_rewriteStep,
    test_performRewrite,
) where

import Control.Monad.Logger.CallStack
import Data.Bifunctor (second)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Numeric.Natural
import Test.Tasty
import Test.Tasty.HUnit

import Booster.Definition.Attributes.Base
import Booster.Definition.Base
import Booster.Log (Logger (..))
import Booster.Pattern.Base
import Booster.Pattern.Index (CellIndex (..), TermIndex (..))
import Booster.Pattern.Pretty (ModifiersRep (..))
import Booster.Pattern.Rewrite
import Booster.SMT.Interface (noSolver)
import Booster.Syntax.Json.Internalise (trm)
import Booster.Syntax.ParsedKore.Internalise (symb)
import Booster.Util (Flag (..))
import Test.Booster.Fixture hiding (inj)
import Test.Booster.Util ((@?>>=))

test_rewriteStep :: TestTree
test_rewriteStep =
    testGroup
        "Rewriting"
        [ errorCases
        , rewriteSuccess
        , unifyNotMatch
        , definednessUnclear
        , rewriteStuck
        , rulePriority
        ]

test_performRewrite :: TestTree
test_performRewrite =
    testGroup
        "Iterated rewriting"
        [ -- same tests as above, but calling the iterating function
          canRewrite
        , abortsOnErrors
        , callsError
        , getsStuckOnFailures
        , supportsDepthControl
        , supportsCutPoints
        , supportsTerminalRules
        ]

----------------------------------------

index :: SymbolName -> TermIndex
index = TermIndex . (: []) . TopSymbol

def :: KoreDefinition
def =
    testDefinition
        { rewriteTheory =
            mkTheory
                [ (index "con1", [rule1, rule2, rule1'])
                , (index "con3", [rule3])
                , (index "con4", [rule4])
                ]
        }

inj :: Symbol
inj = injectionSymbol

rule1, rule1', rule2, rule3, rule4 :: RewriteRule "Rewrite"
rule1 =
    rule
        (Just "con1-f1")
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), RuleVar:SortK{}) ) |]
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}(   \dv{SomeSort{}}("thing") ) ), RuleVar:SortK{}) ) |]
        42
rule1' =
    rule
        (Just "con1-f1'")
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( X:SomeSort{} ) ), RuleVar:SortK{}) ) |]
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}(   X:SomeSort{} ) ), RuleVar:SortK{}) ) |]
        50
rule2 =
    rule
        (Just "con1-f2")
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( X:SomeSort{} ) ),                  RuleVar:SortK{}) ) |]
        [trm| kCell{}( kseq{}( inj{AnotherSort{}, SortKItem{}}( con4{}( X:SomeSort{}, X:SomeSort{} ) ), RuleVar:SortK{}) ) |]
        50
rule3 =
    rule
        (Just "con3-con1")
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con3{}( \dv{SomeSort{}}("otherThing"), Y:SomeSort{} ) ), RuleVar:SortK{}) ) |]
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("somethingElse") ) ),            RuleVar:SortK{}) ) |]
        42
rule4 =
    rule
        (Just "con4-f2-partial")
        [trm| kCell{}( kseq{}( inj{AnotherSort{}, SortKItem{}}( con4{}( X:SomeSort{}, Y:SomeSort{} ) ), RuleVar:SortK{}) ) |]
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f2{}(   Y:SomeSort{} ) ), RuleVar:SortK{}) ) |]
        42
        `withComputedAttributes` ComputedAxiomAttributes False [UndefinedSymbol "f2"]

kCell :: Symbol
kCell =
    [symb| symbol Lbl'-LT-'k'-GT-'{}(SortK{}) : SortKCell{} [constructor{}()] |]

rule :: Maybe Text -> Term -> Term -> Priority -> RewriteRule "Rewrite"
rule ruleLabel lhs rhs priority =
    RewriteRule
        { lhs
        , rhs
        , requires = []
        , ensures = []
        , attributes =
            AxiomAttributes
                { location = Nothing
                , priority
                , ruleLabel
                , simplification = Flag False
                , preserving = Flag False
                , concreteness = Unconstrained
                , uniqueId = mockUniqueId
                , smtLemma = Flag False
                , syntacticClauses = SyntacticClauses []
                }
        , computedAttributes = ComputedAxiomAttributes False []
        , existentials = mempty
        }

withComputedAttributes :: RewriteRule r -> ComputedAxiomAttributes -> RewriteRule r
r@RewriteRule{lhs} `withComputedAttributes` computedAttributes =
    r{lhs, computedAttributes}

mkTheory :: [(TermIndex, [RewriteRule "Rewrite"])] -> Theory (RewriteRule "Rewrite")
mkTheory = Map.map mkPriorityGroups . Map.fromList
  where
    mkPriorityGroups :: [RewriteRule "Rewrite"] -> Map Priority [RewriteRule "Rewrite"]
    mkPriorityGroups rules =
        Map.unionsWith
            (<>)
            [Map.fromList [(r.attributes.priority, [r])] | r <- rules]

d :: Term
d = dv someSort "thing"

testConf :: IO RewriteConfig
testConf = do
    smtSolver <- noSolver
    pure
        RewriteConfig
            { definition = def
            , llvmApi = Nothing
            , smtSolver
            , varsToAvoid = mempty
            , doTracing = NoCollectRewriteTraces
            , logger = Logger $ const $ pure ()
            , prettyModifiers = ModifiersRep @'[] Proxy
            , mbMaxDepth = Nothing
            , mbSimplify = Nothing
            , cutLabels = []
            , terminalLabels = []
            }

----------------------------------------
errorCases
    , rewriteSuccess
    , unifyNotMatch
    , definednessUnclear
    , rewriteStuck
    , rulePriority ::
        TestTree
errorCases =
    testGroup
        "Simple error cases"
        [ testCase "Index is None" $ do
            let t =
                    [trm|
                        kCell{}(
                            kseq{}(
                                inj{SomeSort{}, SortKItem{}}(
                                    \and{SomeSort{}}(
                                        con1{}( \dv{SomeSort{}}("thing") ),
                                        con2{}( \dv{SomeSort{}}("thing") )
                                    )
                                ),
                                Thing:SortK{}
                            )
                        )
                    |]
            t `failsWith` TermIndexIsNone t
        ]
rewriteSuccess =
    testCase "con1 app rewrites to f1 app" $
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), ConfigVar:SortK{}) ) |]
            `rewritesTo` ( "con1-f1"
                         , [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}(   \dv{SomeSort{}}("thing") ) ), ConfigVar:SortK{}) ) |]
                         )
unifyNotMatch =
    testCase "Stuck case when subject has variables" $
        getsStuck
            [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con3{}( X:SomeSort{}, \dv{SomeSort{}}("thing") ) ), ConfigVar:SortK{}) ) |]
definednessUnclear =
    testCase "con4 rewrite to f2 might become undefined" $ do
        let pcon4 =
                [trm| kCell{}( kseq{}( inj{AnotherSort{}, SortKItem{}}( con4{}( \dv{SomeSort{}}("thing"), \dv{SomeSort{}}("thing") ) ), ConfigVar:SortK{}) ) |]
        pcon4 `failsWith` DefinednessUnclear rule4 (Pattern_ pcon4) [UndefinedSymbol "f2"]
rewriteStuck =
    testGroup
        "Stuck states"
        [ testCase "con3 app is stuck (no rules apply)" $ do
            let con3App =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con3{}( \dv{SomeSort{}}("thing"), \dv{SomeSort{}}("thing") ) ), ConfigVar:SortK{}) ) |]
            getsStuck con3App
        , testCase "No rules for con2" $
            getsStuck
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con2{}( \dv{SomeSort{}}("thing") ) ), Thing:SortK{}) ) |]
        , testCase "No rules for dotk()" $
            getsStuck
                [trm| kCell{}( kseq{}(dotk{}()) ) |]
        ]
rulePriority =
    testCase "con1 rewrites to a branch when higher priority does not apply" $
        [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("otherThing") ) ), ConfigVar:SortK{}) ) |]
            `branchesTo` [
                             ( "con1-f2"
                             , [trm| kCell{}( kseq{}( inj{AnotherSort{}, SortKItem{}}( con4{}( \dv{SomeSort{}}("otherThing"), \dv{SomeSort{}}("otherThing") ) ), ConfigVar:SortK{}) ) |]
                             )
                         ,
                             ( "con1-f1'"
                             , [trm| kCell{}( kseq{}( inj{SomeSort{},    SortKItem{}}( f1{}(   \dv{SomeSort{}}("otherThing")                                ) ), ConfigVar:SortK{}) ) |]
                             )
                         ]

runWith :: Term -> IO (Either (RewriteFailed "Rewrite") (RewriteResult Pattern))
runWith t =
    second fst <$> do
        conf <- testConf
        runNoLoggingT $ runRewriteT conf mempty (rewriteStep [] [] $ Pattern_ t)

rewritesTo :: Term -> (Text, Term) -> IO ()
t1 `rewritesTo` (lbl, t2) =
    runWith t1 @?>>= Right (RewriteFinished (Just lbl) (Just mockUniqueId) $ Pattern_ t2)

getsStuck :: Term -> IO ()
getsStuck t1 =
    runWith t1 @?>>= Right (RewriteStuck $ Pattern_ t1)

branchesTo :: Term -> [(Text, Term)] -> IO ()
t `branchesTo` ts =
    runWith t
        @?>>= Right
            (RewriteBranch (Pattern_ t) $ NE.fromList $ map (\(lbl, t') -> (lbl, mockUniqueId, Pattern_ t')) ts)

failsWith :: Term -> RewriteFailed "Rewrite" -> IO ()
failsWith t err =
    runWith t @?>>= Left err

----------------------------------------
-- tests for performRewrite (iterated rewrite in IO with logging)

runRewrite :: Term -> IO (Natural, RewriteResult Term)
runRewrite t = do
    conf <- testConf
    (counter, _, res) <-
        runNoLoggingT $
            performRewrite conf $
                Pattern_ t
    pure (counter, fmap (.term) res)

aborts :: RewriteFailed "Rewrite" -> Term -> IO ()
aborts failure t = runRewrite t >>= (@?= (0, RewriteAborted failure t))

newtype Steps = Steps Natural

rewrites :: Steps -> Term -> t -> (t -> RewriteResult Term) -> IO ()
rewrites (Steps n) t t' f =
    runRewrite t >>= (@?= (n, f t'))

canRewrite :: TestTree
canRewrite =
    testGroup
        "Can rewrite"
        [ testCase "Rewrites con1 once, then gets stuck" $
            let startTerm =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                targetTerm =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}(   \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
             in rewrites
                    (Steps 1)
                    startTerm
                    targetTerm
                    RewriteStuck
        , testCase "Rewrites con3 twice, branching on con1" $ do
            let branch1 =
                    ( "con1-f2"
                    , mockUniqueId
                    , [trm| kCell{}( kseq{}( inj{AnotherSort{}, SortKItem{}}( con4{}( \dv{SomeSort{}}("somethingElse"), \dv{SomeSort{}}("somethingElse") ) ), C:SortK{}) ) |]
                    )
                branch2 =
                    ( "con1-f1'"
                    , mockUniqueId
                    , [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}(    f1{}(   \dv{SomeSort{}}("somethingElse")                                   ) ), C:SortK{}) ) |]
                    )

            rewrites
                (Steps 1)
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con3{}( \dv{SomeSort{}}("otherThing"), \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("somethingElse")                        ) ), C:SortK{}) ) |]
                (`RewriteBranch` NE.fromList [branch1, branch2])
        , testCase "Returns stuck when no rules could be applied" $ do
            rewrites
                (Steps 0)
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con3{}( \dv{SomeSort{}}("thing"), \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con3{}( \dv{SomeSort{}}("thing"), \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                RewriteStuck
        , testCase "when there are no rules at all" $
            getsStuck $
                app con2 [d]
        ]

abortsOnErrors :: TestTree
abortsOnErrors =
    testGroup
        "Aborts rewrite when there is an error"
        [ testCase "when the term index is None" $
            let term =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( \and{SomeSort{}}( con1{}( \dv{SomeSort{}}("thing") ), con2{}( \dv{SomeSort{}}("thing") ) ) ), C:SortK{} ) ) |]
             in aborts
                    (TermIndexIsNone term)
                    term
        ]

callsError :: TestTree
callsError =
    testGroup
        "Calls error when there are unexpected situations"
        [ testCase "on wrong argument count in a symbol application" $
            runRewrite
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing"), \dv{SomeSort{}}("thing"), \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                >>= \case
                    (_, RewriteAborted InternalMatchError{} _) -> pure ()
                    _ -> assertFailure "success"
        ]

getsStuckOnFailures :: TestTree
getsStuckOnFailures =
    testGroup
        "Gets stuck when the rewriter cannot handle it"
        [ testCase "when unification is not a match" $
            getsStuck [trm| con3{}(X:SomeSort{}, \dv{SomeSort{}}("thing")) |]
        , testCase "when definedness is unclear" $
            getsStuck [trm| con4{}(\dv{SomeSort{}}("thing"), \dv{SomeSort{}}("thing")) |]
        ]

newtype MaxDepth = MaxDepth Natural

supportsDepthControl :: TestTree
supportsDepthControl =
    testGroup
        "supports maximum depth control"
        [ testCase "executes normally when maxDepth > maximum expected" $
            let startTerm =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                targetTerm =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
             in rewritesToDepth
                    (MaxDepth 42)
                    (Steps 1)
                    startTerm
                    targetTerm
                    RewriteStuck
        , testCase "stops execution after 1 step when maxDepth == 1" $
            rewritesToDepth
                (MaxDepth 1)
                (Steps 1)
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                (RewriteFinished Nothing Nothing)
        , testCase "performs no steps when maxDepth == 0" $
            rewritesToDepth
                (MaxDepth 0)
                (Steps 0)
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                (RewriteFinished Nothing Nothing)
        , testCase "prefers reporting branches to stopping at depth" $ do
            let branch1 =
                    ( "con1-f2"
                    , mockUniqueId
                    , [trm| kCell{}( kseq{}( inj{AnotherSort{}, SortKItem{}}( con4{}( \dv{SomeSort{}}("somethingElse"), \dv{SomeSort{}}("somethingElse") ) ), C:SortK{}) ) |]
                    )
                branch2 =
                    ( "con1-f1'"
                    , mockUniqueId
                    , [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}(    f1{}(   \dv{SomeSort{}}("somethingElse")                                   ) ), C:SortK{}) ) |]
                    )

            rewritesToDepth
                (MaxDepth 2)
                (Steps 1)
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con3{}( \dv{SomeSort{}}("otherThing"), \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("somethingElse")                        ) ), C:SortK{}) ) |]
                (`RewriteBranch` NE.fromList [branch1, branch2])
        ]
  where
    rewritesToDepth :: MaxDepth -> Steps -> Term -> t -> (t -> RewriteResult Term) -> IO ()
    rewritesToDepth (MaxDepth depth) (Steps n) t t' f = do
        conf <- testConf
        (counter, _, res) <-
            runNoLoggingT $ performRewrite conf{mbMaxDepth = Just depth} $ Pattern_ t
        (counter, fmap (.term) res) @?= (n, f t')

supportsCutPoints :: TestTree
supportsCutPoints =
    testGroup
        "supports cut-point labels"
        [ testCase "stops at a cut-point label" $
            rewritesToCutPoint
                "con1-f1"
                (Steps 0)
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                ( RewriteCutPoint
                    "con1-f1"
                    mockUniqueId
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                )
        , testCase "ignores non-matching cut-point labels" $
            let startTerm =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                targetTerm =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
             in rewritesToCutPoint
                    "otherLabel"
                    (Steps 1)
                    startTerm
                    targetTerm
                    RewriteStuck
        , testCase "prefers reporting branches to stopping at label in one branch" $ do
            let branch1 =
                    ( "con1-f2"
                    , mockUniqueId
                    , [trm| kCell{}( kseq{}( inj{AnotherSort{}, SortKItem{}}( con4{}( \dv{SomeSort{}}("somethingElse"), \dv{SomeSort{}}("somethingElse") ) ), C:SortK{}) ) |]
                    )
                branch2 =
                    ( "con1-f1'"
                    , mockUniqueId
                    , [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}(    f1{}(   \dv{SomeSort{}}("somethingElse")                                   ) ), C:SortK{}) ) |]
                    )

            rewritesToCutPoint
                "con1-f2"
                (Steps 1)
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con3{}( \dv{SomeSort{}}("otherThing"), \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("somethingElse")                        ) ), C:SortK{}) ) |]
                (`RewriteBranch` NE.fromList [branch1, branch2])
        ]
  where
    rewritesToCutPoint :: Text -> Steps -> Term -> t -> (t -> RewriteResult Term) -> IO ()
    rewritesToCutPoint lbl (Steps n) t t' f = do
        conf <- testConf
        (counter, _, res) <-
            runNoLoggingT $
                performRewrite conf{cutLabels = [lbl]} $
                    Pattern_ t
        (counter, fmap (.term) res) @?= (n, f t')

supportsTerminalRules :: TestTree
supportsTerminalRules =
    testGroup
        "supports cut-point labels"
        [ testCase "stops at a terminal rule label" $
            rewritesToTerminal
                "con1-f1"
                (Steps 1)
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                (RewriteTerminal "con1-f1" mockUniqueId)
        , testCase "ignores non-matching labels" $
            let startTerm =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( con1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
                targetTerm =
                    [trm| kCell{}( kseq{}( inj{SomeSort{}, SortKItem{}}( f1{}( \dv{SomeSort{}}("thing") ) ), C:SortK{}) ) |]
             in rewritesToTerminal
                    "otherLabel"
                    (Steps 1)
                    startTerm
                    targetTerm
                    RewriteStuck
        ]
  where
    rewritesToTerminal :: Text -> Steps -> Term -> t -> (t -> RewriteResult Term) -> IO ()
    rewritesToTerminal lbl (Steps n) t t' f = do
        conf <- testConf
        (counter, _, res) <-
            runNoLoggingT $ performRewrite conf{terminalLabels = [lbl]} $ Pattern_ t
        (counter, fmap (.term) res) @?= (n, f t')
