{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Test.Booster.SMT.LowLevel (
    test_smoke,
) where

import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString.Char8 qualified as BS
import Test.Tasty
import Test.Tasty.HUnit

import Booster.SMT.Base as SMT
import Booster.SMT.LowLevelCodec
import Booster.SMT.Runner

test_smoke :: TestTree
test_smoke =
    testGroup
        "Smoke test for SMT bindings (low-level)"
        [ emptyIsSat
        , declTests
        , codecTests
        , checkTests
        ]

----------------------------------------
emptyIsSat :: TestTree
emptyIsSat =
    testCase "An empty solver responds to (check-sat) with sat" $
        (Sat @=?) =<< runSatAfter []

declTests :: TestTree
declTests =
    testGroup
        "Tests for declarations"
        [ testCase "declare-const" $ runsOK [Declare $ DeclareConst "x" "x" smtInt]
        , testCase "declare-fun(_)" $ runsOK [Declare $ DeclareFunc "f" "f" [smtInt] smtInt]
        , testCase "declare-fun(_, _)" $ runsOK [Declare $ DeclareFunc "g" "g" [smtInt, smtInt] smtInt]
        , testCase "declare simple sort" $ runsOK [Declare $ DeclareSort "" "SomeSort" 0]
        , testCase "declare simple sort" $ runsOK [Declare $ DeclareSort "" "OtherSort" 1]
        , testCase "declare custom-sorted function" $
            let sortName = "CustomSort"
             in runsOK
                    [ Declare $ DeclareSort "" sortName 1
                    , Declare $ DeclareFunc "" "f" [SMTSort sortName [smtInt]] smtInt
                    ]
        , testCase "declare assertion" $ runsOK [Declare $ Assert "0 = 0" $ eq 0 0]
        , testCase "declare a lemma with quantifiers" $
            let x = Atom "x"
                y = Atom "y"
                intSorted v = List [List [v, Atom "Int"]]
             in runsOK
                    [ Declare . Assert "an additive inverse exists for all int" $
                        List
                            [ Atom "forall"
                            , intSorted x
                            , List
                                [ Atom "exists"
                                , intSorted y
                                , eq x (List [Atom "-", y])
                                ]
                            ]
                    ]
        ]

-- we need to run a command with response after declaration commands,
-- otherwise they might just get queued.
runSatAfter :: [SMTCommand] -> IO SMT.Response
runSatAfter commands = runNoLoggingT $ do
    ctxt <- mkContext defaultSMTOptions [] mempty
    result <- evalSMT ctxt $ mapM_ runCmd commands >> runCmd CheckSat
    closeContext ctxt
    pure result

runsOK :: [SMTCommand] -> Assertion
runsOK cmds = runSatAfter cmds >>= (Sat @=?)

----------------------------------------
codecTests :: TestTree
codecTests =
    testGroup
        "Codec tests"
        [ responseParsing
        , valueParsing
        ]

responseParsing :: TestTree
responseParsing =
    testGroup
        "Response parsing"
        [ "sat" `parsesTo` Sat
        , "unsat" `parsesTo` Unsat
        , "unknown" `parsesTo` Unknown Nothing
        , "success" `parsesTo` Success
        , "(error \"Something was wrong\")" `parsesTo` Error "Something was wrong"
        , "((x 0))" `parsesTo` Values [(Atom "x", SMT.Int 0)]
        , "((x 0) (y true))" `parsesTo` Values [(Atom "x", SMT.Int 0), (Atom "y", SMT.Bool True)]
        ]
  where
    parsesTo :: BS.ByteString -> SMT.ResponseUnresolved -> TestTree
    input `parsesTo` expected =
        testCase (show input <> " parses to " <> show expected) $
            expected @=? readResponse input

valueParsing :: TestTree
valueParsing =
    testGroup
        "Parsing values"
        [ "42" `parsesAsValue` SMT.Int 42
        , "-42" `parsesAsValue` SMT.Int (negate 42)
        , "0" `parsesAsValue` SMT.Int 0
        , "(- 0)" `parsesAsValue` SMT.Int 0
        , "true" `parsesAsValue` SMT.Bool True
        , "false" `parsesAsValue` SMT.Bool False
        , "&^#$Rubbish" `parsesAsValue` Other (Atom "&^#$Rubbish")
        ]
  where
    parsesAsValue :: BS.ByteString -> Value -> TestTree
    input `parsesAsValue` expected =
        testCase (show input <> " parses to value " <> show expected) $
            Values [(Atom "x", expected)] @=? readResponse ("((x " <> input <> "))")

----------------------------------------
checkTests :: TestTree
checkTests =
    testGroup
        "Smoke tests for the runCheck function"
        [ test "Empty declarations" `returns` Sat $ []
        , test "Simple assertion" `returns` Sat $
            [ DeclareConst "" "X" smtInt
            , Assert "" $ eq (Atom "X") 42
            ]
        , test "Contradicting assertions" `returns` Unsat $
            [ DeclareConst "" "X" smtInt
            , Assert "" $ eq (Atom "X") 42
            , Assert "" $ neq (Atom "X") 42
            ]
        ]
  where
    exec x =
        runNoLoggingT $
            mkContext defaultSMTOptions [] mempty >>= \c -> evalSMT c x <* closeContext c
    test name result decls =
        testCase name $ (result @=?) =<< exec (runCheck decls)
    returns = ($)
