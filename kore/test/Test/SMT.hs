module Test.SMT (
    testPropertyWithSolver,
    testPropertyWithoutSolver,
    testCaseWithoutSMT,
    assertEqual',
    runSMT,
    runSMTWithConfig,
    runNoSMT,
) where

import Control.Monad.Morph qualified as Morph
import Hedgehog
import Log (
    runLoggerT,
 )
import Prelude.Kore
import SMT (
    SMT,
 )
import SMT qualified
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

testPropertyWithSolver ::
    HasCallStack =>
    String ->
    PropertyT SMT () ->
    TestTree
testPropertyWithSolver str =
    testProperty str . Hedgehog.property . Morph.hoist (runSMT (pure ()))

testPropertyWithoutSolver ::
    HasCallStack =>
    String ->
    PropertyT SMT () ->
    TestTree
testPropertyWithoutSolver str =
    testProperty str . Hedgehog.property . Morph.hoist runNoSMT

testCaseWithoutSMT :: String -> SMT () -> TestTree
testCaseWithoutSMT str = testCase str . runNoSMT

assertEqual' ::
    MonadIO m =>
    (Eq a, Show a) =>
    HasCallStack =>
    -- | Remark
    String ->
    -- | Expected value
    a ->
    -- | Actual value
    a ->
    m ()
assertEqual' str expect = liftIO . assertEqual str expect

runSMT :: SMT () -> SMT a -> IO a
runSMT = runSMTWithConfig SMT.z3Config

runSMTWithConfig :: SMT.Config -> SMT () -> SMT a -> IO a
runSMTWithConfig config userInit = flip runLoggerT mempty . SMT.runSMT config userInit

runNoSMT :: SMT a -> IO a
runNoSMT = flip runLoggerT mempty . SMT.runNoSMT
