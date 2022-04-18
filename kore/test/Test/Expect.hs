module Test.Expect (
    expectRight,
    expectLeft,
    expectJust,
    expectNothing,
    expectThese,
    expectOne,
    assertNull,
    assertTop,
    expectBool,
    expectJustT,
    expectRightT,
) where

import Control.Error (
    ExceptT,
    MaybeT,
    exceptT,
    maybeT,
 )
import Data.These
import Debug
import Kore.Internal.InternalBool
import Kore.Internal.TermLike
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.TopBottom
import Prelude.Kore
import Test.Tasty.HUnit.Ext

expectRight :: HasCallStack => Debug left => Either left right -> IO right
expectRight = either (assertFailure . show . debug) return

expectLeft :: HasCallStack => Debug right => Either left right -> IO left
expectLeft = either return (assertFailure . show . debug)

expectJust :: HasCallStack => Maybe a -> IO a
expectJust = maybe (assertFailure "expected Just _, found Nothing") return

expectNothing :: HasCallStack => Maybe a -> IO ()
expectNothing (Just _) = assertFailure "expected Nothing, found Just _"
expectNothing Nothing = pure ()

expectThese :: HasCallStack => These a b -> IO (a, b)
expectThese =
    these
        (\_ -> assertFailure "expected (These _ _), but found (This _)")
        (\_ -> assertFailure "expected (These _ _), but found (That _)")
        (curry pure)

expectOne :: Foldable fold => HasCallStack => Debug [a] => fold a -> IO a
expectOne as =
    case toList as of
        [a] -> return a
        as' -> (assertFailure . show) (debug as')

assertNull :: Foldable fold => HasCallStack => Debug [a] => fold a -> IO ()
assertNull as =
    case toList as of
        [] -> return ()
        as' -> (assertFailure . show) (debug as')

assertTop :: HasCallStack => TopBottom a => Debug a => a -> IO ()
assertTop a
    | isTop a = return ()
    | otherwise = (assertFailure . show) (debug a)

expectBool :: HasCallStack => TermLike RewritingVariableName -> IO Bool
expectBool (InternalBool_ internalBool) = return (internalBoolValue internalBool)
expectBool term = (assertFailure . show) (debug term)

expectJustT :: MonadIO io => HasCallStack => MaybeT io a -> io a
expectJustT =
    maybeT
        (liftIO $ assertFailure "expected Just")
        return

expectRightT :: MonadIO io => HasCallStack => ExceptT e io a -> io a
expectRightT =
    exceptT
        (const $ liftIO $ assertFailure "expected Right")
        return
