{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugContext (
    DebugContext (..),
    SimpleContext (..), -- re-export for inContext
    inContext,
) where

import Data.Aeson qualified as JSON
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )

newtype DebugContext = DebugContext {ctx :: CLContext}
    deriving stock (Show)

instance Pretty DebugContext where
    pretty DebugContext{ctx} = pretty $ show ctx

instance Entry DebugContext where
    entrySeverity DebugContext{} = Debug
    helpDoc _ = "Debug message to signify an action's context"
    oneLineDoc DebugContext{ctx} = pretty $ show ctx
    oneLineContextDoc DebugContext{ctx} = [ctx]
    oneLineJson DebugContext{} = JSON.Null

inContext :: MonadLog log => SimpleContext -> log a -> log a
inContext = logWhile . DebugContext . CLNullary
