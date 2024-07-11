{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Log.InfoJsonRpcCancelRequest (
    InfoJsonRpcCancelRequest (..),
) where

import Log
import Network.JSONRPC (
    Id (..),
 )
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

newtype InfoJsonRpcCancelRequest = InfoJsonRpcCancelRequest {getCancelRequestId :: Id}
    deriving stock (Eq, Show)

instance Pretty InfoJsonRpcCancelRequest where
    pretty (InfoJsonRpcCancelRequest reqId) =
        Pretty.hsep
            [ "Request"
            , case reqId of
                IdInt intId -> Pretty.pretty intId
                IdTxt txtId -> Pretty.pretty txtId
            , "has been cancelled"
            ]

instance Entry InfoJsonRpcCancelRequest where
    entrySeverity _ = Info
    oneLineDoc = Pretty.pretty
    oneLineContextDoc _ = single CtxInfo
    helpDoc _ = "log cancelled request to the JSON RPC server"
