{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.Log.InfoJsonRpcProcessRequest (
    InfoJsonRpcProcessRequest (..),
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

data InfoJsonRpcProcessRequest = forall a.
      (Show a, Pretty a, Typeable a) =>
    InfoJsonRpcProcessRequest
    { requestId :: Id
    , request :: a
    }

deriving stock instance Show InfoJsonRpcProcessRequest

instance Pretty InfoJsonRpcProcessRequest where
    pretty (InfoJsonRpcProcessRequest reqId req) =
        Pretty.hsep
            [ "Processing"
            , "request"
            , case reqId of
                IdInt intId -> Pretty.pretty intId
                IdTxt txtId -> Pretty.pretty txtId
            , "-"
            , Pretty.pretty req
            ]

instance Entry InfoJsonRpcProcessRequest where
    entrySeverity _ = Info
    oneLineDoc = Pretty.pretty
    oneLineContextDoc _ = single CtxInfo
    helpDoc _ = "log valid requests to the JSON RPC server"
