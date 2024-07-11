{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Log.JsonRpc (
    LogJsonRpcServer (..),
) where

import Control.Monad.Logger (
    Loc (..),
    LogLevel (..),
    LogSource,
    LogStr,
    fromLogStr,
 )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )

data LogJsonRpcServer = LogJsonRpcServer
    { loc :: Loc
    , src :: LogSource
    , level :: LogLevel
    , msg :: LogStr
    }
    deriving stock (Show)

instance Pretty LogJsonRpcServer where
    pretty LogJsonRpcServer{msg} = pretty $ Text.decodeUtf8 $ fromLogStr msg

instance Entry LogJsonRpcServer where
    entrySeverity LogJsonRpcServer{level} = case level of
        LevelDebug -> Debug
        LevelInfo -> Info
        LevelWarn -> Warning
        LevelError -> Error
        _ -> Debug
    helpDoc _ = "log JSON RPC Server messages"
    oneLineDoc LogJsonRpcServer{msg} = pretty $ Text.replace "\n" " " $ Text.decodeUtf8 $ fromLogStr msg
    oneLineContextDoc l = [severityToContext $ entrySeverity l]
    contextDoc LogJsonRpcServer{loc} =
        if isDefaultLoc loc
            then Nothing
            else Just $ pretty fileLocStr
      where
        isDefaultLoc :: Loc -> Bool
        isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0, 0) (0, 0)) = True
        isDefaultLoc _ = False

        fileLocStr =
            (loc_package loc)
                ++ ':'
                : (loc_module loc)
                ++ ' '
                : (loc_filename loc)
                ++ ':'
                : (line loc)
                ++ ':'
                : (char loc)
          where
            line = show . fst . loc_start
            char = show . snd . loc_start
