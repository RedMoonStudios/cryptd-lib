{-# LANGUAGE OverloadedStrings #-}
module Cryptd.Lib.Callbacks (requestLoop) where

import Control.Monad (forever)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TChan (readTChan, writeTChan)
import Control.Concurrent.STM.TVar (readTVar)
import Data.CaseInsensitive (mk)
import Network.Wai (Request(..))
import Data.ByteString.Char8 (pack)

import Cryptd.Lib.HTTP (request)
import Cryptd.Lib.HTTPSerial (supplyRequest)
import Cryptd.Lib.Tunnel (TunnelHandle, TunnelState(..), TunnelStatus(..), Channel(..))

requestLoop :: String -> TunnelHandle -> TunnelState -> IO ()
requestLoop url _ ts = forever $ do
    (fullReq, tstatus) <- atomically $ do
        val <- readTChan (inChannel ts)
        case val of
             ChannelRequest r -> do
                 tstatus <- readTVar (tunnelStatus ts)
                 return (r, tstatus)
             _ -> retry
    req <- supplyRequest fullReq
    resp <- request url $ transformRequest req tstatus
    atomically $ writeTChan (outChannel ts) (ChannelResponse resp)
  where
    transformRequest req (Active (Just pid)) =
        req { requestHeaders = newHeaders }
      where
        newHeaders = slaveId : filteredHeaders
        slaveId = (mk $ pack "X-SlaveID", pack pid)
        filteredHeaders = filter stripSlaveId $ requestHeaders req
        stripSlaveId ("X-SlaveID", _) = False
        stripSlaveId _                = True
    transformRequest req _ = req
