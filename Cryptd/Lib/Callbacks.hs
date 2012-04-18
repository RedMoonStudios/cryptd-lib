{-# LANGUAGE OverloadedStrings #-}
-- | A collection of callbacks for "Cryptd.Lib.Tunnel".
module Cryptd.Lib.Callbacks (requestLoop) where

import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TChan (readTChan, writeTChan)
import Control.Concurrent.STM.TVar (readTVar)
import Data.CaseInsensitive (mk)
import Network.Wai (Request(..))
import Data.ByteString.Char8 (pack)

import Cryptd.Lib.HTTP (request, withManager)
import Cryptd.Lib.HTTPSerial (supplyRequest)
import Cryptd.Lib.Tunnel (TunnelHandle, TunnelState(..), TunnelStatus(..), Channel(..))

-- | Default request loop callback for "Cryptd.Lib.Tunnel".
--
-- This callback is sending an HTTP 'request' to the supplied URL whenever a
-- 'ChannelRequest' value is coming from 'inChannel'.
requestLoop :: String
            -- ^ URL to send request to.
            -> TunnelHandle
            -> TunnelState
            -> IO ()
requestLoop url _ ts = forever . withManager $ \mgr -> liftIO $ do
    (fullReq, tstatus, tseq) <- atomically $ do
        val <- readTChan (inChannel ts)
        case val of
             ChannelRequest tseq r -> do
                 tstatus <- readTVar (tunnelStatus ts)
                 return (r, tstatus, tseq)
             _ -> retry
    req <- supplyRequest fullReq
    resp <- request mgr url $ transformRequest req tstatus
    atomically $ writeTChan (outChannel ts) (ChannelResponse tseq resp)
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
