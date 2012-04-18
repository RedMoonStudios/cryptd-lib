module Cryptd.Lib.Wai (tunnelEntry) where

import Data.Conduit (ResourceT)

import Control.Monad.Trans (liftIO)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TChan (readTChan, writeTChan)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)

import Network.Wai (Response)

import Cryptd.Lib.HTTPSerial (FullRequest)
import Cryptd.Lib.Tunnel (TunnelState(..), Channel(..))

-- | 'FullRequest' handler for requests to be tunneled to the other side of the
-- tunnel. This serves as a counterpart of 'Cryptd.Lib.Callbacks.requestLoop'.
tunnelEntry :: TunnelState -> FullRequest -> ResourceT IO Response
tunnelEntry ts fullReq = liftIO $ do
    tseq <- atomically $ do
        tseq <- readTVar (tunnelSeq ts)
        writeTVar (tunnelSeq ts) $ tseq + 1
        writeTChan (outChannel ts) (ChannelRequest tseq fullReq)
        return tseq

    atomically $ do
        val <- readTChan (inChannel ts)
        case val of
             ChannelResponse s r | s == tseq -> return r
             _                               -> retry
