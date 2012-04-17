{-# LANGUAGE TemplateHaskell #-}
-- | Provides support for double-sided HTTP proxies.
module Cryptd.Lib.Tunnel
    ( TunnelHandle
    , TunnelStatus(..)
    , TunnelState(..)
    , Channel(..)
    , HandlerCallbacks(..)
    , noCallbacks
    , makeHandler
    ) where

import System.IO (Handle)
import Data.DeriveTH
import Data.Serialize
import Control.Monad (forever, when)
import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar, newTVarIO)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan, newTChanIO)
import Network.TLS (TLSCtx, recvData', sendData)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

import Cryptd.Lib.HTTPSerial

-- | A datatype representing the protocol inside the tunnel.
data Channel = ChannelRequest FullRequest -- ^ HTTP request from tunnel
             | ChannelResponse Response -- ^ HTTP response to tunnel
             | ChannelKeepalive -- ^ Keepalive packet

$(derive makeSerialize ''Channel)

-- | A Handle for talking from/to the Tunnel.
type TunnelHandle = TLSCtx Handle

-- | The status of the tunnel.
data TunnelStatus
    = Active (Maybe String)
    -- ^ Tunnel is alive (with optional slave ID when used on master)
    | Shutdown
    -- ^ Tunnel is requested to shut down the connection
    | Down
    -- ^ Tunnel is not available.

-- | The state representing the tunnel's 'Channel's and 'TunnelStatus'.
data TunnelState = TunnelState
    { inChannel :: TChan Channel -- ^ Data coming from tunnel
    , outChannel :: TChan Channel -- ^ Data going to tunnel
    , tunnelStatus :: TVar TunnelStatus
    }

-- | Various callback functions for the tunnel handler.
data HandlerCallbacks = HandlerCallbacks
    { onConnect :: TunnelHandle -> TunnelState -> IO Bool
    -- ^ First function to call just after the connection is established.
    , onRecv :: TunnelHandle -> TunnelState -> IO ()
    -- ^ Called whenever data was received from the tunnel.
    , onLoop :: TunnelHandle -> TunnelState -> IO ()
    -- ^ Function that is 'forkIO'ed on every connection.
    }

-- | Return 'HandlerCallbacks' with all-noop functions.
noCallbacks :: HandlerCallbacks
noCallbacks = HandlerCallbacks
    { onConnect = \_ _ -> return True
    , onRecv = \_ _ -> return ()
    , onLoop = \_ _ -> return ()
    }

-- | Update a 'TVar'.
updateTVar :: TVar a
           -- ^ The 'TVar' to operate on.
           -> (a -> a)
           -- ^ Function that takes 'a' and returns a new value of type 'a'.
           -> STM ()
updateTVar dest fun =
    readTVar dest >>= writeTVar dest . fun

-- | Function that forks itself into the background and just receives data
-- sending it into the given 'TChan Channel'.
listener :: TunnelHandle -> TChan Channel -> IO ThreadId
listener handle input =
    forkIO $ forever $ do
        raw <- recvData' handle
        let packet = decode (B.concat $ LB.toChunks raw)
        case packet of
             Left e -> error $ "Can't decode packet from tunnel: " ++ e
             Right p -> atomically $ writeTChan input p

-- | Handle a connection by starting the 'listener', waiting for values
-- coming from 'outChannel' and pushing them into the connection.
handler :: HandlerCallbacks -> TunnelState -> TunnelHandle -> IO ()
handler cb state handle = connectHandler $ finishSetDown $ do
    _ <- listener handle (inChannel state)
    _ <- forkIO $ onLoop cb handle state
    atomically $ updateTVar (tunnelStatus state) setActive
    forever $ do
        raw <- atomically $ readTChan (outChannel state)
        let toSend = LB.fromChunks [encode raw]
        sendData handle toSend
  where
    setActive a@(Active _) = a
    setActive _ = Active Nothing

    finishSetDown = flip (>>) $ atomically (writeTVar (tunnelStatus state) Down)

    connectHandler io = onConnect cb handle state >>= flip when io

-- | Initialize all 'TVar's and 'TChan's associated with 'TunnelState'.
initializeState :: IO TunnelState
initializeState = do
    inChan <- newTChanIO
    outChan <- newTChanIO
    tstatus <- newTVarIO Down
    return $ TunnelState inChan outChan tstatus

-- | Create a new tunnel handler function using the given 'HandlerCallbacks'.
makeHandler :: HandlerCallbacks -> IO (TunnelState, TunnelHandle -> IO ())
makeHandler cb = do
    s <- initializeState
    return (s, handler cb s)
