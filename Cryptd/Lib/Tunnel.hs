{-# LANGUAGE TemplateHaskell #-}
-- | Provides support for double-sided HTTP proxies.
module Cryptd.Lib.Tunnel
    ( TunnelHandle
    , TunnelIdentity(..)
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
import Network.TLS (TLSCtx, recvData, sendData)
import qualified Data.ByteString.Lazy.Char8 as LB

import Cryptd.Lib.HTTPSerial

-- | An identifier for distinguishing the current connection.
type Sequence = Integer

-- | A datatype representing the protocol inside the tunnel.
data Channel = ChannelRequest Sequence FullRequest -- ^ HTTP request from tunnel
             | ChannelResponse Sequence Response -- ^ HTTP response to tunnel
             | ChannelKeepalive -- ^ Keepalive packet

$(derive makeSerialize ''Channel)

-- | A Handle for talking from/to the Tunnel.
type TunnelHandle = TLSCtx Handle

-- | The identifier for a particular connection
data TunnelIdentity = TunnelIdentity
    { idSlave :: Maybe String
    , idInstance :: Maybe String
    }

-- | The status of the tunnel.
data TunnelStatus
    = Active TunnelIdentity
    -- ^ Tunnel is alive (with optional slave/instance ID when used on master)
    | Shutdown
    -- ^ Tunnel is requested to shut down the connection
    | Down
    -- ^ Tunnel is not available.

-- | The state representing the tunnel's 'Channel's and 'TunnelStatus'.
data TunnelState = TunnelState
    { inChannel :: TChan Channel -- ^ Data coming from tunnel
    , outChannel :: TChan Channel -- ^ Data going to tunnel
    , tunnelStatus :: TVar TunnelStatus
    , tunnelSeq :: TVar Sequence -- ^ Current sequence number
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

-- | Return not associated 'TunnelIdentity'.
noIdentity :: TunnelIdentity
noIdentity = TunnelIdentity Nothing Nothing

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
listener handle input = forkIO . forever $
    atomically . writeTChan input =<< getChunks (runGetPartial get)
  where
    getChunks getter = do
        raw <- recvData handle
        case getter raw of
             Fail e -> error $ "Can't decode packet from tunnel: " ++ e
             Partial g -> getChunks g
             Done result _ -> return result

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
    setActive _ = Active noIdentity

    finishSetDown = flip (>>) $ atomically (writeTVar (tunnelStatus state) Down)

    connectHandler io = onConnect cb handle state >>= flip when io

-- | Initialize all 'TVar's and 'TChan's associated with 'TunnelState'.
initializeState :: IO TunnelState
initializeState = do
    inChan <- newTChanIO
    outChan <- newTChanIO
    tstatus <- newTVarIO Down
    tseq <- newTVarIO 0
    return $ TunnelState inChan outChan tstatus tseq

-- | Create a new tunnel handler function using the given 'HandlerCallbacks'.
makeHandler :: HandlerCallbacks -> IO (TunnelState, TunnelHandle -> IO ())
makeHandler cb = do
    s <- initializeState
    return (s, handler cb s)
