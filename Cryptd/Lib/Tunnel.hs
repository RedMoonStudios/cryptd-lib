{-# LANGUAGE TemplateHaskell #-}
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

data Channel = ChannelRequest FullRequest
             | ChannelResponse Response
             | ChannelKeepalive

$(derive makeSerialize ''Channel)

type TunnelHandle = TLSCtx Handle

data TunnelStatus = Active (Maybe String) | Shutdown | Down

data TunnelState = TunnelState
    { inChannel :: TChan Channel -- Data coming from tunnel
    , outChannel :: TChan Channel -- Data going to tunnel
    , tunnelStatus :: TVar TunnelStatus
    }

data HandlerCallbacks = HandlerCallbacks
    { onConnect :: TunnelHandle -> TunnelState -> IO Bool
    , onRecv :: TunnelHandle -> TunnelState -> IO ()
    , onLoop :: TunnelHandle -> TunnelState -> IO ()
    }

noCallbacks :: HandlerCallbacks
noCallbacks = HandlerCallbacks
    { onConnect = \_ _ -> return True
    , onRecv = \_ _ -> return ()
    , onLoop = \_ _ -> return ()
    }

updateTVar :: TVar a -> (a -> a) -> STM ()
updateTVar dest fun =
    readTVar dest >>= writeTVar dest . fun

listener :: TunnelHandle -> TChan Channel -> IO ThreadId
listener handle input =
    forkIO $ forever $ do
        raw <- recvData' handle
        let packet = decode (B.concat $ LB.toChunks raw)
        case packet of
             Left e -> error $ "Can't decode packet from Tunnel" ++ e
             Right p -> atomically $ writeTChan input p

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

initializeState :: IO TunnelState
initializeState = do
    inChan <- newTChanIO
    outChan <- newTChanIO
    tstatus <- newTVarIO Down
    return $ TunnelState inChan outChan tstatus

makeHandler :: HandlerCallbacks -> IO (TunnelState, TunnelHandle -> IO ())
makeHandler cb = do
    s <- initializeState
    return (s, handler cb s)
