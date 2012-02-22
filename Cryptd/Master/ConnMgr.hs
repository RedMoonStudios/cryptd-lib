module Cryptd.Master.ConnMgr (ConnPool, makeMasterHandler, getStates) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Network.TLS (recvData')
import Data.Acid (query)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as LB

import Cryptd.Lib.Tunnel (TunnelHandle, TunnelState(..), TunnelStatus(..),
                          HandlerCallbacks(..), makeHandler)

import qualified Cryptd.Master.Storage.Partner as SP
import Cryptd.Master.Storage (withStorage)

data Connection = Connection
    { cInstance :: Maybe String
    , cState :: TunnelState
    }

type ConnMap = M.Map SP.PartnerID [Connection]
type ConnPool = TVar ConnMap

initConnection :: Maybe String -> TunnelState -> Connection
initConnection inst st = Connection
    { cInstance = inst
    , cState = st
    }

addConnection :: SP.PartnerID -> Connection -> ConnMap -> ConnMap
addConnection pid conn =
    M.insertWith' replace pid [conn]
  where
    replace old new = filter (removeSame $ head new) old ++ new
    removeSame Connection { cInstance = Just x }
               Connection { cInstance = Just y } | x == y = False
    removeSame _ _ = True

requestInit :: ConnPool -> TunnelHandle -> TunnelState -> IO Bool
requestInit pool handle state = do
    sec <- recvData' handle
    inst <- recvData' handle
    maybepid <- withStorage SP.emptyPartner $ \st ->
        query st (SP.GetIDWithSecret $ LB.unpack sec)
    case maybepid of
         Just pid -> setActive pid (rawToMaybe $ LB.unpack inst) >> return True
         Nothing -> return False
  where
    setActive p i = atomically $ do
        writeTVar (tunnelStatus state) (Active $ Just p)
        m <- readTVar pool
        writeTVar pool (addConnection p (initConnection i state) m)

    rawToMaybe ('\001' : d) = Just d
    rawToMaybe _            = Nothing

masterHandler :: ConnPool -> HandlerCallbacks -> TunnelHandle -> IO ()
masterHandler pool cb handle = do
    (_, handler) <- makeHandler newCb
    handler handle
  where
    newCb = cb { onConnect = requestInit pool }

makeMasterHandler :: HandlerCallbacks -> IO (ConnPool, TunnelHandle -> IO ())
makeMasterHandler cb = do
    pool <- newTVarIO M.empty
    return (pool, masterHandler pool cb)

getStates :: ConnPool -> SP.PartnerID -> Maybe String -> IO (Maybe [TunnelState])
getStates pool pid inst =
    atomically $ return . getTS =<< readTVar pool
  where
    getTS :: ConnMap -> Maybe [TunnelState]
    getTS = (=<<) (return . map cState . filter lookupInst) . M.lookup pid

    lookupInst :: Connection -> Bool
    lookupInst Connection { cInstance = i } | i == inst = True
    lookupInst _                                        = False
