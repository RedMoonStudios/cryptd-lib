{-# LANGUAGE OverloadedStrings #-}
module Cryptd.Master.Warp (app) where

import Data.Aeson.Parser (json)
import Data.Aeson (Value (Object, String), encode)
import Data.Conduit (ResourceT, ($$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.Text (pack, unpack)
import Data.Maybe (isJust, fromJust)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as Map

import Control.Monad.Trans (liftIO)
import Control.Exception (SomeException)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TChan
import Network.Wai (Application, Request(..), Response(..), responseLBS)
import Network.HTTP.Types (Status(statusCode), status400, status404, status500)
import qualified Control.Exception.Lifted as EL

import Cryptd.Lib.Tunnel (TunnelState(..), Channel(..))
import Cryptd.Lib.HTTPSerial (consumeRequest, FullRequest)

import Cryptd.Master.Storage.Partner (PartnerID)
import Cryptd.Master.Json (handle, build)
import Cryptd.Master.ConnMgr (ConnPool, getStates)

buildJson :: Status -> Value -> Response
buildJson status val = responseLBS
    status
    [("Content-Type", "application/json")]
    (encode val)

jsonError :: SomeException -> ResourceT IO Response
jsonError ex = return . buildJson status400 $
    Object $ Map.fromList [("message", String $ pack $ show ex)]

appMaster :: Application
appMaster Request{pathInfo = ["build", aname, pid]} = do
    -- special case: build command
    (status, output) <- liftIO $ build (unpack aname) (unpack pid)
    return $ responseLBS status headers output
  where
    headers = [("Content-Type", "application/x-executable; charset=binary")]
appMaster req = EL.handle jsonError $ do
    input <- getJson (requestMethod req) (pathInfo req)
    (status, output) <- liftIO $ handle (pathInfo req) (requestMethod req) input
    return $ buildJson status output
  where
    maybePathId [_, idval] = return $ Just (String idval)
    maybePathId _ = return Nothing

    getJson "GET" path = maybePathId path
    getJson "DELETE" path = maybePathId path
    getJson _ _ = return . Just =<< (requestBody req $$ sinkParser json)

appSlave :: TunnelState -> FullRequest -> ResourceT IO Response
appSlave ts fullReq = liftIO $ do
    atomically $ writeTChan (outChannel ts) (ChannelRequest fullReq)
    atomically $ do
        val <- readTChan (inChannel ts)
        case val of
             ChannelResponse r -> return r
             _                 -> retry

unicast :: [TunnelState] -> String -> Application
unicast [ts] _ req = appSlave ts =<< liftIO (consumeRequest req)
unicast _ inst _ = bailout $ "Instance " ++ (show inst) ++ " not found"

broadcast :: [TunnelState] -> Application
broadcast tss req = do
    fullReq <- liftIO $ consumeRequest req
    filterGood =<< mapM (`appSlave` fullReq) tss
  where
    filterGood :: [Response] -> ResourceT IO Response
    filterGood = pickBest . filter get200

    get200 (ResponseBuilder s _ _) | statusCode s == 200 = True
    get200 _                                             = False

    pickBest :: [Response] -> ResourceT IO Response
    pickBest [] = shriek "No slave was able to answer"
    pickBest (a : _) = return a

errResponse :: Status -> String -> ResourceT IO Response
errResponse status m = return $ responseLBS status
    [("Content-Type", "text/plain")] (LBS.pack m)

shriek :: String -> ResourceT IO Response
shriek = errResponse status500

bailout :: String -> ResourceT IO Response
bailout = errResponse status404

routeSlave :: ConnPool -> PartnerID -> Maybe String -> Application
routeSlave pool pid inst req = do
    maybeTSs <- liftIO $ getStates pool pid inst
    case maybeTSs of
         Just tss | isJust inst -> unicast tss (fromJust inst) req
                  | otherwise   -> broadcast tss req
         Nothing -> bailout "No such slave"

slaveRouter :: ConnPool -> Application
slaveRouter pool req@Request{requestHeaders = hdr} =
    case lookup "X-SlaveID" hdr of
         Just pid -> routeSlave pool (BS.unpack pid) inst req
                     where inst = fmap BS.unpack $ lookup "X-Instance" hdr
         Nothing -> bailout "X-SlaveID missing"

app :: ConnPool -> Application
app pool req@Request{requestHeaders = hdr}
    | ("X-Cryptd", "master") `elem` hdr = appMaster req
    | otherwise = slaveRouter pool req
