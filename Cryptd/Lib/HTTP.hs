{-# LANGUAGE OverloadedStrings #-}
-- | Perform HTTP requests.
module Cryptd.Lib.HTTP (request, HC.withManager) where

import Data.List (isPrefixOf)
import Data.Function (on)
import Data.Conduit (runResourceT, ($$))
import Data.Conduit.List (consume)
import Control.Monad.Trans (liftIO)
import Network.Wai (Request(..), Response(..), responseLBS)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Network.HTTP.Conduit as HC

-- | Join URLs using the first one as a basepath.
--
-- The to be joined URL is not allowed to go up further than the base path.
joinURL :: String -- ^ URI of the base path
        -> String -- ^ URI to append to the base path
        -> String -- ^ The new URI
joinURL =
    join `on` sanitize
  where
    sanitize = flip killdots []
    join a b | a `isPrefixOf` b = b
             | otherwise = trimR a ++ "/" ++ trimL b

    killdots [] [] = "/"
    killdots [] a  = reverse a
    killdots ('.':'.':'/' : r)  a | isSeg a = killdots r  ('/' : trimPath a)
    killdots ('.':'.'     : []) a | isSeg a = killdots [] ('/' : trimPath a)
    killdots ('/'         : r)  a = killdots r ('/' : trimL a)
    killdots (h           : r)  a = killdots r (h : a)

    isSeg []        = True
    isSeg ('/' : _) = True
    isSeg _         = False

    trimPath = trimL . dropWhile (/= '/') . trimL

    trimL = dropWhile (== '/')
    trimR = reverse . trimL . reverse

-- | Perform a HTTP request using 'Request' and a base URL.
request :: HC.Manager  -- ^ The manager for the conduit
        -> String      -- ^ The base URL
        -> Request     -- ^ The request to send
        -> IO Response -- ^ The response from the webserver
request manager rooturl req = runResourceT $ do
    baseReq <- HC.parseUrl rooturl
    newReq <- liftIO $ mkReq baseReq req
    r <- HC.httpLbs newReq manager
    return $ responseLBS (HC.statusCode r)
                         (HC.responseHeaders r)
                         (HC.responseBody r)
  where
    mkReq b r = do
        body <- runResourceT $ requestBody r $$ consume
        return $ b { HC.method = requestMethod r
                   , HC.path = newpath
                   , HC.queryString = rawQueryString r
                   , HC.requestHeaders = requestHeaders r
                   , HC.requestBody = HC.RequestBodyLBS . LB.fromChunks $ body
                   }
      where
        newpath = B.pack $ (joinURL `on` B.unpack) (HC.path b) (rawPathInfo r)
