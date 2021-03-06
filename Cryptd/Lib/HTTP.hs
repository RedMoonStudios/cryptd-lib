{-# LANGUAGE OverloadedStrings #-}
-- | Perform HTTP requests.
module Cryptd.Lib.HTTP (request, HC.withManager) where

import Data.List (isPrefixOf, nubBy)
import Data.Function (on)
import Data.Conduit (runResourceT, ($$))
import Data.Conduit.List (consume)
import Control.Monad.Trans (liftIO)
import Network.Wai (Request(..), Response(..), responseLBS)
import Network.HTTP.Types (RequestHeaders)
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

-- | Strip off and unify headers which are ambiguous for requests.
stripHeaders :: RequestHeaders -> RequestHeaders
stripHeaders = unify . filter (ambiguous . fst)
  where unify = nubBy ((==) `on` fst)
        ambiguous "Host"            = False
        ambiguous "Content-Length"  = False
        ambiguous "Accept-Encoding" = False
        ambiguous _                 = True

-- | Perform a HTTP request using 'Request' and a base URL.
request :: HC.Manager  -- ^ The manager for the conduit
        -> String      -- ^ The base URL
        -> Request     -- ^ The request to send
        -> IO Response -- ^ The response from the webserver
request manager rooturl req = runResourceT $ do
    baseReq <- HC.parseUrl rooturl
    newReq <- liftIO $ mkReq baseReq req
    r <- HC.httpLbs newReq manager
    return $ responseLBS (HC.responseStatus r)
                         (HC.responseHeaders r)
                         (HC.responseBody r)
  where
    mkReq b r = do
        body <- runResourceT $ requestBody r $$ consume
        return $ b { HC.method = requestMethod r
                   , HC.path = newpath
                   , HC.queryString = rawQueryString r
                   , HC.requestHeaders = stripHeaders . requestHeaders $ r
                   , HC.requestBody = HC.RequestBodyLBS . LB.fromChunks $ body
                   , HC.checkStatus = \_ _ -> Nothing -- Allow non-2xx, too.
                   }
      where
        newpath = B.pack $ (joinURL `on` B.unpack) (HC.path b) (rawPathInfo r)
