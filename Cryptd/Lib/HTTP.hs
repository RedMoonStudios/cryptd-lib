{-# LANGUAGE OverloadedStrings #-}
-- | Perform HTTP requests.
module Cryptd.Lib.HTTP (request) where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Conduit (runResourceT)
import Data.Conduit.Lazy (lazyConsume)
import Network.Wai (Request(..), Response(..), responseLBS)
import Network.URI (URI(..), parseRelativeReference,
                    relativeTo, parseAbsoluteURI)
import Network.HTTP (simpleHTTP)
import Network.HTTP.Headers (Header(..), HeaderName(..), hdrName, headerMap)
import qualified Network.HTTP.Types as HT
import qualified Network.HTTP.Base as H
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Conduit as HC

-- | Convert a 'HeaderName' into the representation used by "HT".
fromHeaderName :: HeaderName -> CI.CI HT.Ascii
fromHeaderName (HdrCustom s) = CI.mk . B.pack $ s
fromHeaderName hn =
    CI.mk . B.pack . fst . head $ filter ff headerMap
  where
    ff = (== hn) . snd

-- | Convert a representation used by "HT" into a 'HeaderName'.
toHeaderName :: CI.CI HT.Ascii -> HeaderName
toHeaderName ascii =
    getRes result
  where
    getRes r | null r = HdrCustom unpacked
             | otherwise = snd $ head r
    unpacked = B.unpack . CI.original $ ascii
    result = filter ff headerMap
    ff = (== unpacked) . fst

-- | Convert a list of 'Header's into "HT" headers.
fromHeaders :: [Header] -> [HT.Header]
fromHeaders =
    map hdrConv
  where
    hdrConv (Header key value) =
        (fromHeaderName key, B.pack value)

-- | Convert "HT" headers unto a list of 'Header's.
toHeaders :: [HT.Header] -> [Header]
toHeaders =
    map hdrConv
  where
    hdrConv (key, value) =
        Header (toHeaderName key) (B.unpack value)

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
    killdots [] a = reverse a
    killdots ( '.' : '.' : '/' : r ) a | isSeg a = killdots r  ('/' : trimPath a)
    killdots ( '.' : '.' : [] )      a | isSeg a = killdots [] ('/' : trimPath a)
    killdots ( '/' : r )             a = killdots r  ('/' : trimL a)
    killdots ( h : r )               a = killdots r  (h : a)

    isSeg []        = True
    isSeg ('/' : _) = True
    isSeg _         = False

    trimPath = trimL . dropWhile (/= '/') . trimL

    trimL = dropWhile (== '/')
    trimR = reverse . trimL . reverse

-- | Filter out certain headers.
--
-- Currently this is just 'HdrHost'.
blacklist :: [Header] -> [Header]
blacklist = filter (mh . hdrName)
  where
    mh HdrHost = False
    mh _ = True

-- | Perform a HTTP request using 'Request' and a base URL.
request :: String -- ^ The base URL
        -> Request -- ^ The request to send
        -> IO Response -- ^ The response from the webserver.
request rooturl req = do
    baseReq <- HC.parseUrl rooturl
    newReq <- mkReq baseReq req
    r <- HC.withManager (HC.lbsResponse . HC.http newReq)
    return $ responseLBS (HC.statusCode r) (HC.responseHeaders r) (HC.responseBody r)
  where
    mkReq b r = do
        body <- runResourceT . lazyConsume . requestBody $ r
        return $ b { HC.method = requestMethod r
                   , HC.path = B.pack $ joinURL rooturl $ B.unpack $ rawPathInfo r
                   , HC.queryString = rawQueryString r
                   , HC.requestHeaders = requestHeaders r
                   , HC.requestBody = HC.RequestBodyLBS . LB.fromChunks $ body
                   }
