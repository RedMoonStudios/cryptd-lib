{-# LANGUAGE OverloadedStrings #-}
module Cryptd.Lib.HTTP (request) where

import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
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

-- | Join 'URI's using the first one as a basepath.
--
-- The to be joined URI is not allowed to go up further than the base path.
joinURI :: URI -- ^ URI of the base path
        -> URI -- ^ URI to append to the base path
        -> URI -- ^ The new URI
joinURI base rel =
    newuri { uriPath = basepath +/+ newpath }
  where
    a +/+ b | a `isPrefixOf` b = b
            | otherwise = trimR a ++ "/" ++ trimL b
    trimL = dropWhile (== '/')
    trimR = reverse . trimL . reverse
    basepath = uriPath base
    newpath = uriPath newuri
    newuri = fromMaybe base $ relativeTo rel base

-- | Filter out certain headers.
--
-- Currently this is just 'HdrHost'.
blacklist :: [Header] -> [Header]
blacklist = filter (mh . hdrName)
  where
    mh HdrHost = False
    mh _ = True

-- | Perform a HTTP request using 'Request' and a base URI.
request :: String -- ^ The base URI
        -> Request -- ^ The request to send
        -> IO Response -- ^ The response from the webserver.
request rooturi req =
    return . toResponse =<< runHTTP
  where
    baseuri = fromMaybe
        (error $ "Can't parse base URI: " ++ rooturi)
        (parseAbsoluteURI rooturi)
    runHTTP = do
        body <- runResourceT . lazyConsume . requestBody $ req
        httpResponse <- simpleHTTP $ newReq body
        case httpResponse of
             Left err -> error $ show err
             Right v -> return v
    newReq body = H.Request
        { H.rqURI = joinURI baseuri . justify . parseRelativeReference $ uri
        , H.rqBody = LB.fromChunks body
        , H.rqHeaders = blacklist . toHeaders $ requestHeaders req
        , H.rqMethod = H.Custom $ B.unpack $ requestMethod req
        }
    uri = B.unpack $ rawPathInfo req
    justify = fromMaybe (error $ "Unable to parse URI: " ++ uri)
    toResponse res =
        responseLBS status headers body
      where
        convertStatusCode (v,c,r) = 100 * v + 10 * c + r

        status = HT.Status (convertStatusCode $ H.rspCode res)
                           (B.pack $ H.rspReason res)
        headers = fromHeaders $ H.rspHeaders res
        body = H.rspBody res
