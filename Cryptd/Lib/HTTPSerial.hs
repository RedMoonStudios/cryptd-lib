{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings,
             TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Serialize HTTP data into 'BS.ByteString's.
module Cryptd.Lib.HTTPSerial
    ( decode, encode
    , consumeRequest, supplyRequest, FullRequest
    , Request(..), Response(..)
    )
where

import Data.DeriveTH
import Data.Serialize
import Data.CaseInsensitive (CI, original, mk)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vault (Vault, empty)
import Data.Conduit (Source, runResourceT)
import Data.Conduit.List (sourceNull, sourceList)
import Data.Conduit.Lazy (lazyConsume)
import Network.Socket (SockAddr(..), PortNumber(..))
import Network.Wai (Request(..), Response(..))
import Network.HTTP.Types (HttpVersion(..), Status(..), Ascii)
import Blaze.ByteString.Builder (fromByteString, toByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

instance Serialize Text where
    put = put . encodeUtf8
    get = return . decodeUtf8 =<< get

instance Serialize (CI Ascii) where
    put = put . original
    get = return . mk =<< get

instance Serialize Vault where
    put _ = return ()
    get = return empty

instance (Serialize (Source IO BS.ByteString)) where
    put _ = return ()
    get = return sourceNull

$(derive makeSerialize ''HttpVersion)
$(derive makeSerialize ''PortNumber)
$(derive makeSerialize ''SockAddr)

$(derive makeSerialize ''Request)

$(derive makeSerialize ''Status)

instance Serialize Response where
    put (ResponseBuilder s h b) = put s >> put h >> put (toByteString b)
    put _ = put BS.empty

    get = do
        status <- get
        headers <- get
        builder <- get
        return $ ResponseBuilder status headers (fromByteString builder)

-- | A type separating 'Request' and the body.
--
-- This is to correctly serialize conduits which involve IO actions, as cereal
-- is implemented entirely pure.
data FullRequest = FullRequest Request LBS.ByteString
$(derive makeSerialize ''FullRequest)

-- | Turn a 'Request' into a 'FullRequest' consuming the body from 'Request'
consumeRequest :: Request -> IO FullRequest
consumeRequest req =
    fmap (FullRequest req . LBS.fromChunks)
         (runResourceT . lazyConsume . requestBody $ req)

-- | Turn a 'FullRequest' into a 'Request', supplying a new 'Source' to it.
supplyRequest :: FullRequest -> IO Request
supplyRequest (FullRequest req body) =
    return $ req { requestBody = sourceList $ LBS.toChunks body }
