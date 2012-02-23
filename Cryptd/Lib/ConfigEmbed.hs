{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cryptd.Lib.ConfigEmbed
    ( Config(..)
    , injectConfig
    , privateKey
    , publicX509
    , secret
    , url
    , masterHost
    )
where

import Data.DeriveTH
import Data.FileEmbed (dummySpace, inject)
import Data.Serialize (encode, decode, Serialize(..))
import Data.Certificate.X509 (encodeCertificate, decodeCertificate, X509)
import Crypto.Types.PubKey.RSA (PrivateKey(..))
import Data.ByteString (ByteString)

data Config = Config
    { cPrivateKey :: PrivateKey
    , cPublicKey :: X509
    , cSecret :: String
    , cURL :: String
    , cMasterHost :: String
    } deriving (Show)

instance Serialize X509 where
    put = put . encodeCertificate
    get = do
        raw <- get
        return $ either error id $ decodeCertificate raw

$(derive makeSerialize ''PrivateKey)
$(derive makeSerialize ''Config)

injectConfig :: Config -> ByteString -> Maybe ByteString
injectConfig config = inject (encode config)

getConfig :: Config
getConfig =
    case decode $(dummySpace 16384) of
         Left  _ -> error "This as an unpatched template file, bailing out!"
         Right c -> c

privateKey :: PrivateKey
privateKey = cPrivateKey getConfig

publicX509 :: X509
publicX509 = cPublicKey getConfig

secret :: String
secret = cSecret getConfig

url :: String
url = cURL getConfig

masterHost :: String
masterHost = cMasterHost getConfig
