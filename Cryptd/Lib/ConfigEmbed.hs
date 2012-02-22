{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cryptd.Lib.ConfigEmbed
    ( Config(..)
    , privateKey
    , publicX509
    , secret
    , url
    )
where

import Data.DeriveTH
import Data.FileEmbed (dummySpace)
import Data.Serialize (decode, Serialize(..))
import Data.Certificate.X509 (encodeCertificate, decodeCertificate, X509)
import Crypto.Types.PubKey.RSA (PrivateKey(..))

data Config = Config
    { cPrivateKey :: PrivateKey
    , cPublicKey :: X509
    , cSecret :: String
    , cURL :: String
    } deriving (Show)

instance Serialize X509 where
    put = put . encodeCertificate
    get = do
        raw <- get
        return $ either error id $ decodeCertificate raw

$(derive makeSerialize ''PrivateKey)
$(derive makeSerialize ''Config)

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
