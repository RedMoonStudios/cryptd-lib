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

-- | Configuration to be embedded into a slave binary.
data Config = Config
    { cPrivateKey :: PrivateKey -- ^ SSL private key
    , cPublicKey :: X509 -- ^ SSL certificate
    , cSecret :: String -- ^ Secret value
    , cURL :: String -- ^ URL to the external backend
    , cMasterHost :: String -- ^ Hostname/IP of the master server
    } deriving (Show)

instance Serialize X509 where
    put = put . encodeCertificate
    get = do
        raw <- get
        return $ either error id $ decodeCertificate raw

$(derive makeSerialize ''PrivateKey)
$(derive makeSerialize ''Config)

-- | Inject 'Config' values into a unpatched slave executable.
injectConfig :: Config
             -> ByteString
             -- ^ The unpatched executable
             -> Maybe ByteString
             -- ^ If successful, return the new executable.
injectConfig config = inject (encode config)

-- | Get 'Config' values patched into the current executable.
getConfig :: Config
getConfig =
    case decode $(dummySpace 16384) of
         Left  _ -> error "This as an unpatched template file, bailing out!"
         Right c -> c

-- | Get patched-in 'PrivateKey'.
privateKey :: PrivateKey
privateKey = cPrivateKey getConfig

-- | Get patched-in 'X509' certificate.
publicX509 :: X509
publicX509 = cPublicKey getConfig

-- | Get patched-in secret value.
secret :: String
secret = cSecret getConfig

-- | Get patched-in 'cURL'.
url :: String
url = cURL getConfig

-- | Get patched-in cMasterHost.
masterHost :: String
masterHost = cMasterHost getConfig
