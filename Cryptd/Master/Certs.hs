module Cryptd.Master.Certs (getMasterPair, createKeyPair, createSecret) where

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import Data.Char (ord)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

import Data.Time.Clock (getCurrentTime, addUTCTime, NominalDiffTime)

import Data.Certificate.PEM (findPEM, parsePEMs)
import Data.Certificate.KeyRSA (decodePrivate)
import Data.Certificate.X509 (decodeCertificate, X509)
import Crypto.Types.PubKey.RSA (PrivateKey)

import OpenSSL (withOpenSSL)
import OpenSSL.Random (randBytes)
import OpenSSL.EVP.PKey (KeyPair)
import OpenSSL.RSA (generateRSAKey')
import OpenSSL.PEM (writePKCS8PrivateKey, writeX509)
import qualified OpenSSL.X509 as OX509

import System.FilePath ((</>))
import System.Directory ( doesFileExist
                        , createDirectoryIfMissing
                        )

import Cryptd.Master.Constants (dataDir)

type CommonName = String

certDetails :: CommonName -> [(String, String)]
certDetails cn =
    [ ("C", "DE")
    , ("O", "RedMoon Studios GmbH & Co KG")
    , ("OU", "MoonID")
    , ("CN", cn)
    ]

keySize :: Int
keySize = 2048

validSeconds :: NominalDiffTime
validSeconds = 10 * 365 * 24 * 60 * 60 -- 10 years

createSecret :: IO String
createSecret = return . B.unpack =<< randBytes 100

createCert :: KeyPair kp => CommonName -> kp -> IO OX509.X509
createCert cn kp = do
    x509 <- OX509.newX509
    OX509.setVersion x509 2
    OX509.setSerialNumber x509 1
    OX509.setIssuerName x509 $ certDetails cn
    OX509.setSubjectName x509 $ certDetails cn
    OX509.setNotBefore x509 =<< liftM (addUTCTime (-1)) getCurrentTime
    OX509.setNotAfter x509 =<< liftM (addUTCTime validSeconds) getCurrentTime
    OX509.setPublicKey x509 kp
    OX509.signX509 x509 kp Nothing
    return x509

createOpenSSLKeyPair :: CommonName -> IO (String, String)
createOpenSSLKeyPair cn = withOpenSSL $ do
    pair <- generateRSAKey' keySize 65537
    cert <- createCert cn pair
    pubkey <- writeX509 cert
    privkey <- writePKCS8PrivateKey pair Nothing
    return (pubkey, privkey)

createKeyPair :: CommonName -> IO (X509, PrivateKey)
createKeyPair cn =
    liftM fromOpenSSLPair (createOpenSSLKeyPair cn)

createOrGetMasterPair :: CommonName -> IO (String, String)
createOrGetMasterPair cn = do
    datadir <- dataDir
    let basedir = datadir </> "ssl"
    _ <- createDirectoryIfMissing True basedir
    let keyFile = basedir </> "master.key"
    let certFile = basedir </> "master.pem"
    certExists <- doesFileExist certFile
    keyExists <- doesFileExist keyFile
    if not $ keyExists && certExists
       then do
           (pub, priv) <- createOpenSSLKeyPair cn
           writeFile certFile pub
           writeFile keyFile priv
           return (pub, priv)
       else do
           pub <- readFile certFile
           priv <- readFile keyFile
           return (pub, priv)

unpadPKCS5 :: B.ByteString -> B.ByteString
unpadPKCS5 key =
    B.drop padLen key
  where
    rawOffset = B.take 2 (B.drop 24 key)
    padNum = ord . B.index rawOffset
    offset = padNum 0 * 256 + padNum 1
    padLen = B.length key - offset

fromOpenSSLPair :: (String, String) -> (X509, PrivateKey)
fromOpenSSLPair (pub, priv) =
    (finalCert, finalPriv)
  where
    pemDecode crt =
        fromMaybe (error "Unable to parse PEM encoded X509 key.")
                  (findPEM "TRUSTED CERTIFICATE" $ parsePEMs crt)
    x509Decode crtdata =
        case decodeCertificate $ LB.fromChunks [crtdata] of
             Left err -> error $ "Unable to decode X509 data: " ++ err
             Right x -> x
    finalCert = x509Decode . pemDecode $ B.pack pub

    pemRSADecode key =
        fromMaybe (error "Unable to parse PEM encoded private key.")
                  (findPEM "PRIVATE KEY" $ parsePEMs key)
    rsaDecode keydata =
        case decodePrivate $ LB.fromChunks [keydata] of
             Left err -> error $ "Unable to decode key data: " ++ err
             Right (_, pk) -> pk
    finalPriv = rsaDecode . unpadPKCS5 . pemRSADecode $ B.pack priv

getMasterPair :: IO (X509, PrivateKey)
getMasterPair =
    liftM fromOpenSSLPair (createOrGetMasterPair cn)
  where
    cn = "Root Certificate"
