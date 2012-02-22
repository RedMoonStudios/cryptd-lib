{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cryptd.Master.Storage.Partner where

import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import System.Random (mkStdGen, randoms)
import qualified Data.Map as Map

import Data.Certificate.X509 (encodeCertificate, decodeCertificate, X509)
import Crypto.Types.PubKey.RSA (PrivateKey(private_qinv))

type Secret = String

type PartnerID = String

data PartnerDataWithoutSecret =
    PartnerDataWithoutSecret String String PrivateKey X509
    deriving (Show, Typeable)

data PartnerData = PartnerData
    { pName :: String
    , pURL :: String
    , pSecret :: Secret
    , pPrivateKey :: PrivateKey
    , pPublicKey :: X509
    }
    deriving (Show, Typeable)

data Partner = Partner !(Map.Map PartnerID PartnerData)
    deriving (Show, Typeable)

instance SafeCopy X509 where
    putCopy = contain . safePut . encodeCertificate
    getCopy = contain $ do
        raw <- safeGet
        return $ either error id $ decodeCertificate raw

$(deriveSafeCopy 0 'base ''PrivateKey)
$(deriveSafeCopy 0 'base ''PartnerDataWithoutSecret)
$(deriveSafeCopy 1 'extension ''PartnerData)
$(deriveSafeCopy 0 'base ''Partner)

instance Migrate PartnerData where
    type MigrateFrom PartnerData = PartnerDataWithoutSecret
    migrate (PartnerDataWithoutSecret n u p x) =
        PartnerData n u s p x
      where s = take 100 $ randoms gen :: String
            gen = mkStdGen $ fromIntegral (private_qinv p)

listPartner :: Query Partner [(PartnerID, PartnerData)]
listPartner = do
    Partner m <- ask
    return (Map.toList m)

getPartner :: PartnerID -> Query Partner (Maybe PartnerData)
getPartner pid = do
    Partner m <- ask
    return (Map.lookup pid m)

getIDWithSecret :: Secret -> Query Partner (Maybe PartnerID)
getIDWithSecret sec = do
    Partner m <- ask
    let matching = Map.toList $ Map.filter findSecret m
    case matching of
         [] -> return Nothing
         ((match, _):_) -> return $ Just match
  where
    findSecret p | pSecret p == sec = True
                 | otherwise = False

changeURL :: PartnerID -> String -> Update Partner ()
changeURL pid new_url = do
    Partner m <- get
    put (Partner (Map.update newVal pid m))
  where
    newVal v = Just v { pURL = new_url }

createPartner :: PartnerID -> PartnerData -> Update Partner ()
createPartner pid pdata = do
    Partner m <- get
    put (Partner (Map.insert pid pdata m))

deletePartner :: PartnerID -> Update Partner ()
deletePartner pid = do
    Partner m <- get
    put (Partner (Map.delete pid m))

$(makeAcidic ''Partner [ 'listPartner
                       , 'getPartner
                       , 'getIDWithSecret
                       , 'createPartner
                       , 'deletePartner
                       , 'changeURL
                       ])

emptyPartner :: Partner
emptyPartner = Partner Map.empty
