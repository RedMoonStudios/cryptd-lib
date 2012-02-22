{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cryptd.Master.Json (handle, build) where

import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Char (toLower)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Text (Text, pack)
import Network.HTTP.Types (Method, Status, status200, status404, status405)
import qualified Data.HashMap.Strict as Map
import qualified Data.Acid as Acid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import Cryptd.Master.Certs (createKeyPair, createSecret)
import Cryptd.Master.Storage (withStorage)
import Cryptd.Master.ArchFile (updateArchFile)
import qualified Cryptd.Master.Storage.Architecture as SA
import qualified Cryptd.Master.Storage.Partner as SP
import qualified Cryptd.Master.CLI.Build as Build

data Partner = Partner
    { pId :: SP.PartnerID
    , pName :: String
    , pUrl :: String
    }

$(deriveJSON (map toLower . drop 1) ''Partner)

data Arch = Arch
    { aName :: SA.ArchName
    , aBinary :: Maybe B.ByteString
    }

$(deriveJSON (map toLower . drop 1) ''Arch)

type HandlerReturn = (Status, Value)
type MethodHandler = Method -> Maybe Value -> IO HandlerReturn


instance ToJSON (SP.PartnerID, SP.PartnerData) where
    toJSON (pid, pdata) = toJSON
        Partner
        { pId = pid
        , pName = SP.pName pdata
        , pUrl = SP.pURL pdata
        }

instance ToJSON (SA.ArchName, SA.ArchFile) where
    toJSON (aname, _) = toJSON
        Arch
        { aName = aname
        , aBinary = Nothing
        }

raiseStatus :: Status -> String -> IO HandlerReturn
raiseStatus status msg =
    return (status, Object $ Map.fromList [("message", String $ pack msg)])

forceFromJSON :: FromJSON a => Value -> a
forceFromJSON raw =
    case fromJSON raw of
         Success a -> a
         Error e -> error e

build :: String -> String -> IO (Status, LB.ByteString)
build aname pid = do
    partner <- liftM fromJust $ Build.getPartner pid
    arch <- liftM fromJust $ Build.getArch aname
    contents <- B.readFile arch
    let response = fromJust $ Build.patchBinary partner contents
    return (status200, LB.fromChunks [response])

handlePartner :: Acid.AcidState SP.Partner -> MethodHandler
handlePartner st "GET" Nothing = do
    plist <- Acid.query st SP.ListPartner
    return (status200, toJSON plist)
handlePartner st "GET" (Just pid) = do
    let pidstr = forceFromJSON pid
    maybePartner <- Acid.query st (SP.GetPartner pidstr)
    case maybePartner of
         Just p -> return (status200, toJSON (pidstr, p))
         Nothing -> raiseStatus status404 "Partner not found!"
handlePartner st "POST" (Just input) = do
    secret <- createSecret
    pair <- createKeyPair "Partner Key"
    _ <- Acid.update st (SP.CreatePartner (pId jsonInput) (pdata pair secret))
    return (status200, input)
  where
    jsonInput = forceFromJSON input
    pdata (pubkey, privkey) sec = SP.PartnerData
        { SP.pURL = pUrl jsonInput
        , SP.pName = pName jsonInput
        , SP.pSecret = sec
        , SP.pPrivateKey = privkey
        , SP.pPublicKey = pubkey
        }
handlePartner st "PUT" (Just input) = do
    _ <- Acid.update st (SP.ChangeURL (pId jsonInput) (pUrl jsonInput))
    return (status200, input)
  where
    jsonInput = forceFromJSON input
handlePartner st "DELETE" (Just pid) = do
    _ <- Acid.update st $ SP.DeletePartner (forceFromJSON pid)
    return (status200, String "OK")
handlePartner _ _ _ = do
    return (status405, String "Invalid method")

handleArch :: Acid.AcidState SA.Architecture -> MethodHandler
handleArch st "GET" Nothing = do
    alist <- Acid.query st SA.ListArch
    return (status200, toJSON alist)
handleArch st "GET" (Just aname) = do
    let archstr = forceFromJSON aname
    maybeArch <- Acid.query st (SA.GetArch archstr)
    case maybeArch of
         Just a -> return (status200, toJSON (archstr, a))
         Nothing -> raiseStatus status404 "Architecture not found!"
handleArch st m (Just input) | m `elem` ["POST", "PUT"] = do
    Just path <- updateArchFile (aName jsonInput) (aBinary jsonInput)
    _ <- Acid.update st (SA.CreateArch (aName jsonInput) path)
    return (status200, input)
  where
    jsonInput = forceFromJSON input
handleArch st "DELETE" (Just aname) = do
    Nothing <- updateArchFile (forceFromJSON aname) Nothing
    _ <- Acid.update st $ SA.DeleteArch (forceFromJSON aname)
    return (status200, String "OK")
handleArch _ _ _ = do
    return (status405, String "Invalid method")

handle :: [Text] -> Method -> Maybe Value -> IO HandlerReturn
handle ("partner":_) m v = withStorage SP.emptyPartner $ \st -> handlePartner st m v
handle ("arch":_) m v = withStorage SA.emptyArch $ \st -> handleArch st m v
handle _ _ _ = raiseStatus status404 "URL not found!"
