module Cryptd.Master.CLI.Partner where

import qualified Data.Acid as Acid

import Cryptd.Master.CLI.Options
import Cryptd.Master.Certs (createKeyPair, createSecret)
import Cryptd.Master.Storage
import qualified Cryptd.Master.Storage.Partner as S

handle :: Acid.AcidState S.Partner -> CLI -> IO ()
handle st ( ("add", CLICmd)
          : ("partnerid", CLIValue pid)
          : ("partnerurl", CLIValue purl)
          : _ ) = do
    pair <- createKeyPair "Partner Key"
    secret <- createSecret
    Acid.update st (S.CreatePartner pid (pdata pair secret))
  where
    pdata (pubkey, privkey) sec = S.PartnerData
        { S.pURL = purl
        , S.pName = ""
        , S.pSecret = sec
        , S.pPrivateKey = privkey
        , S.pPublicKey = pubkey
        }
handle st ( ("change", CLICmd)
          : ("partnerid", CLIValue pid)
          : ("partnerurl", CLIValue purl)
          : _ ) =
    Acid.update st (S.ChangeURL pid purl)
handle st ( ("delete", CLICmd)
          : ("partnerid", CLIValue pid)
          : _ ) =
    Acid.update st (S.DeletePartner pid)
handle st _ = do
    plist <- Acid.query st S.ListPartner
    mapM_ putStrLn [show p | p <- plist]

execute :: CLI -> IO ()
execute cli = do
    st <- openStorage S.emptyPartner
    _ <- handle st cli
    closeStorage st
