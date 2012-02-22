module Cryptd.Master.CLI.Build where

import Control.Monad
import Data.Maybe
import Data.FileEmbed (inject)
import Data.Serialize (encode)
import qualified Data.Acid as Acid
import qualified Data.ByteString.Char8 as B

import Cryptd.Lib.ConfigEmbed (Config(..))
import Cryptd.Master.CLI.Options
import Cryptd.Master.Storage
import qualified Cryptd.Master.Storage.Partner as P
import qualified Cryptd.Master.Storage.Architecture as A

writeBinary :: CLI -> B.ByteString -> IO ()
writeBinary ( ("binary", CLIValue binfile)
          : _ ) =
    B.writeFile binfile
writeBinary _ = B.putStr

getPartner :: P.PartnerID -> IO (Maybe P.PartnerData)
getPartner pid =
    withStorage P.emptyPartner $ \st ->
        Acid.query st $ P.GetPartner pid

getArch :: A.ArchName -> IO (Maybe A.ArchFile)
getArch aname =
    withStorage A.emptyArch $ \st ->
        Acid.query st $ A.GetArch aname

patchBinary :: P.PartnerData -> B.ByteString -> Maybe B.ByteString
patchBinary pdata =
    inject (encode config)
  where
    config = Config
        { cPrivateKey = P.pPrivateKey pdata
        , cPublicKey = P.pPublicKey pdata
        , cSecret = P.pSecret pdata
        , cURL = P.pURL pdata
        }

execute :: CLI -> IO ()
execute ( ("partnerid", CLIValue pid)
        : ("archname", CLIValue aname)
        : r ) = do
    buildPartner <- liftM fromJust $ getPartner pid
    buildArch <- liftM fromJust $ getArch aname
    contents <- B.readFile buildArch
    writeBinary r $ fromJust $ patchBinary buildPartner contents
execute _ = do
    error "Invalid options, needed two: partnerid and archname"
