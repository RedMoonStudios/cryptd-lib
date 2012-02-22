module Cryptd.Master.CLI.Arch where

import qualified Data.Acid as Acid
import qualified Data.ByteString.Char8 as B

import Cryptd.Master.CLI.Options
import Cryptd.Master.Storage
import qualified Cryptd.Master.Storage.Architecture as S
import Cryptd.Master.ArchFile (updateArchFile)

getBinary :: CLI -> IO B.ByteString
getBinary ( ("binary", CLIValue binfile)
          : _ ) =
    B.readFile binfile
getBinary _ = B.getContents

handle :: Acid.AcidState S.Architecture -> CLI -> IO ()
handle st ( ("add", CLICmd)
          : ("archname", CLIValue aname)
          : r ) = do
    tpldata <- getBinary r
    Just path <- updateArchFile aname (Just tpldata)
    Acid.update st (S.CreateArch aname path)
handle st ( ("change", CLICmd)
          : ("archname", CLIValue aname)
          : r ) = do
    tpldata <- getBinary r
    Just path <- updateArchFile aname (Just tpldata)
    Acid.update st (S.ChangeTemplate aname path)
handle st ( ("delete", CLICmd)
          : ("archname", CLIValue aname)
          : _ ) = do
    Nothing <- updateArchFile aname Nothing
    Acid.update st (S.DeleteArch aname)
handle st _ = do
    alist <- Acid.query st S.ListArch
    mapM_ putStrLn [show (fst a) | a <- alist]

execute :: CLI -> IO ()
execute cli = do
    st <- openStorage S.emptyArch
    _ <- handle st cli
    closeStorage st
