module Cryptd.Master.ArchFile (updateArchFile) where

import qualified Data.ByteString.Char8 as B
import Control.Monad (when)
import System.FilePath ((</>))
import System.Directory ( removeFile
                        , doesFileExist
                        , createDirectoryIfMissing
                        )

import Cryptd.Master.Constants (dataDir)

getArchFilePath :: String -> IO FilePath
getArchFilePath arch = do
    datadir <- dataDir
    let basedir = datadir </> "files"
    _ <- createDirectoryIfMissing True basedir
    return $ basedir </> arch

updateArchFile :: String -> Maybe B.ByteString -> IO (Maybe FilePath)
updateArchFile name (Just new) = do
    arch <- getArchFilePath name
    exists <- doesFileExist arch
    when exists $ removeFile arch
    _ <- B.writeFile arch new
    return $ Just arch
updateArchFile name Nothing = do
    arch <- getArchFilePath name
    _ <- removeFile arch
    return Nothing
