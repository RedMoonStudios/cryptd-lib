module Cryptd.Master.Constants (dataDir) where

import System.Directory (getAppUserDataDirectory)

dataDir :: IO FilePath
dataDir = getAppUserDataDirectory "redmoon-cryptd"
