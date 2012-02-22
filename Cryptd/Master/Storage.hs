module Cryptd.Master.Storage (openStorage, closeStorage, withStorage) where

import Data.Acid
import Data.Typeable (typeOf, Typeable)
import System.FilePath ((</>))

import Cryptd.Master.Constants (dataDir)

openStorage :: (Typeable st, IsAcidic st) => st -> IO (AcidState st)
openStorage stype = do
    datadir <- dataDir
    let fullpath = datadir </> show (typeOf stype)
    openLocalStateFrom fullpath stype

closeStorage :: AcidState st -> IO ()
closeStorage = closeAcidState

withStorage :: (Typeable st, IsAcidic st) => st -> (AcidState st -> IO a) -> IO a
withStorage stype fun = do
    state <- openStorage stype
    rval <- fun state
    closeStorage state
    return rval
