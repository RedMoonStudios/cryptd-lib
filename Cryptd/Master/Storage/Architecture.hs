{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Cryptd.Master.Storage.Architecture where

import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.Acid
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as Map

type ArchName = String
type ArchFile = FilePath

data Architecture = Architecture !(Map.Map ArchName ArchFile)
    deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Architecture)

listArch :: Query Architecture [(ArchName, ArchFile)]
listArch = do
    Architecture m <- ask
    return (Map.toList m)

getArch :: ArchName -> Query Architecture (Maybe ArchFile)
getArch aname = do
    Architecture m <- ask
    return (Map.lookup aname m)

changeTemplate :: ArchName -> ArchFile -> Update Architecture ()
changeTemplate aname newTpl = do
    Architecture m <- get
    put (Architecture (Map.update newVal aname m))
  where
    newVal _ = Just newTpl

createArch :: ArchName -> ArchFile -> Update Architecture ()
createArch aname afile = do
    Architecture m <- get
    put (Architecture (Map.insert aname afile m))

deleteArch :: ArchName -> Update Architecture ()
deleteArch aname = do
    Architecture m <- get
    put (Architecture (Map.delete aname m))

$(makeAcidic ''Architecture [ 'listArch
                            , 'getArch
                            , 'createArch
                            , 'deleteArch
                            , 'changeTemplate])

emptyArch :: Architecture
emptyArch = Architecture Map.empty
