-- | Thread safe logging using STM.
module Cryptd.Lib.Logger (forkWithLogger, Logger) where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, readTChan, writeTChan)

-- | A function for printing or logging errors.
type Logger a = a -> IO ()

-- | Fork action into the background and collect log messages.
forkWithLogger :: Logger a            -- ^ Logging function
               -> (Logger a -> IO ()) -- ^ Action to call
               -> IO ()
forkWithLogger logger run = newTChanIO >>= (>>)
    (forkIO . run . (.) atomically . writeTChan)
    (forever . (=<<) logger . atomically . readTChan)
