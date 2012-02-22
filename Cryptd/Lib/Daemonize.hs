{-# LANGUAGE CPP #-}
module Cryptd.Lib.Daemonize (daemonize) where

#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS) && !defined(_WIN32)
import Control.Exception (bracket)

import System.FilePath.Posix ((</>))
import System.Directory (getTemporaryDirectory)
import System.IO (SeekMode(AbsoluteSeek))
import System.Exit
import System.Posix

daemonize :: String -> IO () -> IO ()
daemonize lockName fun =
    setFileCreationMask 0 >> forkProcess outer >>
        exitImmediately ExitSuccess
  where
    outer = createSession >> forkProcess inner >>
        exitImmediately ExitSuccess
    inner = changeWorkingDirectory "/" >> closeFds >>
        installHandler sigHUP Ignore Nothing >>
            bracket (acquireLock lockName) releaseLock (\_ -> fun)
    closeFds = do
        devNull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
        mapM_ (connectWith devNull) [stdInput, stdOutput, stdError]
      where
        connectWith oldFd newFd = closeFd newFd >> dupTo oldFd newFd

getLockFile :: String -> IO FilePath
getLockFile ln = fmap (</> (ln ++ ".lock")) getTemporaryDirectory

acquireLock :: String -> IO (Fd, FilePath)
acquireLock lockName = do
    lfile <- getLockFile lockName
    fd <- openFd lfile ReadWrite (Just modes) defaultFileFlags
    setLock fd (WriteLock, AbsoluteSeek, 0, 0)
    return (fd, lfile)
  where
    modes = unionFileModes ownerReadMode ownerWriteMode

releaseLock :: (Fd, FilePath) -> IO ()
releaseLock (fd, lfile) = closeFd fd >> removeLink lfile

#else
daemonize :: String -> IO () -> IO ()
daemonize _ = id
#endif
