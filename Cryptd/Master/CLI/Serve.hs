{-# LANGUAGE OverloadedStrings #-}
module Cryptd.Master.CLI.Serve where

import Network.Wai.Handler.Warp ( runSettings
                                , defaultSettings
                                , Settings(..)
                                , HostPreference(Host)
                                )

import Cryptd.Master.CLI.Options
import Cryptd.Master.Certs (getMasterPair)
import Cryptd.Master.Warp (app)
import Cryptd.Master.ConnMgr (makeMasterHandler)
import Cryptd.Lib.TLS
import Cryptd.Lib.Tunnel
import Cryptd.Lib.Callbacks (requestLoop)
import Cryptd.Lib.Daemonize (daemonize)

getopt :: String -> String -> CLI -> String
getopt opt def opts =
    defval $ lookup opt opts
  where
    defval (Just (CLIValue v)) = v
    defval _ = def

makeSettings :: String -> Int -> Settings
makeSettings laddr port = defaultSettings
    { settingsHost = Host laddr
    , settingsPort = port
    }

execute :: CLI -> IO ()
execute opts = maybeDaemonize $ do
    (pool, handler) <- makeMasterHandler callbacks
    certs <- getMasterPair
    _ <- runTLSServer tlsAddr tlsPort certs handler
    runSettings settings (app pool)
  where
    maybeDaemonize = maybe (daemonize "cryptd-master") (const id) $
        lookup "foreground" opts

    tlsAddr = getopt "tlsaddr" "0.0.0.0" opts
    tlsPort = read (getopt "tlsport" "26662" opts) :: Integer
    laddr = getopt "listenaddr" "127.0.0.1" opts
    port = read (getopt "port" "16661" opts) :: Int
    settings = makeSettings laddr port

    url = getopt "url" "http://localhost/api/" opts

    callbacks = noCallbacks { onLoop = requestLoop url }
