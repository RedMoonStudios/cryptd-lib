{-# LANGUAGE DeriveDataTypeable #-}
module Cryptd.Slave.CLI (run, SlaveSettings(..)) where

import System.Console.CmdArgs.Implicit

import qualified Cryptd.Lib.ConfigEmbed as Conf

data SlaveSettings = SlaveSettings
    { listenAddress :: String
    , port :: Integer
    , masterHost :: String
    , masterPort :: Integer
    , url :: String
    , foreground :: Bool
    , instanceId :: Maybe String
    }
    deriving (Show, Data, Typeable)

slave :: SlaveSettings
slave = SlaveSettings
    { listenAddress = "127.0.0.1"
                   &= typ "ADDRESS"
                   &= help "Local listen address"
    , port          = 16661
                   &= typ "PORT"
                   &= help "Local port to listen on"
    , masterHost    = "connector.moonid.net"
                   &= typ "ADDRESS"
                   &= help "Hostname/IP of the master server"
    , masterPort    = 26662
                   &= typ "PORT"
                   &= help "Port of the master server"
    , url           = Conf.url
                   &= typ "URL"
                   &= help ("URL to the local API backend" ++ defurl)
    , foreground    = False
                   &= help "Run cryptd in the foreground"
    , instanceId    = Nothing
                   &= typ "INSTANCE_IDENTIFIER"
                   &= opt ""
                   &= argPos 0
    }
    &= program "cryptd-slave"
    &= summary "MoonID encryption daemon - Slave server"
    &= details ["(C) 2012 RedMoon Studios GmbH & Co KG"]
  where
    defurl = " (Default: " ++ Conf.url ++ ")"

run :: IO SlaveSettings
run = cmdArgs slave
