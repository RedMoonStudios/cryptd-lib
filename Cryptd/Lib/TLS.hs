-- | Interface for providing networking using TCP sockets and TLS.
module Cryptd.Lib.TLS
    ( TLSSettings(..)
    , runTLSClient
    , runTLSServer
    , TLS.sendData
    , TLS.recvData'
    ) where

import System.IO
import Data.Word (Word16)
import Data.Maybe (isJust, fromJust)
import Data.Certificate.X509 (X509)
import Crypto.Types.PubKey.RSA (PrivateKey)
import Control.Monad (forever, when)
import Control.Exception (bracketOnError)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Network
import Network.BSD (getProtocolNumber)
import Network.TLS.Extra (ciphersuite_strong)
import qualified Network.TLS as TLS
import qualified Network.Socket as S
import qualified Control.Exception as EX
import qualified Crypto.Random.AESCtr as RNG

import Cryptd.Lib.Tunnel (TunnelHandle)

-- | Certificates used for authenticating to the other end of the tunnel.
type Certs = (X509, PrivateKey)

-- | The /real/ TLS handler, which wraps 'HandlerCmd'.
type LoopCmd = HostName -> PortID -> Certs -> HandlerCmd -> IO (Maybe String)

-- | Represents the TLS handler.
type HandlerCmd = TunnelHandle -> IO ()

data TLSSettings = TLSSettings
    { tlsHost :: String -- ^ Host/IP to bind on or connect to
    , tlsPort :: Word16 -- ^ Numeric port to use for the connection
    , tlsCerts :: Certs -- ^ Certificates to use for authentication
    , tlsHandler :: HandlerCmd -- ^ Function that should handle accepts
    }

-- | Return 'TLSParams' for the given 'Certs' pair.
tlsConfig :: Certs -> TLS.TLSParams
tlsConfig (pub, priv) = TLS.defaultParams
    { TLS.pConnectVersion = TLS.TLS12
    , TLS.pAllowedVersions = [TLS.TLS12]
    , TLS.pCiphers = ciphersuite_strong
    , TLS.pCertificates = [(pub, Just (TLS.PrivRSA priv))]
    }

-- | Listen to the given host/port and return the 'Socket'.
listenOnHost :: HostName -> PortID -> IO Socket
listenOnHost host (PortNumber port) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (S.socket S.AF_INET S.Stream proto)
        S.sClose
        (\sock -> do
            S.setSocketOption sock S.ReuseAddr 1
            hostaddr <- S.inet_addr host
            S.bindSocket sock (S.SockAddrInet port hostaddr)
            S.listen sock S.maxListenQueue
            return sock
        )
listenOnHost _ _ = error "Invalid port value!"

-- | Do the TLS handshake, call the handler and cleanup.
doFinalize :: Handle -> TunnelHandle -> HandlerCmd -> IO (Maybe String)
doFinalize tcpHandle tlsHandle handler =
    TLS.handshake tlsHandle >> handle tcpHandle tlsHandle
  where
    handle tcp tls = do
        EX.finally (handler tls)
                   (TLS.bye tls >> hClose tcp)
        return Nothing

-- | Do a TCP connect using the values specified by 'LoopCmd'.
connect :: LoopCmd
connect host port certs handler = withSocketsDo $ do
    rng <- RNG.makeSystem
    tcpHandle <- connectTo host port
    tlsHandle <- TLS.client config rng tcpHandle
    doFinalize tcpHandle tlsHandle handler
  where
    config = tlsConfig certs

-- | Do a TCP bind on the values specified by 'LoopCmd'.
serve :: LoopCmd
serve host port certs handler = withSocketsDo $ do
    rng <- RNG.makeSystem
    tcpListener <- listenOnHost host port
    forever $ do
        (tcpHandle, _, _) <- accept tcpListener
        forkIO $ maybePrintCatchWait $ do
            let config = tlsConfig certs
            tlsHandle <- TLS.server config rng tcpHandle
            doFinalize tcpHandle tlsHandle handler

-- | Wait for some time when a connection has to be retried.
retryWait :: Int
          -- ^ Seconds to wait
          -> IO ()
retryWait secs = threadDelay $ 1000000 * secs

-- | Run an action passing the 'Just' value or the error into a handler
-- function
maybeCatch :: (String -> IO ())
           -- ^ The handler function
           -> IO (Maybe String)
           -- ^ The action to run
           -> IO ()
maybeCatch exhandler fun = do
    result <- EX.catch fun handle
    when (isJust result) $ exhandler (fromJust result)
  where
    handle e = return (Just $ show (e :: EX.SomeException))

-- | Run an action returning 'Maybe' printing out the return value or the
-- error.
maybePrintCatch :: IO (Maybe String) -> IO ()
maybePrintCatch = maybeCatch putStrLn

-- | Same as 'maybeCatch', but wait for 20 seconds afterwards.
maybePrintCatchWait :: IO (Maybe String) -> IO ()
maybePrintCatchWait = maybeCatch (\r -> putStrLn r >> retryWait 20)

-- | Run a single TLS loop.
runConnector :: LoopCmd -> TLSSettings -> IO (Maybe String)
runConnector loop s = loop (tlsHost s) port (tlsCerts s) (tlsHandler s)
  where port = PortNumber . fromIntegral . tlsPort $ s

-- | Forever run TLS loop catching errors and printing them.
runTLSLoop :: LoopCmd -> TLSSettings -> IO ThreadId
runTLSLoop l s = forkIO . forever . maybePrintCatchWait $ runConnector l s

-- | Connect to a TLS server at the specified host/IP and port using 'Certs'
-- and 'HandlerCmd'.
runTLSClient :: TLSSettings -> IO ThreadId
runTLSClient = runTLSLoop connect

-- | Run a TLS server listening on the specified host/IP and port using 'Certs'
-- and 'HandlerCmd'.
runTLSServer :: TLSSettings -> IO ThreadId
runTLSServer = forkIO . maybePrintCatch . runConnector serve
