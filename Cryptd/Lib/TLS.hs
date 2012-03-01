-- | Interface for providing networking using TCP sockets and TLS.
module Cryptd.Lib.TLS (runTLS, runTLSServer, sendData, recvData') where

import System.IO
import Data.Maybe (isJust, fromJust)
import Data.Certificate.X509 (X509)
import Crypto.Types.PubKey.RSA (PrivateKey)
import Control.Monad (forever, when)
import Control.Exception (bracketOnError)
import Control.Concurrent (forkIO, threadDelay, ThreadId)
import Network
import Network.BSD (getProtocolNumber)
import Network.TLS hiding (PrivateKey)
import Network.TLS.Extra
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

-- | Return 'TLSParams' for the given 'Certs' pair.
tlsConfig :: Certs -> TLSParams
tlsConfig (pub, priv) = defaultParams
    { pConnectVersion = TLS12
    , pAllowedVersions = [TLS12]
    , pCiphers = ciphersuite_strong
    , pCertificates = [(pub, Just (PrivRSA priv))]
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
    handshake tlsHandle >> handle tcpHandle tlsHandle
  where
    handle tcp tls = do
        EX.finally (handler tls)
                   (bye tls >> hClose tcp)
        return Nothing

-- | Do a TCP connect using the values specified by 'LoopCmd'.
connect :: LoopCmd
connect host port certs handler = withSocketsDo $ do
    rng <- RNG.makeSystem
    tcpHandle <- connectTo host port
    tlsHandle <- client config rng tcpHandle
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
            tlsHandle <- server config rng tcpHandle
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
runConnector :: LoopCmd
             -> String -- ^ Host/IP of the current connection
             -> Integer -- ^ Port of the current connection
             -> Certs
             -> HandlerCmd
             -> IO (Maybe String)
runConnector loop host port =
    loop host (PortNumber $ fromInteger port)

-- | Forever run TLS loop catching errors and printing them.
runTLSLoop :: LoopCmd
           -> String -- ^ Host/IP of the current connection
           -> Integer -- ^ Port of the current connection
           -> Certs
           -> HandlerCmd
           -> IO ThreadId
runTLSLoop loop host port certs handler =
    forkIO . forever . maybePrintCatchWait $
        runConnector loop host port certs handler

-- | Connect to a TLS server an the specified host/IP and port using 'Certs'
-- and 'HandlerCmd'.
runTLS :: String -- ^ Host/IP to connect to
       -> Integer -- ^ Port to use for connection
       -> Certs -- ^ Certificates to use for authentication
       -> HandlerCmd
       -> IO ThreadId
runTLS = runTLSLoop connect

-- | Run a TLS server listening on the specified host/IP and port using 'Certs'
-- and 'HandlerCmd'.
runTLSServer :: String -- ^ Host/IP to listen on
             -> Integer -- ^ Port to use for connection
             -> Certs -- ^ Certificates to use for authentication
             -> HandlerCmd
             -> IO ThreadId
runTLSServer host port certs hcmd = forkIO . maybePrintCatch $
    runConnector serve host port certs hcmd
