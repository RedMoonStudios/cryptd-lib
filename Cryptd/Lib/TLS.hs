module Cryptd.Lib.TLS (runTLS, runTLSServer) where

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

type Certs = (X509, PrivateKey)
type LoopCmd = HostName -> PortID -> Certs -> HandlerCmd -> IO (Maybe String)
type HandlerCmd = TunnelHandle -> IO ()

tlsConfig :: Certs -> TLSParams
tlsConfig (pub, priv) = defaultParams
    { pConnectVersion = TLS12
    , pAllowedVersions = [TLS12]
    , pCiphers = ciphersuite_strong
    , pCertificates = [(pub, Just (PrivRSA priv))]
    }

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

doFinalize :: Handle -> TunnelHandle -> HandlerCmd -> IO (Maybe String)
doFinalize tcpHandle tlsHandle handler =
    handshake tlsHandle >> handle tcpHandle tlsHandle
  where
    handle tcp tls = do
        EX.finally (handler tls)
                   (bye tls >> hClose tcp)
        return Nothing

connect :: LoopCmd
connect host port certs handler = withSocketsDo $ do
    rng <- RNG.makeSystem
    tcpHandle <- connectTo host port
    tlsHandle <- client config rng tcpHandle
    doFinalize tcpHandle tlsHandle handler
  where
    config = tlsConfig certs

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

retryWait :: Int -> IO ()
retryWait secs = threadDelay $ 1000000 * secs

maybeCatch :: (String -> IO ()) -> IO (Maybe String) -> IO ()
maybeCatch exhandler fun = do
    result <- EX.catch fun handle
    when (isJust result) $ exhandler (fromJust result)
  where
    handle e = return (Just $ show (e :: EX.SomeException))

maybePrintCatch :: IO (Maybe String) -> IO ()
maybePrintCatch = maybeCatch putStrLn

maybePrintCatchWait :: IO (Maybe String) -> IO ()
maybePrintCatchWait = maybeCatch (\r -> putStrLn r >> retryWait 20)

runConnector :: LoopCmd -> String -> Integer -> Certs -> HandlerCmd ->
                IO (Maybe String)
runConnector loop host port =
    loop host (PortNumber $ fromInteger port)

runTLSLoop :: LoopCmd -> String -> Integer -> Certs -> HandlerCmd ->
              IO ThreadId
runTLSLoop loop host port certs handler =
    forkIO . forever . maybePrintCatchWait $
        runConnector loop host port certs handler

runTLS :: String -> Integer -> Certs -> HandlerCmd -> IO ThreadId
runTLS = runTLSLoop connect

runTLSServer :: String -> Integer -> Certs -> HandlerCmd -> IO ThreadId
runTLSServer host port certs hcmd = forkIO . maybePrintCatch $
    runConnector serve host port certs hcmd
