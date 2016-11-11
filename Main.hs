{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Prelude hiding (takeWhile)
import Data.Default (Default(def))
import Control.Applicative ( (<$>), (<*>) )
import Data.IP (IPv4, toIPv4)
import Data.Maybe
import System.Environment (getArgs)
import System.Timeout (timeout)
import Network.Socket.ByteString (sendAll, sendAllTo, recvFrom)
import Network.Socket hiding (recvFrom)
import Network.DNS

import HostsServer.Server
import HostsServer.Config

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString.Char8 as BP

main :: IO ()
main = do
    args <- getArgs
    (hosts, servers) <- readHosts $ fromMaybe "./hosts" (listToMaybe args)
    print (hosts, servers)
    let handler conf s = dnsHandleResponse conf $ handlePacket conf s
    runServer def{hosts=hosts, nameservers=servers, bindAddress=inet4LocalAddr} handler


{--
 - Proxy dns request to a real dns server.
 -}
proxyRequest :: Conf -> HostName -> DNSFormat -> IO (Either String DNSFormat)
proxyRequest Conf{..} server req = do
    let rc = defaultResolvConf { resolvInfo = RCHostName server }
        worker Resolver{..} = do
            sendAll dnsSock $ encodePacket req
            receive dnsSock
    rs <- makeResolvSeed rc
    withResolver rs $ \r ->
        (>>= check) . toEither "proxy request timeout" <$> timeout timeOut (worker r)
  where
    ident = identifier . header $ req
    check :: DNSFormat -> Either String DNSFormat
    check rsp = let hdr = header rsp
                in  if identifier hdr == ident
                        then Right rsp
                        else Left "identifier not match"

{--
 - Handle A query for domain suffixes configured, and proxy other requests to real dns server.
 -}
handleRequest :: Conf -> DNSFormat -> RequestResponse
handleRequest conf req =
    case lookupHosts of
        (Just rsp) -> RequestResponse rsp
        Nothing  -> maybe
                    (NoResponseError "nameserver not configured.")
                    (\srv -> ShouldProxy srv req)
                    (listToMaybe (nameservers conf))
  where
    filterA = filter ((==A) . qtype)
    filterHost dom = filter (\(h, _) -> h `B.isSuffixOf` dom)
    ident = identifier . header $ req
    lookupHosts :: Maybe DNSFormat
    lookupHosts = do
        q <- listToMaybe . filterA . question $ req
        (_, ip) <- listToMaybe . filterHost (qname q) $ hosts conf
        return $ responseA ident q [ip]

{--
 - Parse request and compose response.
 -}
handlePacket :: Conf -> B.ByteString -> PacketResponse
handlePacket conf@Conf{..} s =
    either
    (PacketDecodeError . ("decode fail:"++))
    (\req -> case handleRequest conf req of
            NoResponseError e -> PacketResponseError e
            ShouldProxy host r -> PacketShouldProxy host r
            RequestResponse rsp -> PacketShouldReply rsp
    ) (decodePacket s)

dnsHandleResponse :: Conf -> PacketResponse -> IO(Maybe ServerAction)
dnsHandleResponse conf@Conf{..} pr = case pr of
  PacketDecodeError e -> do
    putStrLn $ "PacketDecodeError: " ++ e
    return Nothing
  PacketResponseError e -> do
    putStrLn $ "PacketResponseError: " ++ e
    return Nothing
  PacketShouldProxy h p -> do
    prx <- proxyRequest conf h p
    case prx of
      Left e -> do
        putStrLn $ "ProxyError: " ++ e
        return Nothing
      Right rsp -> do
        return $ Just $ SendReply rsp
  PacketShouldReply p -> return $ Just $ SendReply p


inet4LocalAddr :: AddrInfo
inet4LocalAddr = defaultHints {
      addrFamily = AF_INET,
      addrSocketType = Datagram,
      addrAddress = SockAddrInet 53 (tupleToHostAddress (0,0,0,0))
    }

toEither :: a -> Maybe b -> Either a b
toEither a = maybe (Left a) Right

-- Getting local interface
-- 
{- 
    addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just "domain")
    addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
    -}
 
