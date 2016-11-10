{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Prelude hiding (takeWhile)
import Control.Applicative ( (<$>), (<*>) )
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Default (Default(def))
import Data.IP (IPv4, toIPv4)
import Data.List (partition)
import Data.Maybe
import System.Environment (getArgs)
import System.Timeout (timeout)
import Network.Socket.ByteString (sendAll, sendAllTo, recvFrom)
import Network.Socket hiding (recvFrom)
import Network.DNS
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

type Host = (Domain, IPv4)

data Conf = Conf
  { bufSize     :: Int
  , timeOut     :: Int
  , nameservers :: [HostName]
  , hosts       :: [Host]
  , bindAddress :: AddrInfo
  }

instance Default Conf where
    def = Conf
      { bufSize     = 512
      , timeOut     = 10 * 1000 * 1000
      , nameservers = []
      , hosts       = []
      , bindAddress = defaultHints
      }

toEither :: a -> Maybe b -> Either a b
toEither a = maybe (Left a) Right

data RequestResponse =
  ShouldProxy HostName DNSFormat |
  NoResponseError String |
  RequestResponse DNSFormat

data PacketResponse =
  PacketDecodeError String |
  PacketResponseError String |
  PacketShouldProxy HostName DNSFormat |
  PacketShouldReply B.ByteString

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
            RequestResponse rsp -> PacketShouldReply $ encodePacket rsp
    ) (decodePacket s)

decodePacket :: B.ByteString -> Either String DNSMessage
decodePacket s = decode (BL.fromChunks [s])

encodePacket :: DNSMessage -> B.ByteString
encodePacket p = B.concat . BL.toChunks $ encode p

runServer :: Conf -> IO ()
runServer conf@Conf{..} = withSocketsDo $ do
    sock <- socket (addrFamily bindAddress) (addrSocketType bindAddress) (addrProtocol bindAddress)
    bind sock (addrAddress bindAddress)
    forever $ do
        (s, addr) <- recvFrom sock bufSize

        forkIO $ do
          case handlePacket conf s of
            PacketDecodeError e -> putStrLn $ "PacketDecodeError: " ++ e
            PacketResponseError e -> putStrLn $ "PacketResponseError: " ++ e
            PacketShouldProxy h p -> do
              prx <- proxyRequest conf h p
              case prx of
                Left e -> putStrLn $ "ProxyError: " ++ e
                Right rsp -> reply sock addr $ encodePacket rsp
            PacketShouldReply p -> reply sock addr p

        where
          reply sock addr p = do
            result <- timeout timeOut (sendAllTo sock p addr)
            case result of
              Just _ -> return ()
              Nothing -> putStrLn "send response timeout"

{--
 - parse config file.
 -}
readHosts :: FilePath -> IO ([Host], [HostName])
readHosts filename =
    B.readFile filename >>= either (fail . ("parse hosts fail:"++)) return . parseHosts
  where
    parseHosts :: B.ByteString -> Either String ([Host], [HostName])
    parseHosts s = let (serverLines, hostLines) = partition (B.isPrefixOf "nameserver") (B.lines s)
                   in  (,) <$> mapM (parseOnly host) hostLines
                           <*> mapM (parseOnly nameserver) serverLines

    host :: Parser Host
    host = do
        skipSpace
        ip <- toIPv4 . map read <$> (many1 digit `sepBy` string ".")
        _ <- space
        skipSpace
        dom <- takeWhile (not . isSpace)
        skipSpace
        return (dom, ip)

    nameserver :: Parser HostName
    nameserver = do
        _ <- string "nameserver"
        _ <- space
        skipSpace
        B.unpack <$> takeWhile (not . isSpace)

main :: IO ()
main = do
    args <- getArgs
    (hosts, servers) <- readHosts $ fromMaybe "./hosts" (listToMaybe args)
    print (hosts, servers)
    runServer def{hosts=hosts, nameservers=servers, bindAddress=inet4LocalAddr}

inet4LocalAddr :: AddrInfo
inet4LocalAddr = defaultHints {
      addrFamily = AF_INET,
      addrSocketType = Datagram,
      addrAddress = SockAddrInet 53 (tupleToHostAddress (0,0,0,0))
    }

-- Getting local interface
-- 
{- 
    addrinfos <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing (Just "domain")
    addrinfo <- maybe (fail "no addr info") return (listToMaybe addrinfos)
    -}
 
