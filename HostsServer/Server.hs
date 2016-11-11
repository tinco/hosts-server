{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module HostsServer.Server where

import Network.Socket (HostName, AddrInfo)
import qualified Network.Socket as NS
import Network.Socket.ByteString as NSB
import qualified Network.DNS as DNS
import Network.DNS (DNSMessage)

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import System.Timeout (timeout)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Attoparsec.ByteString.Char8 as BP

import HostsServer.Config

data RequestResponse =
  ShouldProxy HostName DNSMessage |
  NoResponseError String |
  RequestResponse DNSMessage

data PacketResponse =
  PacketDecodeError String |
  PacketResponseError String |
  PacketShouldProxy HostName DNSMessage |
  PacketShouldReply DNSMessage

data ServerAction = SendReply DNSMessage

decodePacket :: B.ByteString -> Either String DNSMessage
decodePacket s = DNS.decode (BL.fromChunks [s])

encodePacket :: DNSMessage -> B.ByteString
encodePacket p = B.concat . BL.toChunks $ DNS.encode p

type PacketHandler = Conf -> B.ByteString -> IO (Maybe ServerAction)

runServer :: Conf -> PacketHandler -> IO ()
runServer conf@Conf{..} handler = NS.withSocketsDo $ do
    sock <- NS.socket (NS.addrFamily bindAddress) (NS.addrSocketType bindAddress) (NS.addrProtocol bindAddress)
    NS.bind sock (NS.addrAddress bindAddress)
    forever $ do
        (s, addr) <- NSB.recvFrom sock bufSize
        forkIO $ do
          action <- handler conf s
          case action of
            Nothing -> return ()
            Just (SendReply p) -> do
              result <- timeout timeOut (NSB.sendAllTo sock (encodePacket p) addr)
              case result of
                Just _ -> return ()
                Nothing -> putStrLn "send response timeout"


