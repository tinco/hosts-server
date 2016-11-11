{-# LANGUAGE OverloadedStrings #-}
module HostsServer.Config where

import Data.Default (Default(def))
import Network.Socket (HostName, AddrInfo)
import qualified Network.Socket as NS
import Data.IP (IPv4, toIPv4)
import Network.DNS (Domain)
import Data.List (partition)

import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as BP

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
      , bindAddress = NS.defaultHints
      }

{--
 - parse config file.
 -}
readHosts :: FilePath -> IO ([Host], [HostName])
readHosts filename =
    B.readFile filename >>= either (fail . ("parse hosts fail:"++)) return . parseHosts
  where
    parseHosts :: B.ByteString -> Either String ([Host], [HostName])
    parseHosts s = let (serverLines, hostLines) = partition (B.isPrefixOf "nameserver") (B.lines s)
                   in  (,) <$> mapM (BP.parseOnly host) hostLines
                           <*> mapM (BP.parseOnly nameserver) serverLines
    space = BP.space
    skipSpace = BP.skipSpace
    many1 = BP.many1
    digit = BP.digit
    string = BP.string

    host :: BP.Parser Host
    host = do
        skipSpace
        ip <- toIPv4 . map read <$> (many1 digit `BP.sepBy` string ".")
        _ <- space
        skipSpace
        dom <- BP.takeWhile (not . BP.isSpace)
        skipSpace
        return (dom, ip)

    nameserver :: BP.Parser HostName
    nameserver = do
        _ <- string "nameserver"
        _ <- space
        skipSpace
        B.unpack <$> BP.takeWhile (not . BP.isSpace)
