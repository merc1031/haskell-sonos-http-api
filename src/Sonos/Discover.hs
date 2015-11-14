{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Sonos.Discover where

import Network.Socket
import Network.Multicast
import Control.Concurrent.Async
import Control.Concurrent.MVar

import Network.Wreq
import Network.HTTP.Base

import Text.Parsec

import Debug.Trace

import Text.XML
import Text.XML.Cursor

import Sonos.Types

import Control.Lens ((^?), (.~), (&))

import qualified Data.List as L
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

type Parser = Parsec String ()

discover = withSocketsDo $ do
    (sock, addr) <- multicastSender "239.255.255.250" 1900
    let loop = do
           let msg = [ "M-SEARCH * HTTP/1.1"
                   , "HOST: 239.255.255.250:1900"
                   , "MAN: \"ssdp:discover\""
                   , "MX: 1"
                   , "ST: urn:schemas-upnp-org:device:ZonePlayer:1"
                   ]
               msg' = unlines msg
           sendTo sock msg' addr
           (msg, _, addr1) <- recvFrom sock 1024
           if L.isInfixOf "Sonos" msg
               then do
                   print msg
                   return $ parse it "" msg
               else loop
    loop

it :: Parser SonosDiscovery
it = do
    _ <- traceShowId <$> httpRespP <?> "Couldnt get http"
    crlf
    _ <- traceShowId <$> cacheP <?> "Cache header missing"
    crlf
    _ <- traceShowId <$> extP <?> "Weird EXT missing"
    crlf
    location <- traceShowId <$> locationP <?> "Location failed"
    crlf
    server <- serverP <?> "Server Failed"
    crlf
    st <- stP <?> "ST Failed"
    crlf
    usn <- usnP <?> "USN Failed"
    crlf
    household <- householdP <?> "Household failed"
    crlf
    boot <- bootP <?> "boot failed"
    crlf
    wifi <- wifiP <?> "wifi mode failed"
    crlf
    crlf
    return $ SonosDiscovery location server st usn household boot wifi

httpRespP :: Parser ()
httpRespP = do
    string "HTTP/1.1"
    space
    string "200"
    space
    string "OK"
    return ()

cacheP :: Parser ()
cacheP = do
    string "CACHE-CONTROL:"
    space
    string "max-age"
    space
    char '='
    space
    _ <- many digit
    return ()

extP :: Parser ()
extP = do
    string "EXT:" <?> "Missed ext"
    return ()

sonosEPP :: Parser Location
sonosEPP = do
    proto <- string "http://" <|> string "https://"
    url <- traceShowId <$> manyTill (alphaNum <|> oneOf "/_-.()") (char ':') <?> "Missed url"
    port <- many digit <?> "Missed port"
    endpoint <- many (alphaNum <|> oneOf "/_-.()") <?> "Missed endpoint"
    return $ Location proto url port endpoint


locationP :: Parser Location
locationP = do
    string "LOCATION:" <?> "Missed location literal"
    space
    l <- sonosEPP
    return $ traceShowId $ l

serverP :: Parser String
serverP = do
    string "SERVER:"
    space
    serv <- many (alphaNum <|> oneOf "/.-(): ")
    return serv

stP :: Parser String
stP = do
    string "ST:"
    space
    st <- many (alphaNum <|> oneOf "-:")
    return st

usnP :: Parser String
usnP = do
    string "USN:"
    space
    usn <- many (alphaNum <|> oneOf ":_-")
    return usn

householdP :: Parser String
householdP = do
    string "X-RINCON-HOUSEHOLD:"
    space
    household <- many (alphaNum <|> oneOf "_")
    return household

bootP :: Parser String
bootP = do
    string "X-RINCON-BOOTSEQ:"
    space
    seq <- many digit
    return seq

wifiP :: Parser String
wifiP = do
    string "X-RINCON-WIFIMODE:"
    space
    seq <- many digit
    return seq


getTopology = do
    first <- discover
    case first of
        Left _ -> return []
        Right val -> do
            let SonosDiscovery {..} = val
                Location {..} = sdLocation
            r <- get (lProto ++ lUrl ++ ":" ++ lPort ++ "/status/topology")
            let Just body = r ^? responseBody
            let zps = map toZP (toZonePlayer body)
            print zps
            return zps


toZonePlayer :: BSL.ByteString -> [Cursor]
toZonePlayer body =
    let cursor = fromDocument $ parseLBS_ def body
    in cursor $/ element "ZonePlayers" &/ element "ZonePlayer"

toZP :: Cursor -> ZonePlayer
toZP c =
    let [group] = c $.// attribute "group"
        [coordinator] = c $.// attribute "coordinator"
        [location] = traceShowId $ c $.// attribute "location"
        [bootseq] = c $.// attribute "bootseq"
        [uuid] = c $.// attribute "uuid"
        [cntn] = c $// content
        Right l' = parse sonosEPP "" (T.unpack location)
        t = T.unpack
        b = \case
            "true" -> True
            "false" -> False
    in ZonePlayer (t group) (b $ t coordinator) l' (t bootseq) (t uuid) (t cntn)

