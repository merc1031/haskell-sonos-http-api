{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Sonos.Lib where

import System.FilePath.Glob
import Network.Wreq
import Network.HTTP.Base
import Data.String.Utils
import Text.XML
import Text.XML.Cursor
import Control.Concurrent.STM

import Data.String (fromString)
import Sonos.Types
import Sonos.Util (findCoordinatorForIp)

import Control.Monad (forever)

import Control.Concurrent (threadDelay)

import Control.Lens ((^?), (.~), (&))
import Data.Monoid ((<>))
import Data.Char (toLower)
import Sonos.Discover (getTopology)
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Web.Spock as WS
import qualified Data.Map.Strict as M
import qualified Text.XML.Light.Extractors as E
import qualified Data.Text as T

findMatchingArtistGlob :: CliArguments
                       -> String
                       -> IO [[String]]
findMatchingArtistGlob args like = do
    putStrLn $ "What did we hear: " ++ like
    res <- globDirWith  (matchDefault { ignoreCase = True } ) [(compile $ "**/" ++ like ++ "/**/*.flac")] (dir args)
    putStrLn $ "Results were" ++ (show $ fst res)
    return $ fst res


findMatchingGlob :: CliArguments
                 -> String
                 -> IO [[String]]
findMatchingGlob args like = do
    putStrLn $ "What did we hear: " ++ like
    res <- globDirWith  (matchDefault { ignoreCase = True } ) [(compile $ "**/*" ++ like ++ "*.flac")] (dir args)
    putStrLn $ "Results were" ++ (show $ fst res)
    return $ fst res

envelopePreamble = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><s:Body>"
envelopePostamble = "</s:Body><s:Envelope>"

envelope cmd = envelopePreamble ++ cmd ++ envelopePostamble

avTransportAction = "urn:schemas-upnp-org:service:AVTransport:1"
avTransportNS = "xmlns:u=\"" ++ avTransportAction ++ "\""

addURIToQueueTemplate uri meta first next =
    let addUri cts = "<u:AddURIToQueue " ++ avTransportNS ++ "><InstanceID>0</InstanceID>" ++ cts ++ "</u:AddURIToQueue>"
        enqueuedURI = "<EnqueuedURI>" ++ uri ++ "</EnqueuedURI>"
        enqueuedURIMetadata = "<EnqueuedURIMetaData>" ++ meta ++ "</EnqueuedURIMetaData>"
        desiredFTN = "<DesiredFirstTrackNumberEnqueued>" ++ show first ++ "</DesiredFirstTrackNumberEnqueued>"
        enqueueAsNext = "<EnqueueAsNext>" ++ show next ++ "</EnqueueAsNext>"
    in addUri (enqueuedURI ++ enqueuedURIMetadata ++ desiredFTN ++ enqueueAsNext)

playTemplate = "<u:Play " ++ avTransportNS ++ "><InstanceID>0</InstanceID><Speed>1</Speed></u:Play>"

setAVTransportURITemplate uri meta =
    let avTransport cts = "<u:SetAVTransportURI " ++ avTransportNS ++ "><InstanceID>0</InstanceID>" ++ cts ++ "</u:SetAVTransportURI>"
        currentURI = "<CurrentURI>" ++ uri ++ "</CurrentURI>"
        currentURIMetadata = "<CurrentURIMetaData>" ++ meta ++ "</CurrentURIMetaData>"
    in avTransport (currentURI ++ currentURIMetadata)

becomeCoordinatorOfStandaloneGroup md =
    let avTransport cts = "<u:BecomeCoordinatorOfStandaloneGroup " ++ avTransportNS ++ ">" ++ cts ++ "</u:BecomeCoordinatorOfStandaloneGroup>"
        md' = foldl1 (++) $ map mk md
        mk (w, i) = "<" ++ w ++ ">" ++ i ++ "</" ++ w ++ ">"
    in avTransport (md')

seekTrackTemplate track = "<u:Seek " ++ avTransportNS ++ "><InstanceID>0</InstanceID><Unit>TRACK_NR</Unit><Target>" ++ show track ++ "</Target></u:Seek>"

soapAction host action msg = do
    let opts = defaults & header "Host" .~ [BSC.pack host]
                        & header "User-Agent" .~ ["Haskell post"]
                        & header "Content-type" .~ ["text/xml; charset=\"UTF-8\""]
                        & header "Content-length" .~ [BSC.pack $ show $ length msg]
                        & header "SOAPAction" .~ [BSC.pack $ avTransportAction ++ "#" ++ action]
        ep = "/MediaRenderer/AVTransport/Control"

    print opts
    print msg
    resp <- postWith opts ("http://" ++ host ++ ep) (BSC.pack $ envelope msg)

    print $ resp ^? responseBody
    print $ resp ^? responseStatus
    return $ resp ^? responseBody

getRoom :: [ZonePlayer]
        -> String
        -> ZonePlayer
getRoom zps room =
    let rooms = M.fromList $ map (\zp@(ZonePlayer {..}) ->
            let name = zpName
            in (fmap toLower name, zp)
                                 ) zps


        Just room' = M.lookup (fmap toLower room) rooms
    in room'

groupRoom :: [ZonePlayer]
          -> CliArguments
          -> ZonePlayer
          -> ZonePlayer
          -> IO ()
groupRoom zps args a b = do
    let coordA = findCoordinatorForIp (zpLocation a) zps
        coordB = findCoordinatorForIp (zpLocation b) zps
        addr = let l = zpLocation coordA
               in lUrl l ++ ":" ++ lPort l
    let avMessage = setAVTransportURITemplate ("x-rincon:"  ++ (zpUUID coordB)) ""
    soapAction addr "SetAVTransportURI" avMessage
    return ()

ungroupRoom :: [ZonePlayer]
          -> CliArguments
          -> ZonePlayer
          -> IO ()
ungroupRoom zps args room = do
    let addr = let l = zpLocation room
               in lUrl l ++ ":" ++ lPort l
    let avMessage = becomeCoordinatorOfStandaloneGroup [("InstanceID", show 0)]
    soapAction addr "BecomeCoordinatorOfStandaloneGroup" avMessage
    return ()


queueAndPlayTrackLike :: [ZonePlayer]
                      -> CliArguments
                      -> ZonePlayer
                      -> String
                      -> IO ()
queueAndPlayTrackLike zps args host like = do
    let coord = findCoordinatorForIp (zpLocation host) zps
    queuedBody <- queueTrackLike zps args host like
    let trackNo = getTrackNum queuedBody
        addr = let l = zpLocation coord
               in lUrl l ++ ":" ++ lPort l

    putStrLn $ ("Coord was: " ++ show  coord)
    uuid <- fetchUUID (addr)
    let avMessage = setAVTransportURITemplate ("x-rincon-queue:" ++ (T.unpack uuid) ++ "#0") ""

    soapAction addr "SetAVTransportURI" avMessage
    soapAction addr "Seek" (seekTrackTemplate trackNo)
    soapAction addr "Play" playTemplate


    return ()

queueTrackLike :: [ZonePlayer]
               -> CliArguments
               -> ZonePlayer
               -> String
               -> IO BSL.ByteString
queueTrackLike zps args host like = do
    let coord = findCoordinatorForIp (zpLocation host) zps
    tracks <- findMatchingGlob args like
    let firstTrack = head $ head tracks
    putStrLn $ "First track is:" ++ firstTrack


    let firstTrackE = urlEncode $ replace ".flac" ".mp3" $ replace (dir args) "" $ firstTrack
        soapMessage = addURIToQueueTemplate ("x-file-cifs://asgard/mp3Music/" ++ firstTrackE) "" 0 0
        addr = let l = zpLocation coord
               in lUrl l ++ ":" ++ lPort l

    putStrLn $ ("Coord was: " ++ show  coord)
    Just queuedBody <- soapAction addr "AddURIToQueue" soapMessage
    return queuedBody

fetchUUID host = do
    Just body <- getXMLDescription host
    return $ getUUID body

getXMLDescription host = do
    res <- get $ "http://" ++ host ++ "/xml/device_description.xml"
    return $ res ^? responseBody

getUUID body =
    let cursor = fromDocument $ parseLBS_ def body
        ns = "{urn:schemas-upnp-org:device-1-0}"
    in T.drop 5 $ head $ cursor $/ element (fromString $ ns ++ "device")
                                &/ element (fromString $ ns ++ "UDN")
                                &// content

getTrackNum :: BSL.ByteString
            -> Int
getTrackNum body =
    let cursor = fromDocument $ parseLBS_ def body
    in read $ T.unpack $ head $ cursor $/ element "{http://schemas.xmlsoap.org/soap/envelope/}Body"
                                       &/ element "{urn:schemas-upnp-org:service:AVTransport:1}AddURIToQueueResponse"
                                       &/ element "FirstTrackNumberEnqueued"
                                       &// content

