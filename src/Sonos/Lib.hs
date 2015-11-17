{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Sonos.Lib where

import System.FilePath.Glob
import Network.Wreq
import Network.HTTP.Base (urlEncode)
import Data.String.Utils
import Text.XML
import Text.XML.Cursor
import Control.Concurrent.STM

import Sonos.Types

import Data.Maybe                           (fromJust)
import Data.String                          (fromString, IsString)
import Sonos.Util                           (findCoordinatorForIp)
import Control.Monad                        (forever)
import Control.Concurrent                   (threadDelay)
import Control.Lens                         ((^?), (.~), (&))
import Data.Monoid                          ((<>))
import Data.Char                            (toLower)

import Sonos.Discover                       (getTopology)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Char8      as BSC
import qualified Web.Spock                  as WS
import qualified Data.Map.Strict            as M
import qualified Text.XML.Light.Extractors  as E
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Sonos.Plugins.Pandora as Pandora
import qualified HTMLEntities.Builder as HTML

findMatchingArtistGlob :: CliArguments
                       -> String
                       -> IO [[String]]
findMatchingArtistGlob args like = do
    putStrLn $ "What did we hear: " ++ like
    res <- globDirWith (matchDefault { ignoreCase = True } )
           [compile $ like ++ "/**/*.flac"]
           (dir args)
    putStrLn $ "Results were" ++ show (fst res)
    return $ fst res


findMatchingGlob :: CliArguments
                 -> String
                 -> IO [[String]]
findMatchingGlob args like = do
    putStrLn $ "What did we hear: " ++ like
    res <- globDirWith (matchDefault { ignoreCase = True } )
                       [compile $ "**/*" ++ like ++ "*.flac"]
                       (dir args)
    putStrLn $ "Results were" ++ show (fst res)
    return $ fst res

envelope :: String
         -> String
envelope content =
    wrap (Name "Envelope"
               (Just "http://schemas.xmlsoap.org/soap/envelope/")
               (Just "s")
         )
         (Just "http://schemas.xmlsoap.org/soap/encoding/")
         $ wrap (Name "Body" Nothing (Just "s"))
                Nothing
                content

wrap :: Name
     -> Maybe String
     -> String
     -> String
wrap (Name {..}) ec content =
    let (pfx,pfxs) = case namePrefix of
            Just p -> (T.unpack p ++ ":", ":" ++ T.unpack p)
            Nothing -> ("","")
        ns = case nameNamespace of
            Just n -> " xmlns" ++ pfxs ++ "=\"" ++ T.unpack n ++ "\""
            Nothing -> ""
        ec' = case ec of
            Just e -> " " ++ pfx ++ "encodingStyle=\"" ++ e ++ "\""
            Nothing -> ""
    in "<"
    ++ pfx
    ++ T.unpack nameLocalName
    ++ ns
    ++ ec'
    ++ ">"
    ++ content
    ++ "</"
    ++ pfx
    ++ T.unpack nameLocalName
    ++ ">"

avTransportAction :: String
avTransportAction = "urn:schemas-upnp-org:service:AVTransport:1"

avTransportNS :: String
avTransportNS = "xmlns:u=\"" ++ avTransportAction ++ "\""


addURIToQueueTemplate_ :: String
                       -> String
                       -> Int
                       -> Int
                       -> String
addURIToQueueTemplate_ uri meta first next =
    avTransportTemplate "AddURIToQueue"
                        [ ("InstanceID", "0")
                        , ("EnqueuedURI", uri)
                        , ("EnqueuedURIMetaData", meta)
                        , ("DesiredFirstTrackNumberEnqueued", show first)
                        , ("EnqueueAsNext", show next)
                        ]

playTemplate_ :: String
playTemplate_ =
    avTransportTemplate "Play"
                        [ ("InstanceID", "0")
                        , ("Speed", "1")
                        ]

setAVTransportURITemplate_ :: String
                           -> String
                           -> String
setAVTransportURITemplate_ uri meta =
    avTransportTemplate "SetAVTransportURI"
                        [ ("InstanceID", "0")
                        , ("CurrentURI", uri)
                        , ("CurrentURIMetaData", meta)
                        ]

becomeCoordinatorOfStandaloneGroup_ :: String
becomeCoordinatorOfStandaloneGroup_ =
    avTransportTemplate "BecomeCoordinatorOfStandaloneGroup"
                        [ ("InstanceID", "0")
                        ]

seekTrackTemplate_ :: Int
                   -> String
seekTrackTemplate_ track =
    avTransportTemplate "Seek"
                        [ ("InstanceID", "0")
                        , ("Unit", "TRACK_NR")
                        , ("Target", show track)
                        ]

wrap_ :: (String, String)
      -> String
wrap_ (w, i) = wrap (fromString w)
                    Nothing
                    i

avTransportTemplate :: String
                    -> [(String, String)]
                    -> String
avTransportTemplate action md =
    let avTransport cts = "<u:"
                       ++ action
                       ++ " "
                       ++ avTransportNS
                       ++ ">"
                       ++ cts
                       ++ "</u:"
                       ++ action
                       ++ ">"
        md' = foldl1 (++) $ map wrap_ md
    in avTransport md'

soapAction :: String
           -> String
           -> String
           -> IO (Maybe BSL.ByteString)
soapAction host action msg = do
    let opts = defaults & header "Host" .~ [BSC.pack host]
                        & header "User-Agent" .~ ["Haskell post"]
                        & header "Content-type" .~ ["text/xml; charset=\"UTF-8\""]
                        & header "Content-length" .~ [BSC.pack $ show $ length msg]
                        & header "SOAPAction" .~ [BSC.pack $ avTransportAction ++ "#" ++ action]
        ep = "/MediaRenderer/AVTransport/Control"

    print opts
    print msg
    resp <- postWith opts
                     ("http://" ++ host ++ ep)
                     (BSC.pack $ envelope msg)

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
    let avMessage = setAVTransportURITemplate_ ("x-rincon:" ++ zpUUID coordB) ""
    soapAction addr "SetAVTransportURI" avMessage
    return ()

ungroupRoom :: [ZonePlayer]
            -> CliArguments
            -> ZonePlayer
            -> IO ()
ungroupRoom zps args room = do
    let addr = let l = zpLocation room
               in lUrl l ++ ":" ++ lPort l
    let avMessage = becomeCoordinatorOfStandaloneGroup_
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

    putStrLn ("Coord was: " ++ show  coord)
    uuid <- fetchUUID addr
    let avMessage = setAVTransportURITemplate_ ("x-rincon-queue:" ++ T.unpack uuid ++ "#0") ""

    soapAction addr "SetAVTransportURI" avMessage
    soapAction addr "Seek" (seekTrackTemplate_ trackNo)
    soapAction addr "Play" playTemplate_


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


    let firstTrackE = urlEncode
                    $ replace ".flac" ".mp3"
                    $ replace (dir args) "" firstTrack
        soapMessage = addURIToQueueTemplate_ ("x-file-cifs://asgard/mp3Music/" ++ firstTrackE)
                                             ""
                                             0
                                             0
        addr = let l = zpLocation coord
               in lUrl l ++ ":" ++ lPort l

    putStrLn ("Coord was: " ++ show coord)
    Just queuedBody <- soapAction addr "AddURIToQueue" soapMessage
    return queuedBody

queueAndPlayArtistLike :: [ZonePlayer]
                      -> CliArguments
                      -> ZonePlayer
                      -> String
                      -> IO ()
queueAndPlayArtistLike zps args host like = do
    let coord = findCoordinatorForIp (zpLocation host) zps
    queuedBodys <- queueArtistLike zps args host like
    let trackNo = getTrackNum $ fromJust $ head $ queuedBodys
        addr = let l = zpLocation coord
               in lUrl l ++ ":" ++ lPort l

    putStrLn ("Coord was: " ++ show  coord)
    uuid <- fetchUUID addr
    let avMessage = setAVTransportURITemplate_ ("x-rincon-queue:" ++ T.unpack uuid ++ "#0") ""

    soapAction addr "SetAVTransportURI" avMessage
    soapAction addr "Seek" (seekTrackTemplate_ trackNo)
    soapAction addr "Play" playTemplate_


    return ()

queueArtistLike :: [ZonePlayer]
               -> CliArguments
               -> ZonePlayer
               -> String
               -> IO [Maybe BSL.ByteString]
queueArtistLike zps args host like = do
    let coord = findCoordinatorForIp (zpLocation host) zps
    tracks <- findMatchingArtistGlob args like
    let tracks' = head tracks
    putStrLn $ "Tracks are:" ++ show tracks'


    let firstTrackE track = urlEncode
                    $ replace ".flac" ".mp3"
                    $ replace (dir args) "" track
        soapMessage track = addURIToQueueTemplate_ ("x-file-cifs://asgard/mp3Music/" ++ firstTrackE track)
                                                   ""
                                                   0
                                                   0
        addr = let l = zpLocation coord
               in lUrl l ++ ":" ++ lPort l

    putStrLn ("Coord was: " ++ show coord)
    queuedBodys <- mapM (\t -> soapAction addr "AddURIToQueue" (soapMessage t)) tracks'
    return queuedBodys

playPandoraStationLike :: [ZonePlayer]
                       -> CliArguments
                       -> ZonePlayer
                       -> String
                       -> IO ()
playPandoraStationLike zps args host like = do
    let coord = findCoordinatorForIp (zpLocation host) zps
    let addr = let l = zpLocation coord
               in lUrl l ++ ":" ++ lPort l

    putStrLn ("Coord was: " ++ show  coord)

    pw <- Pandora.login (T.pack $ email args) (T.pack $ password args)
    st <- Pandora.searchStation pw $ T.pack like
    print st
    st' <- Pandora.createStation pw (Pandora.artistMusicToken $ head $ Pandora.msrArtists st)

    print st'
    let email' = T.pack $ email args
        metadata = TL.unpack $ TLB.toLazyText $ HTML.text $ T.concat
           [ "<DIDL-Lite xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:upnp=\"urn:schemas-upnp-org:metadata-1-0/upnp/\" xmlns:r=\"urn:schemas-rinconnetworks-com:metadata-1-0/\" xmlns=\"urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/\">"
           , T.concat ["<item id=\"OOOX", (Pandora.sStationId $ Pandora.csrStation st'), "\" parentID=\"0\" restricted=\"true\">"]
           , "<dc:title>"
           , Pandora.sStationName $ Pandora.csrStation st'
           , "</dc:title>"
           , "<upnp:class>object.item.audioItem.audioBroadcast</upnp:class>"
           , "<desc id=\"cdudn\" nameSpace=\"urn:schemas-rinconnetworks-com:metadata-1-0/\">"
           , T.concat ["SA_RINCON3_", email']
           , "</desc>"
           , "</item>"
           , "</DIDL-Lite>"
           ]
    let avMessage = setAVTransportURITemplate_ ("pndrradio:" ++ (T.unpack $ Pandora.sStationId $ Pandora.csrStation st') ++ "?sn=6") metadata

    soapAction addr "SetAVTransportURI" avMessage
    soapAction addr "Play" playTemplate_


    return ()

fetchUUID :: String
          -> IO T.Text
fetchUUID host = do
    Just body <- getXMLDescription host
    return $ getUUID body

getXMLDescription :: String
                  -> IO (Maybe BSL.ByteString)
getXMLDescription host = do
    res <- get $ "http://" ++ host ++ "/xml/device_description.xml"
    return $ res ^? responseBody

getUUID :: BSL.ByteString
        -> T.Text
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


xmlEvent body =
    let cursor = fromDocument $ parseLBS_ def body
    in cursor
