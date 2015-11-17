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
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Sonos.Plugins.Pandora as Pandora
import qualified HTMLEntities.Builder as HTML
import qualified Data.Text.Format as Fmt
import qualified Formatting as Format

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

envelope :: T.Text
          -> T.Text
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
     -> Maybe T.Text
     -> T.Text
     -> T.Text
wrap (Name {..}) ec content =
    let (pfx,pfxs) = case namePrefix of
            Just p -> let fmtPL = Format.sformat $ Format.stext Format.% ":"
                          fmtPR = Format.sformat $ ":" Format.% Format.stext
                      in (fmtPL p, fmtPR p)
            Nothing -> ("","")
        ns = case nameNamespace of
            Just n -> let fmtN = Format.sformat $ " xmlns" Format.% Format.stext Format.% "=\"" Format.% Format.stext Format.% "\""
                      in fmtN pfxs n
            Nothing -> ""
        ec' = case ec of
            Just e -> let fmtEC = Format.sformat $ " " Format.% Format.stext Format.% "encodingStyle=\"" Format.% Format.stext Format.% "\""
                      in fmtEC pfx e
            Nothing -> ""
    in 
        let fmtM = Format.sformat $ "<" Format.% Format.stext Format.%
                       Format.stext Format.%
                       Format.stext Format.%
                       Format.stext Format.%
                   ">" Format.% Format.stext Format.%
                   "</" Format.% Format.stext Format.%
                       Format.stext Format.%
                   ">"
        in fmtM pfx nameLocalName ns ec' content pfx nameLocalName

avTransportAction :: T.Text
avTransportAction = "urn:schemas-upnp-org:service:AVTransport:1"

avTransportNS :: T.Text
avTransportNS = Format.sformat ("xmlns:u=\"" Format.% Format.stext Format.% "\"") avTransportAction

addURIToQueueTemplate :: T.Text
                       -> T.Text
                       -> Int
                       -> Int
                       -> T.Text
addURIToQueueTemplate uri meta first next =
    avTransportTemplate "AddURIToQueue"
                        [ ("InstanceID", "0")
                        , ("EnqueuedURI", uri)
                        , ("EnqueuedURIMetaData", meta)
                        , ("DesiredFirstTrackNumberEnqueued", T.pack $ show first)
                        , ("EnqueueAsNext", T.pack $ show next)
                        ]


playTemplate :: T.Text
playTemplate =
    avTransportTemplate "Play"
                        [ ("InstanceID", "0")
                        , ("Speed", "1")
                        ]

setAVTransportURITemplate :: T.Text
                          -> T.Text
                          -> T.Text
setAVTransportURITemplate uri meta =
    avTransportTemplate "SetAVTransportURI"
                        [ ("InstanceID", "0")
                        , ("CurrentURI", uri)
                        , ("CurrentURIMetaData", meta)
                        ]

becomeCoordinatorOfStandaloneGroup :: T.Text
becomeCoordinatorOfStandaloneGroup =
    avTransportTemplate "BecomeCoordinatorOfStandaloneGroup"
                        [ ("InstanceID", "0")
                        ]

seekTrackTemplate :: Int
                  -> T.Text
seekTrackTemplate track =
    avTransportTemplate "Seek"
                        [ ("InstanceID", "0")
                        , ("Unit", "TRACK_NR")
                        , ("Target", T.pack $ show track)
                        ]


wrap' :: (String, T.Text)
      -> T.Text
wrap' (w, i) = wrap (fromString w)
                    Nothing
                    i

avTransportTemplate :: T.Text
                    -> [(String, T.Text)]
                    -> T.Text
avTransportTemplate action md =
    let avTransport cts = Format.sformat fmtI
                                         action
                                         avTransportNS
                                         cts
                                         action
        fmtI = "<u:" Format.% Format.stext Format.%
               " " Format.% Format.stext Format.%
               ">" Format.% Format.stext Format.%
               "</u:" Format.% Format.stext Format.%
               ">"
        md' = T.concat $ map wrap' md
    in avTransport md'

soapActionFmt = Format.sformat (Format.stext Format.% "#" Format.% Format.stext)

urlFmt = Format.sformat (Format.stext Format.% ":" Format.% Format.stext)
hostFmt = Format.sformat ("http://" Format.% Format.stext Format.% Format.stext)

soapAction :: T.Text
           -> T.Text
           -> T.Text
           -> IO (Maybe BSL.ByteString)
soapAction host action msg = do
    let opts = defaults & header "Host" .~ [TE.encodeUtf8 host]
                        & header "User-Agent" .~ ["Haskell post"]
                        & header "Content-type" .~ ["text/xml; charset=\"UTF-8\""]
                        & header "Content-length" .~ [BSC.pack $ show $ T.length msg]
                        & header "SOAPAction" .~ [TE.encodeUtf8 $ soapActionFmt avTransportAction action]
        ep = "/MediaRenderer/AVTransport/Control"

    print opts
    print msg
    resp <- postWith opts
                     (T.unpack $ hostFmt host ep)
                     (TE.encodeUtf8 $ envelope msg)

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


fmtRincon = Format.sformat ("x-rincon:" Format.% Format.stext)

groupRoom :: [ZonePlayer]
          -> CliArguments
          -> ZonePlayer
          -> ZonePlayer
          -> IO ()
groupRoom zps args a b = do
    let coordA = findCoordinatorForIp (zpLocation a) zps
        coordB = findCoordinatorForIp (zpLocation b) zps
        addr = let l = zpLocation coordA
               in urlFmt (lUrl l) (lPort l)
    let avMessage = setAVTransportURITemplate (fmtRincon (zpUUID coordB)) ""
    soapAction addr "SetAVTransportURI" avMessage
    return ()

ungroupRoom :: [ZonePlayer]
            -> CliArguments
            -> ZonePlayer
            -> IO ()
ungroupRoom zps args room = do
    let addr = let l = zpLocation room
               in urlFmt (lUrl l) (lPort l)
    let avMessage = becomeCoordinatorOfStandaloneGroup
    soapAction addr "BecomeCoordinatorOfStandaloneGroup" avMessage
    return ()


fmtRinconQueue = Format.sformat ("x-rincon-queue:" Format.% Format.stext Format.% "#0")

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
               in urlFmt (lUrl l) (lPort l)

    putStrLn ("Coord was: " ++ show  coord)
    uuid <- fetchUUID addr
    let avMessage = setAVTransportURITemplate (fmtRinconQueue uuid) ""

    soapAction addr "SetAVTransportURI" avMessage
    soapAction addr "Seek" (seekTrackTemplate trackNo)
    soapAction addr "Play" playTemplate


    return ()

firstTrackE args track = T.pack
                    $ urlEncode
                    $ replace ".flac" ".mp3"
                    $ replace (dir args) "" track
fmtCifs = Format.sformat ("x-file-cifs://asgard/mp3Music" Format.% Format.stext)

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


    let soapMessage = addURIToQueueTemplate (fmtCifs $ firstTrackE args firstTrack)
                                            ""
                                            0
                                            0
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

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
               in urlFmt (lUrl l) (lPort l)

    putStrLn ("Coord was: " ++ show  coord)
    uuid <- fetchUUID addr
    let avMessage = setAVTransportURITemplate (fmtRinconQueue uuid) ""

    soapAction addr "SetAVTransportURI" avMessage
    soapAction addr "Seek" (seekTrackTemplate trackNo)
    soapAction addr "Play" playTemplate


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


    let soapMessage track = addURIToQueueTemplate (fmtCifs $ firstTrackE args track)
                                                  ""
                                                  0
                                                  0
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    putStrLn ("Coord was: " ++ show coord)
    queuedBodys <- mapM (\t -> soapAction addr "AddURIToQueue" (soapMessage t)) tracks'
    return queuedBodys

didlWrapper =
    let dc,upnp,r,ns :: T.Text
        dc = "xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
        upnp = "xmlns:upnp=\"urn:schemas-upnp-org:metadata-1-0/upnp/\""
        r = "xmlns:r=\"urn:schemas-rinconnetworks-com:metadata-1-0/\""
        ns = "xmlns=\"urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/\""
        stext' l r = l Format.% Format.stext Format.% r
    in Format.sformat ("<DIDL-Lite "
                      `stext'` " "
                      `stext'` " "
                      `stext'` " "
                      `stext'`
                      ">"
                      `stext'`
                      "</DIDL-Lite>"
                      )
                      dc
                      upnp
                      r
                      ns

didlTemplate :: T.Text
             -> T.Text
             -> T.Text
             -> T.Text
didlTemplate stId stName email =
    let stext' l r = l Format.% Format.stext Format.% r
    in didlWrapper $ Format.sformat
        ("<item id=\"OOOX" `stext'` " parentID=\"0\" restricted=\"true\">\
        \<dc:title>" `stext'` "</dc:title>\
        \<upnp:class>object.item.audioItem.audioBroadcast</upnp:class>\
        \<desc id=\"cdudn\" nameSpace=\"urn:schemas-rinconnetworks-com:metadata-1-0/\">\
        \SA_RINCON3_" `stext'` "\
        \</desc>\
        \</item>"
        )
        stId
        stName
        email

playPandoraStationLike :: [ZonePlayer]
                       -> CliArguments
                       -> ZonePlayer
                       -> String
                       -> IO ()
playPandoraStationLike zps args@(CliArguments{..}) host like = do
    let coord = findCoordinatorForIp (zpLocation host) zps
    let addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    putStrLn ("Coord was: " ++ show  coord)

    pw <- Pandora.login email password
    st <- Pandora.searchStation pw $ T.pack like
    print st
    st' <- Pandora.createStation pw (Pandora.artistMusicToken $ head $ Pandora.msrArtists st)

    print st'
    let station = Pandora.csrStation st'
        stationId = Pandora.sStationId station
        stationName = Pandora.sStationName station
        metadata = didlTemplate stationId
                                stationName
                                email
    let avMessage = setAVTransportURITemplate (pandoraRadioFmt stationId)
                                              metadata

    soapAction addr "SetAVTransportURI" avMessage
    soapAction addr "Play" playTemplate


    return ()

pandoraRadioFmt = Format.sformat ("pndrradio:" Format.% Format.stext Format.% "?sn=6")

fetchUUID :: T.Text
          -> IO T.Text
fetchUUID host = do
    Just body <- getXMLDescription host
    return $ getUUID body

getXMLDescription :: T.Text
                  -> IO (Maybe BSL.ByteString)
getXMLDescription host = do
    let fmtXmlUri = Format.sformat ("http://" Format.% Format.stext Format.% "/xml/device_description.xml")
    res <- get $ T.unpack $ fmtXmlUri host
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
