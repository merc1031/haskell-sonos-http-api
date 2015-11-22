{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
module Sonos.Lib where

import Network.Wreq
import Network.HTTP.Base (urlEncode)
import Data.String.Utils
import Text.XML
import Text.XML.Cursor
import Control.Concurrent.STM

import Text.Regex.PCRE

import Sonos.Types

import Control.Monad
import Data.Maybe                           (fromJust, catMaybes)
import Data.String                          (fromString, IsString)
import Sonos.Util                           (findCoordinatorForIp)
import Control.Monad                        (forever)
import Control.Concurrent                   (threadDelay)
import Network.HTTP.Types.Status (status200)
import Control.Lens                         ((^?), (^?!), (.~), (&))
import Data.Monoid                          ((<>))
import Data.Char                            (toLower)

import Sonos.Discover                       (getTopology)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Char8      as BSC
import qualified Web.Spock                  as WS
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Sonos.Plugins.Pandora as Pandora
import qualified HTMLEntities.Builder as HTML
import qualified HTMLEntities.Decoder as HTML
import qualified Data.Text.Format as Fmt
import qualified Formatting as Format
import Formatting (stext, (%), sformat)

import Debug.Trace


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
            Just p -> let fmtPL = sformat $ stext % ":"
                          fmtPR = sformat $ ":" % stext
                      in (fmtPL p, fmtPR p)
            Nothing -> ("","")
        ns = case nameNamespace of
            Just n -> let fmtN = sformat $ " xmlns" % stext % "=\"" % stext % "\""
                      in fmtN pfxs n
            Nothing -> ""
        ec' = case ec of
            Just e -> let fmtEC = sformat $ " " % stext % "encodingStyle=\"" % stext % "\""
                      in fmtEC pfx e
            Nothing -> ""
    in 
        let fmtM = sformat
                 $ "<" %
                        stext %
                        stext %
                        stext %
                        stext %
                   ">" % stext %
                   "</" % stext %
                        stext %
                   ">"
        in fmtM pfx nameLocalName ns ec' content pfx nameLocalName

avTransportAction :: T.Text
avTransportAction = "urn:schemas-upnp-org:service:AVTransport:1"

cdTransportAction :: T.Text
cdTransportAction = "urn:schemas-upnp-org:service:ContentDirectory:1"

avTransportNS :: T.Text
avTransportNS = sformat ("xmlns:u=\"" % stext % "\"") avTransportAction

cdTransportNS :: T.Text
cdTransportNS = sformat ("xmlns:u=\"" % stext % "\"") cdTransportAction

nextTrackTemplate =
    avTransportTemplate "Next"
                        [ ("InstanceID", "0")]

previousTrackTemplate =
    avTransportTemplate "Previous"
                        [ ("InstanceID", "0")]

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

addMultipleURIsToQueueTemplate :: [T.Text]
                               -> [T.Text]
                               -> T.Text
                               -> T.Text
                               -> Int
                               -> Int
                               -> T.Text
addMultipleURIsToQueueTemplate uris metas containerUri containerMetadata first next =
    avTransportTemplate "AddMultipleURIsToQueue"
                        [ ("InstanceID", "0")
                        , ("UpdateID", "0")
                        , ("NumberOfURIs", T.pack $ show $ length uris)
                        , ("EnqueuedURIs", T.intercalate " " uris)
                        , ("EnqueuedURIMetaDatas", T.intercalate " " metas)
                        , ("ContainerURI", containerUri)
                        , ("ContainerMetaData", containerMetadata)
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

browseContentDirectoryTemplate :: T.Text
                               -> T.Text
                               -> T.Text
                               -> Int
                               -> Int
                               -> T.Text
                               -> T.Text
browseContentDirectoryTemplate oid flag filter sidx rqc sort =
    cdTransportTemplate "Browse"
                        [ ("ObjectID", oid)
                        , ("BrowseFlag", flag)
                        , ("Filter", filter)
                        , ("StartingIndex", T.pack $ show sidx)
                        , ("RequestedCount", T.pack $ show rqc)
                        , ("SortCriteria", sort)
                        ]

getMetaData oid = browseContentDirectoryTemplate oid
                                                 "BrowseMetadata"
                                                 "*"
                                                 0
                                                 0
                                                 ""

wrap' :: (String, T.Text)
      -> T.Text
wrap' (w, i) = wrap (fromString w)
                    Nothing
                    i

avTransportTemplate :: T.Text
                    -> [(String, T.Text)]
                    -> T.Text
avTransportTemplate action md = transportTemplate action avTransportNS md

cdTransportTemplate :: T.Text
                    -> [(String, T.Text)]
                    -> T.Text
cdTransportTemplate action md = transportTemplate action cdTransportNS md

transportTemplate :: T.Text
                  -> T.Text
                  -> [(String, T.Text)]
                  -> T.Text
transportTemplate action ns md =
    let avTransport cts = sformat fmtI
                                  action
                                  ns
                                  cts
                                  action
        fmtI = "<u:" % stext %
               " " % stext %
               ">" % stext %
               "</u:" % stext %
               ">"
        md' = T.concat $ map wrap' md
    in avTransport md'

soapActionFmt = sformat (stext % "#" % stext)

urlFmt = sformat (stext % ":" % stext)
hostFmt = sformat ("http://" % stext % stext)


cdSoapAction = soapAction' cdTransportAction "/MediaServer/ContentDirectory/Control"
avSoapAction = soapAction' avTransportAction "/MediaRenderer/AVTransport/Control"

soapAction' t ep h a m = do
    resp <- soapAction t ep h a m
    handle resp
    return resp

handle resp = do
    when (resp ^?! responseStatus /= status200) $ do
        putStrLn $ show (resp ^?! responseBody)

soapAction :: T.Text
           -> T.Text
           -> T.Text
           -> T.Text
           -> T.Text
           -> IO (Response BSL.ByteString)
soapAction transport ep host action msg = do
    let opts = defaults & header "Host" .~ [TE.encodeUtf8 host]
                        & header "User-Agent" .~ ["Haskell post"]
                        & header "Content-type" .~ ["text/xml; charset=\"UTF-8\""]
                        & header "Content-length" .~ [BSC.pack $ show $ T.length msg]
                        & header "SOAPAction" .~ [TE.encodeUtf8 $ soapActionFmt transport action]

    resp <- postWith opts
                     (T.unpack $ hostFmt host ep)
                     (TE.encodeUtf8 $ envelope msg)

    return $ resp

getRoom :: [ZonePlayer]
        -> Room
        -> ZonePlayer
getRoom zps room =
    let rooms = M.fromList
              $ map (\zp@(ZonePlayer {..}) ->
                    let name = zpName
                    in (T.toLower name, zp)
                    )
                    zps

        Just room' = M.lookup (T.toLower $ unRoom room) rooms
    in room'


fmtRincon = sformat ("x-rincon:" % stext)

groupRoom :: State
          -> CliArguments
          -> ZonePlayer
          -> ZonePlayer
          -> IO ()
groupRoom state args a b = do
    zps <- getZPs state
    let coordA = findCoordinatorForIp (zpLocation a) zps
        coordB = findCoordinatorForIp (zpLocation b) zps
        addr = let l = zpLocation coordA
               in urlFmt (lUrl l) (lPort l)
    let avMessage = setAVTransportURITemplate (fmtRincon (zpUUID coordB)) ""
    avSoapAction addr "SetAVTransportURI" avMessage
    return ()

ungroupRoom :: State
            -> CliArguments
            -> ZonePlayer
            -> IO ()
ungroupRoom state args room = do
    let addr = let l = zpLocation room
               in urlFmt (lUrl l) (lPort l)
    let avMessage = becomeCoordinatorOfStandaloneGroup
    avSoapAction addr "BecomeCoordinatorOfStandaloneGroup" avMessage
    return ()


fmtRinconQueue = sformat ("x-rincon-queue:" % stext % "#0")

queueAndPlayTrackLike :: State
                      -> CliArguments
                      -> ZonePlayer
                      -> String
                      -> IO ()
queueAndPlayTrackLike state args host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
    queuedBody <- queueTrackLike state args host like
    let trackNo = getTrackNum queuedBody
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    uuid <- fetchUUID addr
    let avMessage = setAVTransportURITemplate (fmtRinconQueue uuid) ""

    avSoapAction addr "SetAVTransportURI" avMessage
    avSoapAction addr "Seek" (seekTrackTemplate trackNo)
    avSoapAction addr "Play" playTemplate


    return ()

queueTrackLike :: State
               -> CliArguments
               -> ZonePlayer
               -> String
               -> IO BSL.ByteString
queueTrackLike state args host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
    tracksM <- atomically $ readTVar $ tracks $ mdb state
    let tracks = lookupMany (T.pack $ "*" ++ like ++ "*") tracksM
        tracks' = tracks
        firstTrack = snd $ head tracks
    putStrLn $ "First track is:" ++ show firstTrack


    let soapMessage = addURIToQueueTemplate firstTrack
                                            ""
                                            0
                                            0
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    resp <- avSoapAction addr "AddURIToQueue" soapMessage
    let Just queuedBody = resp ^? responseBody
    return queuedBody

queueAndPlayArtistLike :: State
                       -> CliArguments
                       -> ZonePlayer
                       -> String
                       -> IO ()
queueAndPlayArtistLike state args host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
    queuedBodys <- queueArtistLike state args host like
    let trackNo = getTrackNum $ fromJust $ head $ queuedBodys
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    uuid <- fetchUUID addr
    let avMessage = setAVTransportURITemplate (fmtRinconQueue uuid) ""

    avSoapAction addr "SetAVTransportURI" avMessage
    avSoapAction addr "Seek" (seekTrackTemplate trackNo)
    avSoapAction addr "Play" playTemplate


    return ()

nextTrack :: State
          -> CliArguments
          -> ZonePlayer
          -> IO ()
nextTrack state args host = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)
    avSoapAction addr "Next" nextTrackTemplate
    return ()

previousTrack :: State
              -> CliArguments
              -> ZonePlayer
              -> IO ()
previousTrack state args host = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)
    avSoapAction addr "Previous" previousTrackTemplate
    return ()


getZPs = atomically . readTVar . zps

hadGlobs :: T.Text
         -> Bool
hadGlobs = T.isInfixOf "*"

convertGlobsToRegex = rejigger
    where
        rejigger = TE.encodeUtf8 . regex
        regex = T.replace "*" "[^.]*?"

lookupMany :: T.Text
           -> M.Map T.Text T.Text
           -> [(T.Text, T.Text)]
lookupMany s m = catMaybes $ if hadGlobs s
                    then map Just
                       $ M.toList
                       $ M.filterWithKey (\k _ -> TE.encodeUtf8 k =~ (convertGlobsToRegex $ T.toLower s)) m
                    else [ ( (s,) <$> ) $ M.lookup s m]

queueArtistLike :: State
                -> CliArguments
                -> ZonePlayer
                -> String
                -> IO [Maybe BSL.ByteString]
queueArtistLike state args host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
    artistsM <- atomically $ readTVar $ artists $ mdb state
    let tracks = lookupMany (T.pack $ "*" ++ like ++ "*") artistsM
        tracks' = tracks
    putStrLn $ "Tracks are:" ++ show tracks'


    let soapMessage track = addURIToQueueTemplate track
                                                  ""
                                                  0
                                                  0
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    mapM (\t -> (\r -> r ^? responseBody) <$> avSoapAction addr "AddURIToQueue" (soapMessage $ snd t)) tracks'


didlWrapper c =
    let dc,upnp,r,ns :: T.Text
        dc = "xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
        upnp = "xmlns:upnp=\"urn:schemas-upnp-org:metadata-1-0/upnp/\""
        r = "xmlns:r=\"urn:schemas-rinconnetworks-com:metadata-1-0/\""
        ns = "xmlns=\"urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/\""
        stext' l r = l % stext % r
    in TL.toStrict
     $ TLB.toLazyText
     $ HTML.text
     $ sformat ("<DIDL-Lite "
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
               c

didlTemplate :: T.Text
             -> T.Text
             -> T.Text
             -> T.Text
didlTemplate stId stName email =
    let stext' l r = l % stext % r
    in didlWrapper
     $ sformat
       ("<item id=\"OOOX" `stext'` "\" parentID=\"0\" restricted=\"true\">\
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

playPandoraStationLike :: State
                       -> CliArguments
                       -> ZonePlayer
                       -> String
                       -> IO ()
playPandoraStationLike state args@(CliArguments{..}) host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    pw <- Pandora.login email password
    st <- Pandora.searchStation pw $ T.pack like
    st' <- Pandora.createStation pw (Pandora.artistMusicToken $ head $ Pandora.msrArtists st)

    let station = Pandora.csrStation st'
        stationId = Pandora.sStationId station
        stationName = Pandora.sStationName station
        metadata = didlTemplate stationId
                                stationName
                                email
        avMessage = setAVTransportURITemplate (pandoraRadioFmt stationId)
                                              metadata

    putStrLn $ show avMessage
    avSoapAction addr "SetAVTransportURI" avMessage
    putStrLn "Time to play"
    avSoapAction addr "Play" playTemplate


    return ()

pandoraRadioFmt = sformat ("pndrradio:" % stext % "?sn=6")


browseContentDirectory :: State
                       -> CliArguments
                       -> T.Text -- Category A:ARTIST A:ALBUM A:TRACK
                       -> T.Text -- Filter?
                       -> Int -- Start
                       -> Int -- Count
                       -> T.Text --sort criteria
                       -> IO (Int, Int, [(T.Text, T.Text)])

browseContentDirectory state args cat filt s c sor = do
    zps <- getZPs state
    let coord = head zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)


        cdMessage = browseContentDirectoryTemplate cat
                                                   "BrowseDirectChildren"
                                                   filt
                                                   s
                                                   c
                                                   sor

    resp <- cdSoapAction addr "Browse" cdMessage

    let Just body = resp ^? responseBody
        structured = browsedContent (BrowseDefault cat) body
    return structured

browseMetaData :: State
               -> CliArguments
               -> T.Text -- Category A:ARTIST A:ALBUM A:TRACK
               -> IO (Int, Int, [(T.Text, T.Text)])
browseMetaData state args cat = do
    zps <- getZPs state
    let coord = head zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)


        cdMessage = getMetaData cat

    resp <- cdSoapAction addr "Browse" cdMessage

    let Just body = resp ^? responseBody
        structured = browsedContent (BrowseSpecified "container") body

    putStrLn $ show body
    return structured

lookupWrapper :: BrowseContainer -> T.Text
lookupWrapper (BrowseSpecified c) = c
lookupWrapper (BrowseDefault k) = fromJust
                $ M.lookup k
                $ M.fromList [ ("A:ARTIST", "container")
                             , ("A:ALBUM", "container")
                             , ("A:TRACKS", "item")
                             ]

data BrowseContainer = BrowseDefault T.Text | BrowseSpecified T.Text
browsedContent :: BrowseContainer
               -> BSL.ByteString
               -> (Int, Int, [(T.Text, T.Text)])
browsedContent typeKey body =
    let cursor = fromDocument $ parseLBS_ def body
        wrapper = lookupWrapper typeKey
        [root] = cursor $/ element "{http://schemas.xmlsoap.org/soap/envelope/}Body"
        [resultO] = root $/ element (Name "BrowseResponse" (Just "urn:schemas-upnp-org:service:ContentDirectory:1") (Just "u"))
        [numReturned] = resultO $/ element "NumberReturned"
                           &// content
        [totalMatches] = resultO $/ element "TotalMatches"
                           &// content

        [result] = resultO $/ element "Result"
        result' = (T.concat $ result $/ content)

        resultCursor = fromDocument $ parseLBS_ def $ BSL.fromStrict $ TE.encodeUtf8 result'

        things = resultCursor $/ element (Name wrapper (Just "urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/") Nothing)

        res = map transform things
        transform elem =
            let title = element (Name "title" (Just "http://purl.org/dc/elements/1.1/") (Just "dc")) &// content
                link = element "{urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/}res" &// content
            in (head $ elem $/ title, head $ elem $/ link)
    in (read $ T.unpack numReturned, read $ T.unpack totalMatches, res)

fetchUUID :: T.Text
          -> IO T.Text
fetchUUID host = do
    Just body <- getXMLDescription host
    return $ getUUID body

getXMLDescription :: T.Text
                  -> IO (Maybe BSL.ByteString)
getXMLDescription host = do
    let fmtXmlUri = sformat ("http://" % stext % "/xml/device_description.xml")
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


