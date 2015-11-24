{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Sonos.Commands where

import Control.Exception (catch, SomeException)
import Network.Wreq
import Data.String.Utils
import Sonos.Types
import Sonos.XML
import Control.Monad (when)

import Text.XML
import Data.String                          ( fromString
                                            , IsString
                                            )
import Network.HTTP.Types.Status            (status200)
import Control.Lens                         ( (^?)
                                            , (^?!)
                                            , (.~)
                                            , (&)
                                            )

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Char8      as BSC
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB
import qualified HTMLEntities.Builder       as HTML
import qualified HTMLEntities.Decoder       as HTML
import Formatting                           ( stext
                                            , (%)
                                            , sformat
                                            )

fmtRincon = sformat ("x-rincon:" % stext)
fmtRinconQueue = sformat ("x-rincon-queue:" % stext % "#0")

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

rcTransportAction :: T.Text
rcTransportAction = "urn:schemas-upnp-org:service:RenderingControl:1"

grcTransportAction :: T.Text
grcTransportAction = "urn:schemas-upnp-org:service:GroupRenderingControl:1"

avTransportNS :: T.Text
avTransportNS = sformat ("xmlns:u=\"" % stext % "\"") avTransportAction

cdTransportNS :: T.Text
cdTransportNS = sformat ("xmlns:u=\"" % stext % "\"") cdTransportAction

rcTransportNS :: T.Text
rcTransportNS = sformat ("xmlns:u=\"" % stext % "\"") rcTransportAction

grcTransportNS :: T.Text
grcTransportNS = sformat ("xmlns:u=\"" % stext % "\"") grcTransportAction

type Template = (T.Text, T.Text)

nextTrackTemplate :: Template
nextTrackTemplate =
    let a = "Next"
    in (a, avTransportTemplate a
                               [ ("InstanceID", "0")])

previousTrackTemplate :: Template
previousTrackTemplate =
    let a = "Previous"
    in (a, avTransportTemplate a
                               [ ("InstanceID", "0")])

volumeTemplate :: Int
               -> Template
volumeTemplate volume =
    let a = "SetVolume"
    in (a, rcTransportTemplate a
                               [ ("InstanceID", "0")
                               , ("Channel", "Master")
                               , ("DesiredVolume", T.pack $ show volume)
                               ])

groupVolumeTemplate :: Int
                    -> Template
groupVolumeTemplate volume =
    let a = "SetGroupVolume"
    in (a, grcTransportTemplate a
                                [ ("InstanceID", "0")
                                , ("Channel", "Master")
                                , ("DesiredVolume", T.pack $ show volume)
                                ])

addURIToQueueTemplate :: T.Text
                      -> T.Text
                      -> Int
                      -> Int
                      -> Template
addURIToQueueTemplate uri meta first next =
    let a = "AddURIToQueue"
    in (a, avTransportTemplate a
                               [ ("InstanceID", "0")
                               , ("EnqueuedURI", uri)
                               , ("EnqueuedURIMetaData", meta)
                               , ("DesiredFirstTrackNumberEnqueued", T.pack $ show first)
                               , ("EnqueueAsNext", T.pack $ show next)
                               ])

addMultipleURIsToQueueTemplate :: [T.Text]
                               -> [T.Text]
                               -> T.Text
                               -> T.Text
                               -> Int
                               -> Int
                               -> Template
addMultipleURIsToQueueTemplate uris metas containerUri containerMetadata first next =
    let a = "AddMultipleURIsToQueue"
    in (a, avTransportTemplate a
                               [ ("InstanceID", "0")
                               , ("UpdateID", "0")
                               , ("NumberOfURIs", T.pack $ show $ length uris)
                               , ("EnqueuedURIs", T.intercalate " " uris)
                               , ("EnqueuedURIMetaDatas", T.intercalate " " metas)
                               , ("ContainerURI", containerUri)
                               , ("ContainerMetaData", containerMetadata)
                               , ("DesiredFirstTrackNumberEnqueued", T.pack $ show first)
                               , ("EnqueueAsNext", T.pack $ show next)
                               ])

playTemplate :: Template
playTemplate =
    let a = "Play"
    in (a, avTransportTemplate a
                               [ ("InstanceID", "0")
                               , ("Speed", "1")
                               ])

pauseTemplate :: Template
pauseTemplate =
    let a = "Pause"
    in (a, avTransportTemplate a
                               [ ("InstanceID", "0")])

setAVTransportURITemplate :: T.Text
                          -> T.Text
                          -> Template
setAVTransportURITemplate uri meta =
    let a = "SetAVTransportURI"
    in (a, avTransportTemplate a
                               [ ("InstanceID", "0")
                               , ("CurrentURI", uri)
                               , ("CurrentURIMetaData", meta)
                               ])

becomeCoordinatorOfStandaloneGroup :: Template
becomeCoordinatorOfStandaloneGroup =
    let a = "BecomeCoordinatorOfStandaloneGroup"
    in (a, avTransportTemplate a
                               [ ("InstanceID", "0")
                               ])

seekTrackTemplate :: Int
                  -> Template
seekTrackTemplate track =
    let a = "Seek"
    in (a, avTransportTemplate a
                               [ ("InstanceID", "0")
                               , ("Unit", "TRACK_NR")
                               , ("Target", T.pack $ show track)
                               ])

browseContentDirectoryTemplate :: T.Text
                               -> T.Text
                               -> T.Text
                               -> Int
                               -> Int
                               -> T.Text
                               -> Template
browseContentDirectoryTemplate oid flag filter sidx rqc sort =
    let a = "Browse"
    in (a, cdTransportTemplate a
                               [ ("ObjectID", oid)
                               , ("BrowseFlag", flag)
                               , ("Filter", filter)
                               , ("StartingIndex", T.pack $ show sidx)
                               , ("RequestedCount", T.pack $ show rqc)
                               , ("SortCriteria", sort)
                               ])

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

rcTransportTemplate :: T.Text
                    -> [(String, T.Text)]
                    -> T.Text
rcTransportTemplate action md = transportTemplate action rcTransportNS md

grcTransportTemplate :: T.Text
                     -> [(String, T.Text)]
                     -> T.Text
grcTransportTemplate action md = transportTemplate action grcTransportNS md


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
rcSoapAction = soapAction' rcTransportAction "/MediaRenderer/RenderingControl/Control"
grcSoapAction = soapAction' grcTransportAction "/MediaRenderer/GroupRenderingControl/Control"

avSoapAction' = soapAction avTransportAction "/MediaRenderer/AVTransport/Control"

soapAction' t ep h t' = do
    resp <- soapAction t ep h t'
    handle resp
    return resp

handle resp = do
    when (resp ^?! responseStatus /= status200) $ do
        putStrLn $ show (resp ^?! responseBody)

soapAction :: T.Text
           -> T.Text
           -> T.Text
           -> Template
           -> IO (Response BSL.ByteString)
soapAction transport ep host (action, msg) = do
    let opts = defaults & header "Host" .~ [TE.encodeUtf8 host]
                        & header "User-Agent" .~ ["Haskell post"]
                        & header "Content-type" .~ ["text/xml; charset=\"UTF-8\""]
                        & header "Content-length" .~ [BSC.pack $ show $ T.length msg]
                        & header "SOAPAction" .~ [TE.encodeUtf8 $ soapActionFmt transport action]

    resp <- postWith opts
                     (T.unpack $ hostFmt host ep)
                     (TE.encodeUtf8 $ envelope msg)

    return $ resp

getXMLDescription :: T.Text
                  -> IO (Maybe BSL.ByteString)
getXMLDescription host = do
    let fmtXmlUri = sformat ("http://" % stext % "/xml/device_description.xml")
    res <- get $ T.unpack $ fmtXmlUri host
    return $ res ^? responseBody


fetchUUID :: T.Text
          -> IO T.Text
fetchUUID host = do
    Just body <- getXMLDescription host
    return $ getUUID body

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
