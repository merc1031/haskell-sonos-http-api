{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Sonos.Events where

import Network.Info
import Network.Wreq
import Sonos.Types
import Network.Wai (Request (..))
import Control.Lens                         ((^?), (.~), (&))
import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Char (toLower)
import Data.List
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BSL
import qualified Safe as Safe
import Debug.Trace

--getLocalAddr = ipv4 . getNetworkInterfaces
subAll zp me port = do
    let sub' = sub zp me port

    -- Track change events...
    sub' "/MediaRenderer/AVTransport/Event"
    -- Volume and mute events
    sub' "/MediaRenderer/RenderingControl/Event"
    -- Group volume and group mutes
    sub' "/MediaRenderer/GroupRenderingControl/Event"
    -- Queue and favorite events
    sub' "/MediaServer/ContentDirectory/Event"

sub :: ZonePlayer
    -> String
    -> Int
    -> T.Text
    -> IO ()
sub zp me port event = do
    print $ "Subscribing to " ++ show zp
    let loc = zpLocation zp

    let opts =  defaults & header "CALLBACK" .~ [BSC.pack $ "<http://" ++ me ++ ":" ++ show port ++ "/eventSub>"]
                         & header "NT" .~  ["upnp:event"]
    resp <- customMethodWith "SUBSCRIBE" opts (T.unpack $ fmtLocation loc "http://" event)
    let Just headers = fmap M.fromList $ resp ^? responseHeaders
        Just timeout = M.lookup "TIMEOUT" headers
        Just sid = M.lookup "SID" headers
        timeoutSeconds :: Maybe Int
        timeoutSeconds = case map toLower $ BSC.unpack timeout of
                           (stripPrefix "second-" -> Just rest) -> Just $ read rest
                           "infinite" -> Nothing

    case timeoutSeconds of
      Just seconds -> do
          let timeoutReal = floor $ fromIntegral seconds * (0.85 :: Double)
          let resub = do
                _ <- async $ do
                    print "Start async renew"
                    let renew' t = do
                            print $ "Renewing at rate" ++ (show $ t * 1000000)
                            threadDelay $ t * 1000000
                            newT <- renew zp sid timeout event
                            case newT of
                                Just sec -> renew' sec
                                Nothing -> return ()
                    renew' timeoutReal
                return ()
          resub
          return ()

      Nothing -> return ()
    return ()


renew :: ZonePlayer
      -> BS.ByteString
      -> BS.ByteString
      -> T.Text
      -> IO (Maybe Int)
renew zp sid timeoutH event = do
    print $ "Renewing to " ++ show zp
    let loc = zpLocation zp

    let opts =  defaults & header "SID" .~ [sid]
                         & header "TIMEOUT" .~  [timeoutH]
    resp <- customMethodWith "SUBSCRIBE" opts (T.unpack $ fmtLocation loc "http://" event)
    let Just headers = fmap M.fromList $ resp ^? responseHeaders
        Just timeout = M.lookup "TIMEOUT" headers
        timeoutSeconds :: Maybe Int
        timeoutSeconds = case map toLower $ BSC.unpack timeout of
                            (stripPrefix "second-" -> Just rest) -> Just $ read rest
                            "infinite" -> Nothing

    return timeoutSeconds

mkProperty :: Cursor
           -> Maybe T.Text
           -> Maybe Property
mkProperty cursor Nothing = Nothing
mkProperty cursor (Just ns)
    | ns == "urn:schemas-upnp-org:metadata-1-0/RCS/" = Just $ propertyChange cursor
    | ns == "urn:schemas-upnp-org:metadata-1-0/AVT/" = Just $ propertyTransport cursor

propertyChange cursor = PropertyChange
     $ Change { cMasterVolume = findProp' cursor "Volume" "channel" "Master" toInt
              , cLFVolume = findProp' cursor "Volume" "channel" "LF" toInt
              , cRFVolume = findProp' cursor "Volume" "channel" "RF" toInt
              , cMasterMute = findProp' cursor "Mute" "channel" "Master" toInt
              , cLFMute = findProp' cursor "Mute" "channel" "LF" toInt
              , cRFMute = findProp' cursor "Mute" "channel" "RF" toInt
              , cBass = findProp cursor "Bass" toInt
              , cTreble = findProp cursor "Treble" toInt
              , cMasterLoudness = findProp' cursor "Loudness" "channel" "Master" toInt
              , cOutputFixed = findProp cursor "OutputFixed" toInt
              , cHeadphonesConnected = findProp cursor "HeadphoneConnected" toInt
              , cSpeakerSize = findProp cursor "SpeakerSize" toInt
              , cSubGain = findProp cursor "SubGain" toInt
              , cSubCrossover = findProp cursor "SubCrossover" toInt
              , cSubPolarity = findProp cursor "SubPolarity" toInt
              , cSubEnabled = findProp cursor "SubEnabled" toInt
              , cSonarEnabled = findProp cursor "SonarEnabled" toInt
              , cSonarCalibrationAvailable = findProp cursor "SonarCalibrationAvailable" toInt
              , cPresetNameList = findProp cursor "PresetNameList" id
              }

propertyTransport cursor = PropertyTransport
     $  Transport { tTransportState = findProp cursor "TransportState" id
                  , tCurrentPlayMode = findProp cursor "CurrentPlayMode" id
                  , tCurrentCrossfadeMode = findProp cursor "CurrentCrossfadeMode" toInt
                  , tNumberOfTracks = findProp cursor "NumberOfTracks" toInt
                  , tCurrentTrack = findProp cursor "CurrentTrack" toInt
                  , tCurrentSection = findProp cursor "CurrentSection" toInt
                  , tCurrentTrackURI = findProp cursor "CurrentTrackURI" id
                  , tCurrentTrackDuration = findProp cursor "CurrentTrackDuration" id
                  , tCurrentTrackMetaData = findProp cursor "CurrentTrackMetaData" id
                  , tNextTrackURI = findProp cursor "NextTrackURI" id
                  , tNextTrackMetaData = findProp cursor "NextTrackMetaData" id
                  , tEnqueuedTransportURI = findProp cursor "EnqueuedTransportURI" id
                  , tEnqueuedTransportMetaData = findProp cursor "EnqueuedTransportMetaData" id
                  , tPlaybackStorageMedium = findProp cursor "PlaybackStorageMedium" id
                  , tAVTransportURI = findProp cursor "AVTransportURI" id
                  , tAVTransportURIMetaData = findProp cursor "AVTransportURIMetaData" id
                  , tNextAVTransportURI = findProp cursor "NextAVTransportURI" id
                  , tNextAVTransportURIMetaData = findProp cursor "NextAVTransportURIMetaData" id
                  , tCurrentTransportActions = findProp cursor "CurrentTransportActions" id
                  , tCurrentValidPlayModes = findProp cursor "CurrentValidPlayModes" id
                  , tMuseSessions = findProp cursor "MuseSessions" id
                  , tTransportStatus = findProp cursor "TransportStatus" id
                  , tSleepTimerGeneration = findProp cursor "SleepTimerGeneration" toInt
                  , tAlarmRunning = findProp cursor "AlarmRunning" toInt
                  , tSnoozeRunning = findProp cursor "SnoozeRunning" toInt
                  , tRestartPending = findProp cursor "RestartPending" toInt
                  , tTransportPlaySpeed = findProp cursor "TransportPlaySpeed" id
                  , tCurrentMediaDuration = findProp cursor "CurrentMediaDuration" id
                  , tRecordStorageMedium = findProp cursor "RecordStorageMedium" id
                  , tPossiblePlaybackStorageMedia = findProp cursor "PossiblePlaybackStorageMedia" id
                  , tPossibleRecordStorageMedia = findProp cursor "PossibleRecordStorageMedia" id
                  , tRecordMediumWriteStatus = findProp cursor "RecordMediumWriteStatus" id
                  , tCurrentRecordQualityMode = findProp cursor "CurrentRecordQualityMode" id
                  , tPossibleRecordQualityModes = findProp cursor "PossibleRecordQualityModes" id
                 }

findProp' cursor name attr val reader =
    let p = case cursor $// laxElement name >=> attributeIs attr val >=> attribute "val" of
            [] -> ""
            [p'] -> p'
    in reader p
findProp cursor name reader =
    let p = case cursor $// laxElement name >=> attribute "val" of
            [] -> ""
            [p'] -> p'
    in reader p
toInt = read . T.unpack

toProperty "LastChange" pc =
    let cursor = fromDocument $ parseText_ def $ TL.fromStrict $ traceShowId pc
        eventNamespace = cursor $| nameNamespace . elementName . (\(NodeElement e) -> e) . node
    in mkProperty cursor eventNamespace
toProperty _ pc = Just $ PropertySimple pc

handleEvent req body = do
    print $ requestHeaders req
    let hs = M.fromList $ requestHeaders req
        Just sid = M.lookup "SID" hs
    let event = eventToXML body
    putStrLn $ "***EVENT*** " ++ show event
    return event


eventToXML :: BS.ByteString
           -> Event
eventToXML body =
    let cursor = fromDocument $ parseLBS_ def $ BSL.fromStrict $ traceShowId body
        pset = cursor $// laxElement "property"
        properties = map getProperty pset
        getProperty curs =
            let [innerElement] = curs $/ anyElement
                realName = innerElement $| nameLocalName . elementName . (\(NodeElement e) -> e) . node
                propertyContents = case innerElement $/ content of
                    [] -> ""
                    [p] -> p
                propertyValue = toProperty realName propertyContents
            in (realName , propertyValue)
    in Event $ M.fromList properties
