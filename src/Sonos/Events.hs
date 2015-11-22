{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Sonos.Events where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.String (IsString)
import Data.Maybe (fromJust)
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
import qualified Data.Text.Encoding as TE
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
     $ Change { cMasterVolume = findPropD' cursor "Volume" "channel" "Master" toInt 0
              , cLFVolume = findPropD' cursor "Volume" "channel" "LF" toInt 0
              , cRFVolume = findPropD' cursor "Volume" "channel" "RF" toInt 0
              , cMasterMute = findPropD' cursor "Mute" "channel" "Master" toInt 0
              , cLFMute = findPropD' cursor "Mute" "channel" "LF" toInt 0
              , cRFMute = findPropD' cursor "Mute" "channel" "RF" toInt 0
              , cBass = findPropD cursor "Bass" toInt 0
              , cTreble = findPropD cursor "Treble" toInt 0
              , cMasterLoudness = findPropD' cursor "Loudness" "channel" "Master" toInt 0
              , cOutputFixed = findPropD cursor "OutputFixed" toInt 0
              , cHeadphonesConnected = findPropD cursor "HeadphoneConnected" toInt 0
              , cSpeakerSize = findPropD cursor "SpeakerSize" toInt 0
              , cSubGain = findPropD cursor "SubGain" toInt 0
              , cSubCrossover = findPropD cursor "SubCrossover" toInt 0
              , cSubPolarity = findPropD cursor "SubPolarity" toInt 0
              , cSubEnabled = findPropD cursor "SubEnabled" toInt 0
              , cSonarEnabled = findPropD cursor "SonarEnabled" toInt 0
              , cSonarCalibrationAvailable = findPropD cursor "SonarCalibrationAvailable" toInt 0
              , cPresetNameList = findProp cursor "PresetNameList" id
              }

propertyTransport cursor = PropertyTransport
     $  Transport { tTransportState = findProp cursor "TransportState" id
                  , tCurrentPlayMode = findPropD cursor "CurrentPlayMode" naiveToPlayMode def
                  , tCurrentCrossfadeMode = findPropD cursor "CurrentCrossfadeMode" toInt 0
                  , tNumberOfTracks = findPropD cursor "NumberOfTracks" toInt 0
                  , tCurrentTrack = findPropD cursor "CurrentTrack" toInt 0
                  , tCurrentSection = findPropD cursor "CurrentSection" toInt 0
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
                  , tSleepTimerGeneration = findPropD cursor "SleepTimerGeneration" toInt 0
                  , tAlarmRunning = findPropD cursor "AlarmRunning" toInt 0
                  , tSnoozeRunning = findPropD cursor "SnoozeRunning" toInt 0
                  , tRestartPending = findPropD cursor "RestartPending" toInt 0
                  , tTransportPlaySpeed = findProp cursor "TransportPlaySpeed" id
                  , tCurrentMediaDuration = findProp cursor "CurrentMediaDuration" id
                  , tRecordStorageMedium = findProp cursor "RecordStorageMedium" id
                  , tPossiblePlaybackStorageMedia = findProp cursor "PossiblePlaybackStorageMedia" id
                  , tPossibleRecordStorageMedia = findProp cursor "PossibleRecordStorageMedia" id
                  , tRecordMediumWriteStatus = findProp cursor "RecordMediumWriteStatus" id
                  , tCurrentRecordQualityMode = findProp cursor "CurrentRecordQualityMode" id
                  , tPossibleRecordQualityModes = findProp cursor "PossibleRecordQualityModes" id
                 }

findProp' :: IsString a => Cursor -> T.Text -> Name -> T.Text -> (T.Text -> a) -> a
findProp' cursor name attr val reader = findPropD' cursor name attr val reader ""

findPropD' :: Cursor -> T.Text -> Name -> T.Text -> (T.Text -> a) -> a -> a
findPropD' cursor name attr val reader d =
    let p = case cursor $// laxElement name >=> attributeIs attr val >=> attribute "val" of
            [] -> d
            [p'] -> reader p'
    in p

findProp :: IsString a => Cursor -> T.Text -> (T.Text -> a) -> a
findProp cursor name reader = findPropD cursor name reader ""

findPropD :: Cursor -> T.Text -> (T.Text -> a) -> a -> a
findPropD cursor name reader d =
    let p = case cursor $// laxElement name >=> attribute "val" of
            [] -> d
            [p'] -> reader p'
    in p

toInt = read . T.unpack

naiveToPlayMode modeStr
    | modeStr == "NORMAL" = PlayMode { shuffle = False, repeat = False, crossfade = False }
    | modeStr == "REPEAT_ALL" = PlayMode { shuffle = False, repeat = True, crossfade = False }
    | modeStr == "SHUFFLE_NOREPEAT" = PlayMode { shuffle = True, repeat = False, crossfade = False }
    | modeStr == "SHUFFLE" = PlayMode { shuffle = True, repeat = True, crossfade = False }

toProperty "LastChange" pc =
    let cursor = fromDocument $ parseText_ def $ TL.fromStrict $ traceShowId pc
        eventNamespace = cursor $| nameNamespace . elementName . (\(NodeElement e) -> e) . node
    in mkProperty cursor eventNamespace
toProperty _ pc = Just $ PropertySimple pc

handleEvent state req body = do
    print $ requestHeaders req
    let hs = M.fromList $ requestHeaders req
        Just sid = M.lookup "SID" hs
        uuid = fst . T.breakOn "_sub" . snd . T.breakOn "RINCON"
    let speaker = (fromJust $ M.lookup (uuid $ TE.decodeUtf8 sid) $ speakers $ state)
    let event = eventToXML body
    updateSpeaker speaker event
    putStrLn $ "***EVENT*** " ++ show event
    return event

updateSpeaker speaker event =
    let Event properties = event
    in do
        speakerData <- liftIO $ atomically $ readTVar speaker
        let lastChangeM = M.lookup "LastChange" properties
        putStrLn $ "Trying to update a speaker with event" ++ show lastChangeM
        case lastChangeM of
          Just lastChange -> do
            let sData = case lastChange of
                    (Just (PropertyChange (Change {..}))) ->
                        speakerData { ssVolume = cMasterVolume
                                    , ssMute = cMasterMute == 0
                                    }
                    (Just (PropertyTransport (Transport {..}))) ->
                        speakerData { ssCurrentTrack = parseXMLMetaData tCurrentTrackMetaData
                                    , ssNextTrack = parseXMLMetaData tNextTrackMetaData
                                    , ssTrackNo = tCurrentTrack
                                    , ssElapsedTime = ""
                                    , ssElapsedTimeFormatted = ""
                                    , ssPlayerState = tTransportState
                                    , ssCurrentPlayMode = tCurrentPlayMode { crossfade = tCurrentCrossfadeMode == 1 } 
                                    }
                    _ -> speakerData
            liftIO $ atomically $ writeTVar speaker sData
          Nothing -> return ()

parseXMLMetaData :: T.Text -> TrackState
parseXMLMetaData body
    | body == "" = def
    | otherwise =
        let cursor = fromDocument $ parseText_ def $ TL.fromStrict body
            [item] = cursor $/ laxElement "item"
            [res] = item $/ laxElement "res"
            [duration] = res $.// attribute "duration"
            [uri] = res $.// content
            streamContent = case item $/ laxElement "streamContent" &.// content of
                              [] -> ""
                              [s] -> s
            radioShowMd = case item $/ laxElement "radioShowMd" &.// content of
                            [] -> ""
                            [r] -> r
            [albumArtURI] = item $/ laxElement "albumArtURI" &.// content
            title = case item $/ laxElement "title" &.// content of
                      [] -> ""
                      [t] -> t
            creator = case item $/ laxElement "creator" &.// content of
                          [] -> ""
                          [c] -> c
            album = case item $/ laxElement "album" &.// content of
                      [] -> ""
                      [a] -> a
            originalTrackNo = case item $/ laxElement "originalTrackNumber" &.// content of
                                [] -> ""
                                [o] -> o
            albumArtist = case item $/ laxElement "albumArtist" &.// content of
                            [] -> creator
                            [a] -> a
            
        in TrackState
        { tsArtist = albumArtist
        , tsTitle = title
        , tsAlbum = album
        , tsAlbumArtURI = albumArtURI
        , tsDuration = durationToInt duration
        , tsURI = uri
        , tsRadioShowMetaData = radioShowMd
        }

durationToInt :: T.Text -> Int
durationToInt t =
    let (h:m:s:_) = map T.unpack $ T.splitOn ":" t
    in (read h * 60 * 60) + (read m * 60) + (read s)

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
