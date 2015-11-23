{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sonos.Types where

import Control.Concurrent.STM
import Web.PathPieces
import Data.Default

import Data.Aeson                   ( ToJSON(..)
                                    , object
                                    , (.=)
                                    )
import qualified Data.Text          as T
import qualified Formatting         as Format
import qualified Data.Map.Strict    as M

newtype Room = Room { unRoom :: T.Text } deriving (PathPiece)
data Op = Pl | Mi | E
    deriving Show

instance PathPiece Op where
    fromPathPiece text
        | text == "+" = Just Pl
        | text == "-" = Just Mi
        | text == "=" = Just E
        | otherwise = Nothing
    toPathPiece Pl = "+"
    toPathPiece Mi = "-"
    toPathPiece E = "="

data MusicDB = MusicDB
    { artists :: TVar (M.Map T.Text T.Text)
    , albums :: TVar (M.Map T.Text T.Text)
    , tracks :: TVar (M.Map T.Text T.Text)
    }

data State = State
    { zps :: TVar [ZonePlayer]
    , speakers :: M.Map T.Text (TVar SpeakerState)
    , mdb :: MusicDB
    }

newtype PandoraEmail = PandoraEmail { unPandoraEmail :: T.Text }
    deriving Show
newtype PandoraPassword = PandoraPassword { unPandoraPassword :: T.Text }
newtype SongzaId = SongzaId { unSongzaId :: T.Text }
    deriving Show

instance Show PandoraPassword where
    show PandoraPassword{} = "PandoraPassword"

data CliArguments = CliArguments
    { caPandoraEmail :: !PandoraEmail
    , caPandoraPassword :: !PandoraPassword
    , caSongzaId :: !SongzaId
    }

data TrackState = TrackState
    { tsArtist :: !T.Text
    , tsTitle :: !T.Text
    , tsAlbum :: !T.Text
    , tsAlbumArtURI :: !T.Text
    , tsDuration :: !Int
    , tsURI :: !T.Text
    , tsRadioShowMetaData :: !T.Text
    } deriving Show

instance Default TrackState where
    def = TrackState
        { tsArtist = ""
        , tsTitle = ""
        , tsAlbum = ""
        , tsAlbumArtURI = ""
        , tsDuration = 0
        , tsURI = ""
        , tsRadioShowMetaData = ""
        }

instance ToJSON TrackState where
    toJSON (TrackState {..}) = object [
              "artist" .= tsArtist
            , "title" .= tsTitle
            , "album" .= tsAlbum
            , "albumArtURI" .= tsAlbumArtURI
            , "duration" .= tsDuration
            , "uri" .= tsURI
            , "radioShowMetaData" .= tsRadioShowMetaData
        ]

data PlayMode = PlayMode
    { shuffle :: !Bool
    , repeat :: !Bool
    , crossfade :: !Bool
    } deriving Show

instance Default PlayMode where
    def = PlayMode
        { shuffle = False
        , repeat = False
        , crossfade = False
        }

instance ToJSON PlayMode where
    toJSON (PlayMode {..}) = object [
              "shuffle" .= shuffle
            , "repeat" .= repeat
            , "crossfade" .= crossfade
        ]

data SpeakerState = SpeakerState
    { ssCurrentTrack :: !TrackState
    , ssNextTrack :: !TrackState
    , ssVolume :: !Int
    , ssMute :: !Bool
    , ssTrackNo :: !Int
    , ssElapsedTime :: !T.Text
    , ssElapsedTimeFormatted :: !T.Text
    , ssPlayerState :: !T.Text
    , ssCurrentPlayMode :: !PlayMode
    } deriving Show

instance Default SpeakerState where
    def = SpeakerState
        { ssCurrentTrack = def
        , ssNextTrack = def
        , ssVolume = 0
        , ssMute = False
        , ssTrackNo = 0
        , ssElapsedTime = ""
        , ssElapsedTimeFormatted = ""
        , ssPlayerState = "STOPPED"
        , ssCurrentPlayMode = def
        }

instance ToJSON SpeakerState where
    toJSON (SpeakerState {..}) = object [
              "currentTrack" .= ssCurrentTrack
            , "nextTrack" .= ssNextTrack
            , "volume" .= ssVolume
            , "mute" .= ssMute
            , "trackNo" .= ssTrackNo
            , "elapsedTime" .= ssElapsedTime
            , "elapsedTimeFormatted" .= ssElapsedTimeFormatted
            , "playerState" .= ssPlayerState
            , "currentPlayMode" .= ssCurrentPlayMode
        ]

data ReplyState = ReplyState
    { rsState :: !SpeakerState
    , rsCoordinatorState :: !SpeakerState
    , rsCurrentTrack :: !TrackState
    , rsNextTrack :: !TrackState
    , rsVolume :: !Int
    , rsMute :: !Bool
    , rsTrackNo :: !Int
    , rsElapsedTime :: !T.Text
    , rsElapsedTimeFormatted :: !T.Text
    , rsZoneState :: !T.Text
    , rsPlayerState :: !T.Text
    , rsZonePlayMode :: !PlayMode
    } deriving Show

instance ToJSON ReplyState where
    toJSON (ReplyState {..}) = object [
              "state" .= rsState
            , "coordinatorState" .= rsCoordinatorState
            , "currentTrack" .= rsCurrentTrack
            , "nextTrack" .= rsNextTrack
            , "volume" .= rsVolume
            , "mute" .= rsMute
            , "trackNo" .= rsTrackNo
            , "elapsedTime" .= rsElapsedTime
            , "elapsedTimeFormatted" .= rsElapsedTimeFormatted
            , "zoneState" .= rsZoneState
            , "playerState" .= rsPlayerState
            , "zonePlayMode" .= rsZonePlayMode
        ]

data Location = Location
    { lProto :: !T.Text
    , lUrl :: !T.Text
    , lPort :: !T.Text
    , lEndpoint :: T.Text
    } deriving Show

data SonosDiscovery = SonosDiscovery
    { sdLocation :: !Location
    , sdServer :: !String
    , sdST :: !String
    , sdUSN :: !String
    , sdHousehold :: !String
    , sdBootSeq :: !String
    , sdWifiMode :: !String
    } deriving Show

data ZonePlayer = ZonePlayer 
    { zpGroup :: !String
    , zpCoordinator :: !Bool
    , zpLocation :: !Location
    , zpBootSeq :: !String
    , zpUUID :: !T.Text
    , zpName :: !T.Text
    } deriving (Show)

data Change =
    Change { cMasterVolume :: !Int
           , cLFVolume :: !Int
           , cRFVolume :: !Int
           , cMasterMute :: !Int
           , cLFMute :: !Int
           , cRFMute :: !Int
           , cBass :: !Int
           , cTreble :: !Int
           , cMasterLoudness :: !Int
           , cOutputFixed :: !Int
           , cHeadphonesConnected :: !Int
           , cSpeakerSize :: !Int
           , cSubGain :: !Int
           , cSubCrossover :: !Int
           , cSubPolarity :: !Int
           , cSubEnabled :: !Int
           , cSonarEnabled :: !Int
           , cSonarCalibrationAvailable :: !Int
           , cPresetNameList :: !T.Text
           } deriving Show

data Transport =
    Transport { tTransportState :: !T.Text
              , tCurrentPlayMode :: !PlayMode
              , tCurrentCrossfadeMode :: !Int
              , tNumberOfTracks :: !Int
              , tCurrentTrack :: !Int
              , tCurrentSection :: !Int
              , tCurrentTrackURI :: !T.Text
              , tCurrentTrackDuration :: !T.Text
              , tCurrentTrackMetaData :: !T.Text
              , tNextTrackURI :: !T.Text
              , tNextTrackMetaData :: !T.Text
              , tEnqueuedTransportURI :: !T.Text
              , tEnqueuedTransportMetaData :: !T.Text
              , tPlaybackStorageMedium :: !T.Text
              , tAVTransportURI :: !T.Text
              , tAVTransportURIMetaData :: !T.Text
              , tNextAVTransportURI :: !T.Text
              , tNextAVTransportURIMetaData :: !T.Text
              , tCurrentTransportActions :: !T.Text
              , tCurrentValidPlayModes :: !T.Text
              , tMuseSessions :: !T.Text
              , tTransportStatus :: !T.Text
              , tSleepTimerGeneration :: !Int
              , tAlarmRunning :: !Int
              , tSnoozeRunning :: !Int
              , tRestartPending :: !Int
              , tTransportPlaySpeed :: !T.Text
              , tCurrentMediaDuration :: !T.Text
              , tRecordStorageMedium :: !T.Text
              , tPossiblePlaybackStorageMedia :: !T.Text
              , tPossibleRecordStorageMedia :: !T.Text
              , tRecordMediumWriteStatus :: !T.Text
              , tCurrentRecordQualityMode :: !T.Text
              , tPossibleRecordQualityModes :: !T.Text
              } deriving Show

data Property = PropertySimple T.Text
              | PropertyChange Change
              | PropertyTransport Transport
              deriving Show
data Event =
      Event {
            eProperties :: M.Map T.Text (Maybe Property)
            } deriving Show


instance ToJSON ZonePlayer where
    toJSON ZonePlayer {..} = object [
              "group" .= zpGroup
            , "coordinator" .= zpCoordinator
            , "location" .= zpLocation
            , "bootSeq" .= zpBootSeq
            , "uuid" .= zpUUID
            , "name" .= zpName
        ]

instance ToJSON Location where
    toJSON Location {..} = object [
              "proto" .= lProto
            , "url" .= lUrl
            , "port" .= lPort
            , "endpoint" .= lEndpoint
        ]

fmtLocation (Location{..}) protoO endpoint =
    Format.sformat (Format.stext Format.%
                    Format.stext Format.%
                    ":" Format.% Format.stext Format.%
                    Format.stext
                   )
                   protoO
                   lUrl
                   lPort
                   endpoint
