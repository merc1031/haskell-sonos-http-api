{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sonos.Types where

import Data.Aeson ( ToJSON(..)
                  , object
                  , (.=)
                  )
import Control.Concurrent.STM
import Web.PathPieces
import qualified Data.Text as T
import qualified Formatting as Format
import qualified Data.Map.Strict as M

newtype Room = Room { unRoom :: T.Text } deriving (PathPiece)



data MusicDB = MusicDB
    { artists :: TVar (M.Map T.Text T.Text)
    , albums :: TVar (M.Map T.Text T.Text)
    , tracks :: TVar (M.Map T.Text T.Text)
    }

data State = State
    { zps :: TVar [ZonePlayer]
    , mdb :: MusicDB
    }

data CliArguments = CliArguments
    { dir :: !String
    , email :: !T.Text
    , password :: !T.Text
    }

data Location = Location
    { lProto :: T.Text
    , lUrl :: T.Text
    , lPort :: T.Text
    , lEndpoint :: T.Text
    } deriving Show

data SonosDiscovery = SonosDiscovery
    { sdLocation :: Location
    , sdServer :: String
    , sdST :: String
    , sdUSN :: String
    , sdHousehold :: String
    , sdBootSeq :: String
    , sdWifiMode :: String
    } deriving Show

data ZonePlayer = ZonePlayer 
    { zpGroup :: String
    , zpCoordinator :: Bool
    , zpLocation :: Location
    , zpBootSeq :: String
    , zpUUID :: T.Text
    , zpName :: T.Text
    } deriving (Show)

data Change =
    Change { cMasterVolume :: Int
           , cLFVolume :: Int
           , cRFVolume :: Int
           , cMasterMute :: Int
           , cLFMute :: Int
           , cRFMute :: Int
           , cBass :: Int
           , cTreble :: Int
           , cMasterLoudness :: Int
           , cOutputFixed :: Int
           , cHeadphonesConnected :: Int
           , cSpeakerSize :: Int
           , cSubGain :: Int
           , cSubCrossover :: Int
           , cSubPolarity :: Int
           , cSubEnabled :: Int
           , cSonarEnabled :: Int
           , cSonarCalibrationAvailable :: Int
           , cPresetNameList :: T.Text
           } deriving Show

data Transport =
    Transport { tTransportState :: T.Text
              , tCurrentPlayMode :: T.Text
              , tCurrentCrossfadeMode :: Int
              , tNumberOfTracks :: Int
              , tCurrentTrack :: Int
              , tCurrentSection :: Int
              , tCurrentTrackURI :: T.Text
              , tCurrentTrackDuration :: T.Text
              , tCurrentTrackMetaData :: T.Text
              , tNextTrackURI :: T.Text
              , tNextTrackMetaData :: T.Text
              , tEnqueuedTransportURI :: T.Text
              , tEnqueuedTransportMetaData :: T.Text
              , tPlaybackStorageMedium :: T.Text
              , tAVTransportURI :: T.Text
              , tAVTransportURIMetaData :: T.Text
              , tNextAVTransportURI :: T.Text
              , tNextAVTransportURIMetaData :: T.Text
              , tCurrentTransportActions :: T.Text
              , tCurrentValidPlayModes :: T.Text
              , tMuseSessions :: T.Text
              , tTransportStatus :: T.Text
              , tSleepTimerGeneration :: Int
              , tAlarmRunning :: Int
              , tSnoozeRunning :: Int
              , tRestartPending :: Int
              , tTransportPlaySpeed :: T.Text
              , tCurrentMediaDuration :: T.Text
              , tRecordStorageMedium :: T.Text
              , tPossiblePlaybackStorageMedia :: T.Text
              , tPossibleRecordStorageMedia :: T.Text
              , tRecordMediumWriteStatus :: T.Text
              , tCurrentRecordQualityMode :: T.Text
              , tPossibleRecordQualityModes :: T.Text
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
