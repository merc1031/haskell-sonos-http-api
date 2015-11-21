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
