{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Sonos.Types where

import Data.Aeson ( ToJSON(..)
                  , object
                  , (.=)
                  )
import Control.Concurrent.STM

type State = TVar [ZonePlayer]

data CliArguments = CliArguments
    { dir :: !String
    }

data Location = Location
    { lProto :: String
    , lUrl :: String
    , lPort :: String
    , lEndpoint :: String
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
    , zpUUID :: String
    , zpName :: String
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
