{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Sonos.Types where

import Data.Aeson ( ToJSON(..)
                  , object
                  , (.=)
                  )
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Formatting as Format

type State = TVar [ZonePlayer]

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
