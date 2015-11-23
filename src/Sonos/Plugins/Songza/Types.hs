{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Sonos.Plugins.Songza.Types where

import qualified Data.Text as T
import qualified Data.Aeson as J

data StationListResponse = StationListResponse
    { slrDasherizedName :: T.Text
    , slrStatus :: T.Text
    , slrGlobalStation :: Maybe T.Text
    , slrName :: T.Text
    , slrCreatorName :: T.Text
    , slrUrl :: T.Text
    , slrSongCount :: Int
    , slrCoverUrl :: T.Text
    , slrFeaturedArtists :: [Artist]
    , slrCreatorId :: Int
    , slrType :: T.Text
    , slrId :: Int
    , slrDescription :: T.Text
    } deriving Show

newtype Artist = Artist T.Text
    deriving Show

instance J.FromJSON Artist where
    parseJSON = J.withObject "FromJSON Artist" $ \o ->
        Artist <$> o J..: "name"

instance J.FromJSON StationListResponse where
    parseJSON = J.withObject "FromJSON StationListResponse" $ \o ->
        StationListResponse <$> o J..: "dasherized_name"
                            <*> o J..: "status"
                            <*> o J..:? "global_station"
                            <*> o J..: "name"
                            <*> o J..: "creator_name"
                            <*> o J..: "url"
                            <*> o J..: "song_count"
                            <*> o J..: "cover_url"
                            <*> o J..: "featured_artists"
                            <*> o J..: "creator_id"
                            <*> o J..: "type"
                            <*> o J..: "id"
                            <*> o J..: "description"

data SituationListResponse = SituationListResponse
    { silrTitle :: T.Text
    , silrSituations :: [T.Text]
    , silrSelectedMessage :: T.Text
    , silrStationIds :: Maybe [Int]
    , silrId :: T.Text
    , silrIcon :: T.Text
    } deriving Show

instance J.FromJSON SituationListResponse where
    parseJSON = J.withObject "FromJSON SituationListResponse" $ \o ->
        SituationListResponse <$> o J..: "title"
                              <*> o J..: "situations"
                              <*> o J..: "selected_message"
                              <*> o J..:? "station_ids"
                              <*> o J..: "id"
                              <*> o J..: "icon"

