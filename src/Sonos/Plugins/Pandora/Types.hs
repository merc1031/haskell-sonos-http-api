{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Sonos.Plugins.Pandora.Types where

import qualified Data.Aeson             as J
import qualified Data.Text              as T
import qualified Data.Aeson.Types       as JT
import qualified Data.Map.Strict        as M
import qualified Data.HashMap.Strict    as H

type UserAuthToken = T.Text
type SyncTime = Int

data UserLogin = UserLogin
    { ulUserName :: T.Text
    , ulPassword :: T.Text
    , ulPartnerAuthToken :: T.Text
    , ulSyncTime :: SyncTime
    } deriving (Show)

data UserLoginReply = UserLoginReply
    { ulrCanListen :: Bool
    , ulrHasAudioAds :: Bool
    , ulrListeningTimeoutAlertMsgUri :: T.Text
    , ulrListeningTimeoutMinutes :: T.Text
    , ulrMaxStationsAllowed :: Int
    , ulrMinimumAdRefreshInterval :: Int
    , ulrSplashScreenAdUrl :: T.Text
    , ulrStationCreationAdUrl :: T.Text
    , ulrSubscriptionHasExpired :: Bool
    , ulrUserAuthToken :: UserAuthToken
    , ulrUserId :: T.Text
    , ulrUserProfileUrl :: T.Text
    , ulrUsername :: T.Text
    , ulrVideoAdUrl :: T.Text
    , ulrZeroVolumeAutoPauseEnabledFlag :: Bool
    , ulrZeroVolumeNumMutedTracks :: Int
    } deriving (Show)

instance J.FromJSON UserLoginReply where
    parseJSON = J.withObject "FromJSON UserLoginReply" $ \o ->
        UserLoginReply <$> o J..: "canListen"
                       <*> o J..: "hasAudioAds"
                       <*> o J..: "listeningTimeoutAlertMsgUri"
                       <*> o J..: "listeningTimeoutMinutes"
                       <*> o J..: "maxStationsAllowed"
                       <*> o J..: "minimumAdRefreshInterval"
                       <*> o J..: "splashScreenAdUrl"
                       <*> o J..: "stationCreationAdUrl"
                       <*> o J..: "subscriptionHasExpired"
                       <*> o J..: "userAuthToken"
                       <*> o J..: "userId"
                       <*> o J..: "userProfileUrl"
                       <*> o J..: "username"
                       <*> o J..: "videoAdUrl"
                       <*> o J..: "zeroVolumeAutoPauseEnabledFlag"
                       <*> o J..: "zeroVolumeNumMutedTracks"


instance J.ToJSON UserLogin where
    toJSON UserLogin {..} = J.object [
              "username" J..= ulUserName
            , "password" J..= ulPassword
            , "partnerAuthToken" J..= ulPartnerAuthToken
            , "syncTime" J..= ulSyncTime
            , "loginType" J..= ("user" :: T.Text)
            , "includePandoraOneInfo" J..= True
            , "includeAdAttributes" J..= True
            , "includeSubscriptionExpiration" J..= True
        ]

data PartnerLogin = PartnerLogin
    { plUserName :: T.Text
    , plPassword :: T.Text
    , plDeviceModel :: T.Text
    , plVersion :: T.Text
    , plIncludeUrls :: Bool
    } deriving (Show)

data Ooyala = Ooyala
    { oStreamingPercentage :: Int
    , oStreamingWhiteList :: [Int]
    , oVideoAdBufferRetryCount :: Int
    , oVideoAdLoadingTimeout :: Int
    , oVideoAdPlayTimeout :: Int
    } deriving (Show)


data PandoraDeviceProperties = PandoraDeviceProperties
    { pdpAdRefreshInterval :: Int
    , pdpOoyala :: Ooyala
    , pdpVideoAdRefreshInterval :: Int
    , pdpVideoAdStartInterval :: Int
    , pdpVideoAdUniqueInterval :: Int
    } deriving (Show)

data PartnerLoginReply = PartnerLoginReply
    { plrSyncTime :: T.Text
    , plrDeviceProperties :: PandoraDeviceProperties
    , plrPartnerAuthToken :: T.Text
    , plrPartnerId  :: T.Text
    , plrStationSkipLimit :: Int
    , plrStationSkipUnit :: T.Text -- make a type
    , plrUrls :: M.Map T.Text T.Text
    } deriving (Show)

data PandoraReply a = PandoraReply
    { prResult :: a
    , prStat :: T.Text
    } deriving (Show)

data PandoraRequest a = PandoraRequest
    { preqBody :: a
    , preqSyncTime :: SyncTime
    , preqUserAuthToken :: UserAuthToken
    } deriving (Show)

instance J.ToJSON a => J.ToJSON (PandoraRequest a) where
    toJSON (PandoraRequest {..}) = J.object $ [
              "syncTime" J..= preqSyncTime
            , "userAuthToken" J..= preqUserAuthToken
        ] ++ rest
        where
            rest = toAesonList preqBody

toAesonList :: J.ToJSON a
            => a
            -> [JT.Pair]
toAesonList v = case aesonV of
    J.Object v' ->
        H.toList v'
    _ -> []
    where
        aesonV = J.toJSON v




instance J.FromJSON a => J.FromJSON (PandoraReply a) where
    parseJSON = J.withObject "FromJSON PandoraReply" $ \o ->
        PandoraReply <$> o J..: "result"
                     <*> o J..: "stat"

instance J.FromJSON Ooyala where
    parseJSON = J.withObject "FromJSON Ooyala" $ \o ->
        Ooyala <$> o J..: "streamingPercentage"
               <*> o J..: "streamingWhitelist"
               <*> o J..: "videoAdBufferRetryCount"
               <*> o J..: "videoAdLoadingTimeout"
               <*> o J..: "videoAdPlayTimeout"

instance J.FromJSON PandoraDeviceProperties where
    parseJSON = J.withObject "FromJSON PandoraDeviceProperties" $ \o ->
        PandoraDeviceProperties <$> o J..: "adRefreshInterval"
                                <*> o J..: "ooyala"
                                <*> o J..: "videoAdRefreshInterval"
                                <*> o J..: "videoAdStartInterval"
                                <*> o J..: "videoAdUniqueInterval"

instance J.FromJSON PartnerLoginReply where
    parseJSON = J.withObject "FromJSON PartnerLoginReply" $ \o ->
        PartnerLoginReply <$> o J..: "syncTime"
                          <*> o J..: "deviceProperties"
                          <*> o J..: "partnerAuthToken"
                          <*> o J..: "partnerId"
                          <*> o J..: "stationSkipLimit"
                          <*> o J..: "stationSkipUnit"
                          <*> o J..: "urls"

instance J.ToJSON PartnerLogin where
    toJSON PartnerLogin {..} = J.object [
              "username" J..= plUserName
            , "password" J..= plPassword
            , "deviceModel" J..= plDeviceModel
            , "version" J..= plVersion
            , "includeUrls" J..= plIncludeUrls
        ]

data PandoraWorld = PandoraWorld
    { pwSyncTime :: SyncTime
    , pwTimeSynced :: Int
    , pwUserAuthToken :: UserAuthToken
    , pwPartnerId :: T.Text
    , pwUserId :: T.Text
    , pwHasSub :: Bool
    }

data MusicSearch = MusicSearch
    { msSearchText :: T.Text
    } deriving Show

instance J.ToJSON MusicSearch where
    toJSON (MusicSearch {..}) = J.object [
              "searchText" J..= msSearchText
        ]

data MusicSearchReply = MusicSearchReply
    { msrNearMatchesAvailable :: Bool
    , msrExplanation :: T.Text
    , msrSongs :: [Song]
    , msrArtists :: [Artist]
    } deriving Show

type ArtistName = T.Text
type MusicToken = T.Text

data Song = Song
    { songArtistName :: ArtistName
    , songMusicToken :: MusicToken
    , songSongName :: T.Text
    , songScore :: Int
    } deriving Show

data Artist = Artist
    { artistArtistName :: ArtistName
    , artistMusicToken :: MusicToken
    , artistLikelyMatch :: Bool
    , artistScore :: Int
    } deriving Show

instance J.FromJSON MusicSearchReply where
    parseJSON = J.withObject "FromJSON MusicSearchReply" $ \o ->
        MusicSearchReply <$> o J..: "nearMatchesAvailable"
                         <*> o J..: "explanation"
                         <*> o J..: "songs"
                         <*> o J..: "artists"

instance J.FromJSON Song where
    parseJSON = J.withObject "FromJSON Song" $ \o ->
        Song <$> o J..: "artistName"
             <*> o J..: "musicToken"
             <*> o J..: "songName"
             <*> o J..: "score"

instance J.FromJSON Artist where
    parseJSON = J.withObject "FromJSON Artist" $ \o ->
        Artist <$> o J..: "artistName"
               <*> o J..: "musicToken"
               <*> o J..: "likelyMatch"
               <*> o J..: "score"

data CreateStation = CreateStation
    { csToken :: MusicToken
    } deriving Show

instance J.ToJSON CreateStation where
    toJSON (CreateStation {..}) = J.object [
              "musicToken" J..= csToken
        ]

data CreateStationReply = CreateStationReply
    { csrStation :: Station
    } deriving Show

instance J.FromJSON CreateStationReply where
    parseJSON = J.withObject "FromJSON CreateStationReply" $ \o ->
        CreateStationReply <$> (J.parseJSON $ JT.Object o)

data StationList = StationList
    { slIncludeStationArtUrl :: Bool
    } deriving Show

data StationListReply = StationListReply
    { slrChecksum :: T.Text
    , slrStations :: [Station]
    } deriving Show

data Station = Station
    { sAllowAddMusic :: Bool
    , sAllowDelete :: Bool
    , sAllowEditDescription :: Bool
    , sAllowRename :: Bool
    , sArtUrl :: Maybe T.Text
    , sDateCreated :: PandoraDate
    , sGenre :: Maybe [T.Text]
    , sIsQuickMix :: Bool
    , sIsShared :: Bool
    , sQuickMixStationIds :: Maybe [T.Text]
    , sRequiresCleanAds :: Bool
    , sStationDetailUrl :: T.Text
    , sStationId :: T.Text
    , sStationName :: T.Text
    , sStationSharingUrl :: T.Text
    , sStationToken :: T.Text
    , sSuppressVideoAds :: Bool
    } deriving Show

data PandoraDate = PandoraDate
    { pdDate :: Int
    , pdDay :: Int
    , pdHours :: Int
    , pdMinutes :: Int
    , pdMonth :: Int
    , pdNanos :: Integer
    , pdSeconds :: Int
    , pdTime :: Integer
    , pdTimezoneOffset :: Int
    , pdYear :: Int
    } deriving Show

instance J.FromJSON StationListReply where
    parseJSON = J.withObject "FromJSON StationListReply" $ \o ->
        StationListReply <$> o J..: "checksum"
                         <*> o J..: "stations"

instance J.FromJSON Station where
    parseJSON = J.withObject "FromJSON Station" $ \o ->
        Station <$> o J..: "allowAddMusic"
                <*> o J..: "allowDelete"
                <*> o J..: "allowEditDescription"
                <*> o J..: "allowRename"
                <*> o J..:? "artUrl"
                <*> o J..: "dateCreated"
                <*> o J..:? "genre"
                <*> o J..: "isQuickMix"
                <*> o J..: "isShared"
                <*> o J..:? "quickMixStationIds"
                <*> o J..: "requiresCleanAds"
                <*> o J..: "stationDetailUrl"
                <*> o J..: "stationId"
                <*> o J..: "stationName"
                <*> o J..: "stationSharingUrl"
                <*> o J..: "stationToken"
                <*> o J..: "suppressVideoAds"

instance J.FromJSON PandoraDate where
    parseJSON = J.withObject "FromJSON PandoraDate" $ \o ->
        PandoraDate <$> o J..: "date"
                    <*> o J..: "day"
                    <*> o J..: "hours"
                    <*> o J..: "minutes"
                    <*> o J..: "month"
                    <*> o J..: "nanos"
                    <*> o J..: "seconds"
                    <*> o J..: "time"
                    <*> o J..: "timezoneOffset"
                    <*> o J..: "year"


instance J.ToJSON StationList where
    toJSON (StationList {..}) = J.object [
              "includeStationArtUrl" J..= slIncludeStationArtUrl
        ]
