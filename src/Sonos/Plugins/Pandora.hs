{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Sonos.Plugins.Pandora
( login
, getStationList
, searchStation
, createStation
, module Sonos.Plugins.Pandora.Types
) where

import Sonos.Plugins.Pandora.Types
import Sonos.Plugins.Pandora.Crypt

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC

import Network.Wreq
import Control.Monad
import Network.HTTP.Types.Status (status200)
import Control.Lens                         ((^?), (^?!), (.~), (&))
import qualified Data.Time.Clock.POSIX as POSIX

endpoint = "http://tuner.pandora.com/services/json/"
endpointSecure = "https://tuner.pandora.com/services/json/"

mkPandoraRequest (PandoraWorld {..}) a = do
    adjustedSyncTime <- adjustSyncTime pwSyncTime pwTimeSynced
    return $ PandoraRequest a adjustedSyncTime pwUserAuthToken

userAgent = "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/535.7 (KHTML, like Gecko) Chrome/16.0.912.63 Safari/535.7"

partnerLogin = do
    let plUserName = "android"
        plPassword = "AC7IBG09A3DTSYM4R41UJWL07VLN8JI7"
        plDeviceModel = "android-generic"
        plVersion = "5"
        plIncludeUrls = True
        pl = PartnerLogin {..}

        target = endpointSecure
        postBody = J.encode $ pl

        opts = defaults & header "Content-type" .~ ["text/plain; charset=utf8"]
                        & header "User-Agent" .~ [userAgent]
                        & param "method" .~ ["auth.partnerLogin"]
    resp <- postWith opts target postBody

    let Just (Right resp') = fmap J.eitherDecode $ resp ^? responseBody :: Maybe (Either String (PandoraReply PartnerLoginReply))

    let readSyncTime = plrSyncTime $ prResult $ resp'
        decodedSyncTime = decSyncTime readSyncTime

    unixSyncTime <- fmap floor POSIX.getPOSIXTime

    return (read $ BSC.unpack decodedSyncTime
           , plrPartnerId $ prResult $ resp'
           , plrPartnerAuthToken $ prResult $ resp'
           , unixSyncTime
           )

userLogin ulSyncTime partnerId ulPartnerAuthToken ulUserName ulPassword = do
    let ul = UserLogin {..}

        target = endpointSecure
        postBody = J.encode $ ul
        postBodyC = enc $ BSL.toStrict postBody

        opts = defaults & header "Content-type" .~ ["text/plain; charset=utf8"]
                        & header "User-Agent" .~ [userAgent]
                        & param "method" .~ ["auth.userLogin"]
                        & param "partner_id" .~ [partnerId]
                        & param "auth_token" .~ [ulPartnerAuthToken]
    resp <- postWith opts target postBodyC

    let Just (Right resp') = fmap J.eitherDecode $ resp ^? responseBody :: Maybe (Either String (PandoraReply UserLoginReply))

    return (ulrUserAuthToken $ prResult resp', ulrUserId $ prResult resp', not $ ulrHasAudioAds $ prResult resp')

login email password = do
    (pwSyncTime, pwPartnerId, pauth, pwTimeSynced) <- partnerLogin
    (pwUserAuthToken, pwUserId, pwHasSub) <- userLogin pwSyncTime pwPartnerId pauth email password
    return (PandoraWorld {..})


adjustSyncTime syncTime timeSynced = do
    now <- fmap floor POSIX.getPOSIXTime
    return $ syncTime + (now - timeSynced)


handle resp = do
    when (resp ^?! responseStatus /= status200) $ do
        putStrLn $ show (resp ^?! responseBody)

getStationList pw@(PandoraWorld {..}) = do
    let target = endpoint
        slIncludeStationArtUrl = True
    resp <- postIt' pw "user.getStationList" $ StationList {..}

    let Just (Right resp') = fmap J.eitherDecode $ resp ^? responseBody :: Maybe (Either String (PandoraReply StationListReply))

    return $ slrStations $ prResult $ resp'

searchStation pw@(PandoraWorld {..}) t = do
    let target = endpoint
        msSearchText = t
    resp <- postIt' pw "music.search" $ MusicSearch {..}

    let Just (Right resp') = fmap J.eitherDecode $ resp ^? responseBody :: Maybe (Either String (PandoraReply MusicSearchReply))

    return $ prResult $ resp'

createStation pw@(PandoraWorld {..}) t = do
    let target = endpoint
        csToken = t
    resp <- postIt' pw "station.createStation" $ CreateStation {..}

    let Just (Right resp') = fmap J.eitherDecode $ resp ^? responseBody :: Maybe (Either String (PandoraReply CreateStationReply))

    return $ prResult $ resp'

postIt' pw method js = do
    resp <- postIt pw method js
    handle resp
    return resp

postIt pw@(PandoraWorld {..}) method js = do
    preq <- mkPandoraRequest pw js
    let target = endpoint

        postBody = J.encode $ preq
        postBodyC = enc $ BSL.toStrict postBody

        opts = defaults & header "Content-type" .~ ["text/plain; charset=utf8"]
                        & header "User-Agent" .~ [userAgent]
                        & param "method" .~ [method]
                        & param "partner_id" .~ [pwPartnerId]
                        & param "auth_token" .~ [pwUserAuthToken]
                        & param "user_id" .~ [pwUserId]

    resp <- postWith opts target postBodyC

    return resp
