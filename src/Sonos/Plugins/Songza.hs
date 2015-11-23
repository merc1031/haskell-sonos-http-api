{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Sonos.Plugins.Songza
( mkMetaData
, mkUri
, getStationList
, getSituationList
, StationListResponse (..)
, SituationListResponse (..)
) where

import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as H

import Network.HTTP.Base (urlEncode)
import Network.Wreq
import Control.Monad
import Network.HTTP.Types.Status (status200)
import Data.Char (toLower)
import Control.Lens                         ((^?), (^?!), (.~), (&))
import qualified Crypto.Cipher as C
import qualified Crypto.Padding as CP
import Crypto.Types (ByteLength)
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Formatting as Format
import Formatting (stext, (%), sformat, int)

import qualified HTMLEntities.Builder as HTML
import qualified HTMLEntities.Decoder as HTML
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB

endpoint = "http://songza.com/api/1"
-- | Interestingly i need to encode the : in station and the & in flags but not urlEncode...
stationFmt = sformat $ "station%3a" % int % "?sid=" % stext % "&amp;flags=" % stext % "&amp;sn=" % stext
enqUriFmt = sformat $ "x-sonosapi-radio:" % stext

mkUri :: Int
      -> T.Text
mkUri id = enqUriFmt $ stationFmt id "29" "829" "8" -- These magic numbers... not sure why they are

didlWrapper c =
    let dc,upnp,r,ns :: T.Text
        dc = "xmlns:dc=\"http://purl.org/dc/elements/1.1/\""
        upnp = "xmlns:upnp=\"urn:schemas-upnp-org:metadata-1-0/upnp/\""
        r = "xmlns:r=\"urn:schemas-rinconnetworks-com:metadata-1-0/\""
        ns = "xmlns=\"urn:schemas-upnp-org:metadata-1-0/DIDL-Lite/\""
        stext' l r = l % stext % r
    in TL.toStrict
     $ TLB.toLazyText
     $ HTML.text
     $ sformat ("<DIDL-Lite "
                    `stext'` " "
                    `stext'` " "
                    `stext'` " "
                    `stext'`
               ">"
                    `stext'`
               "</DIDL-Lite>"
               )
               dc
               upnp
               r
               ns
               c

didlTemplate :: T.Text
             -> T.Text
             -> T.Text
             -> T.Text
             -> T.Text
didlTemplate stId sitId stName uid =
    let stext' l r = l % stext % r
    in didlWrapper
     $ sformat
     ("<item id=\"000c2068" `stext'` "\" parentID=\"0082064" `stext'` "\" restricted=\"true\">\
     \<dc:title>" `stext'` "</dc:title>\
     \<upnp:class>object.item.audioItem.audioBroadcast</upnp:class>\
     \<desc id=\"cdudn\" nameSpace=\"urn:schemas-rinconnetworks-com:metadata-1-0\">\
     \SA_RINCON7431_" `stext'` "\
     \</desc>\
     \</item>"
     )
     stId
     sitId
     stName
     uid

mkMetaData stId sitId stName uid = didlTemplate (T.concat ["station%3a",T.pack $ show stId]) (T.concat ["situation%3a", sitId]) stName uid

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


getStationList search = do
    let target = endpoint
    resp <- getIt "/search/station" [("query", search)]

    let Just (Right resp') = fmap J.eitherDecode $ resp ^? responseBody :: Maybe (Either String [StationListResponse])

    return $ resp'

getSituationList search = do
    let target = endpoint
    resp <- getIt "/search/situation" [("query", search)]
    print resp
    let Just (Right resp') = fmap J.eitherDecode $ resp ^? responseBody :: Maybe (Either String [SituationListResponse])

    return $ resp'

getIt method params = do
    let target = T.concat [endpoint, method]

        opts = defaults
        opts' = foldl (\o (l,r) -> o & param l .~ [r]) opts params


    resp <- getWith opts' (T.unpack target)

    return resp
