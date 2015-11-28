{-# LANGUAGE OverloadedStrings #-}
module Sonos.XML where

import Text.XML
import Text.XML.Cursor
import Sonos.Types

import Data.Maybe                           ( fromJust)
import Data.String                          ( fromString
                                            , IsString
                                            )
import Formatting                           ( stext
                                            , (%)
                                            , sformat
                                            )

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE

data BrowseContainer = BrowseDefault T.Text
                     | BrowseSpecified T.Text


lookupWrapper :: BrowseContainer
              -> T.Text
lookupWrapper (BrowseSpecified c) = c
lookupWrapper (BrowseDefault k) = fromJust
                $ M.lookup k
                $ M.fromList [ ("A:ARTIST", "container")
                             , ("A:ALBUM", "container")
                             , ("A:TRACKS", "item")
                             , ("0", "container")
                             , ("FV:2", "item")
                             , ("FV:3", "item")
                             ]

browsedContent :: BrowseContainer
               -> BSL.ByteString
               -> (Int, Int, [(T.Text, (T.Text, T.Text))])
browsedContent typeKey body =
    let cursor = fromDocument $ parseLBS_ def body
        wrapper = lookupWrapper typeKey
        [root] = cursor $/ laxElement "Body"
        [resultO] = root $/ laxElement "BrowseResponse"
        [numReturned] = resultO $/ laxElement "NumberReturned"
                           &// content
        [totalMatches] = resultO $/ laxElement "TotalMatches"
                           &// content

        [result] = resultO $/ laxElement "Result"
        result' = (T.concat $ result $/ content)

        resultCursor = fromDocument $ parseLBS_ def $ BSL.fromStrict $ TE.encodeUtf8 result'

        things = resultCursor $/ laxElement wrapper

        res = map transform things
        transform elem =
            let title = case elem $/ laxElement "title" &// content of
                    [] -> ""
                    t -> head t
                link = case elem $/ laxElement "res" &// content of
                    [] -> ""
                    l -> head l
                resMD = case elem $/ laxElement "resMD" &// content of
                    [] -> ""
                    m -> head m
            in (title, (link, resMD))
    in (read $ T.unpack numReturned, read $ T.unpack totalMatches, res)


getUUID :: BSL.ByteString
        -> T.Text
getUUID body =
    let cursor = fromDocument $ parseLBS_ def body
        ns = "{urn:schemas-upnp-org:device-1-0}"
    in T.drop 5 $ head $ cursor $/ element (fromString $ ns ++ "device")
                                &/ element (fromString $ ns ++ "UDN")
                                &// content

getQueueData :: BSL.ByteString
             -> SonosQueueData
getQueueData body = 
    let cursor = fromDocument $ parseLBS_ def body
        [elem] = cursor $/ laxElement "Body"
                      &/ laxElement "AddURIToQueueResponse"
        tread = read . T.unpack
        queueLength = case elem $/ element "NewQueueLength" &// content of
                        [] -> 0
                        [ql] -> tread ql
        numTracksAdded = case elem $/ element "NumTracksAdded" &// content of
                           [] -> 0
                           [nta] -> tread nta
        firstTrackNumberEnq = case elem $/ element "FirstTrackNumberEnqueued" &// content of
                           [] -> 0
                           [ftn] -> tread ftn

    in SonosQueueData {
          sqdNewQueueLength = queueLength
        , sqdNumTracksAdded = numTracksAdded
        , sqdFirstTrackNumberEnqueued = firstTrackNumberEnq
        , sqdFirstTrackOfNewQueue = (queueLength - numTracksAdded) + 1
        }



getTrackNum :: BSL.ByteString
            -> Int
getTrackNum body =
    let cursor = fromDocument $ parseLBS_ def body
        [elem] = cursor $/ laxElement "Body"
                      &/ laxElement "AddURIToQueueResponse"
        tread = read . T.unpack
        queueLength = case elem $/ element "NewQueueLength" &// content of
                        [] -> 0
                        [ql] -> tread ql
        numTracksAdded = case elem $/ element "NumTracksAdded" &// content of
                           [] -> 0
                           [nta] -> tread nta

    in (queueLength - numTracksAdded) + 1


