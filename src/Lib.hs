{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( someFunc
    , soap
    , serve
    ) where

import Control.Monad.IO.Class
import System.FilePath.Glob
import Network.Wreq

import Control.Lens ((^?), (.~), (&))
import Network.HTTP.Base
import Data.String.Utils
import Data.Monoid ((<>))
import Data.Char (toLower)

import Options.Applicative     ( Parser
                               , execParser
                               , argument
                               , info
                               , helper
                               , fullDesc
                               , help
                               , switch
                               , metavar
                               , str
                               , long
                               , short
                               , value
                               , strOption
                               )

--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Web.Spock as WS
import qualified Data.Map.Strict as M
import qualified Text.XML.Light.Extractors as E
import Text.XML
import Text.XML.Cursor
import qualified Data.Text as T

data CliArguments = CliArguments
    { dir :: !String
    }

parseArgs :: IO CliArguments
parseArgs = execParser $ info (helper <*> parseCliArgs) fullDesc

parseCliArgs :: Parser CliArguments
parseCliArgs =
    let directory = strOption
            ( long "directory"
            <> short 'd'
            <> help "Directory to search in"
            )
    in CliArguments <$> directory

someFunc :: CliArguments -> String -> IO [[String]]
someFunc args like = do
    putStrLn $ "What did we hear: " ++ like
    res <- globDirWith  (matchDefault { ignoreCase = True } ) [(compile $ "**/*" ++ like ++ "*.flac")] (dir args)
    putStrLn $ "Results were" ++ (show $ fst res)
    return $ fst res

soapTemplate' uri meta first next = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><s:Body><u:AddURIToQueue xmlns:u=\"urn:schemas-upnp-org:service:AVTransport:1\"><InstanceID>0</InstanceID><EnqueuedURI>" ++ uri ++ "</EnqueuedURI><EnqueuedURIMetaData>" ++ meta ++ "</EnqueuedURIMetaData><DesiredFirstTrackNumberEnqueued>" ++ show first ++ "</DesiredFirstTrackNumberEnqueued><EnqueueAsNext>" ++ show next ++ "</EnqueueAsNext></u:AddURIToQueue></s:Body><s:Envelope>"

playTemplate = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><s:Body><u:Play xmlns:u=\"urn:schemas-upnp-org:service:AVTransport:1\"><InstanceID>0</InstanceID><Speed>1</Speed></u:Play></s:Body></s:Envelope>"

setAVTransportURITemplate uri meta = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><s:Body><u:SetAVTransportURI xmlns:u=\"urn:schemas-upnp-org:service:AVTransport:1\"><InstanceID>0</InstanceID><CurrentURI>" ++ uri ++ "</CurrentURI><CurrentURIMetaData>" ++ meta ++ "</CurrentURIMetaData></u:SetAVTransportURI></s:Body></s:Envelope>"

seekTrackTemplate track = "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><s:Body><u:Seek xmlns:u=\"urn:schemas-upnp-org:service:AVTransport:1\"><InstanceID>0</InstanceID><Unit>TRACK_NR</Unit><Target>" ++ show track ++ "</Target></u:Seek></s:Body></s:Envelope>"

soapAction host action msg = do
    let opts = defaults & header "Host" .~ [BSC.pack host]
                        & header "User-Agent" .~ ["Haskell post"]
                        & header "Content-type" .~ ["text/xml; charset=\"UTF-8\""]
                        & header "Content-length" .~ [BSC.pack $ show $ length msg]
                        & header "SOAPAction" .~ [BSC.pack $ "urn:schemas-upnp-org:service:AVTransport:1#" ++ action]
        ep = "/MediaRenderer/AVTransport/Control"

    putStrLn $ show opts
    putStrLn $ show msg
    resp <- postWith opts ("http://" ++ host ++ ep) (BSC.pack msg)

    putStrLn $ show $ resp ^? responseBody
    putStrLn $ show $ resp ^? responseStatus
    return $ resp ^? responseBody

soap :: CliArguments -> String -> String -> IO ()
soap args room like = do
    putStrLn $ "Room was: " ++ room
    tracks <- someFunc args like
    let firstTrack = head $ head tracks
    putStrLn $ "First track is:" ++ firstTrack


    let rooms = M.fromList $ [
                                 ("kitchen", "192.168.1.161:1400")
                               , ("living room", "192.168.1.222:1400")
                               , ("patio", "192.168.1.148:1400")
                               , ("master bedroom", "192.168.1.236:1400")
                               , ("bathroom", "192.168.1.240:1400")
                               , ("media room", "192.168.1.210:1400")
                             ]
        Just room' = M.lookup (fmap toLower room) rooms
        firstTrackE = urlEncode $ replace ".flac" ".mp3" $ replace (dir args) "" $ firstTrack
        soapMessage = soapTemplate' ("x-file-cifs://asgard/mp3Music/" ++ firstTrackE) "" 0 0

    uuid <- fetchUUID room'

    let avMessage = setAVTransportURITemplate ("x-rincon-queue:" ++ (T.unpack uuid) ++ "#0") ""

    Just queuedBody <- soapAction room' "AddURIToQueue" soapMessage

    let trackNo = getTrackNum queuedBody
    soapAction room' "SetAVTransportURI" avMessage
    soapAction room' "Seek" (seekTrackTemplate trackNo)
    soapAction room' "Play" playTemplate


    return ()

--data UUID = UUID { _uuid :: String } deriving Show
--
--toUUID = E.extractDocContents uuid
--
--uuid = E.element "root" $ E.children $ do
--    _ <- E.element "specVersion" $ E.contents $ E.anyContent
--    E.element "device" $ E.children $ do
--        udn <- E.element "UDN" $ E.contents $ E.text
--        return $ UUID udn
fetchUUID host = do
    Just body <- getXMLDescription host
    return $ getUUID body

getXMLDescription host = do
    res <- get $ "http://" ++ host ++ "/xml/device_description.xml"
    return $ res ^? responseBody

getUUID body =
    let cursor = fromDocument $ parseLBS_ def body
    in T.drop 5 $ head $ cursor $/ element "{urn:schemas-upnp-org:device-1-0}device" &/ element "{urn:schemas-upnp-org:device-1-0}UDN" &// content

getTrackNum :: BSL.ByteString -> Int
getTrackNum body =
    let cursor = fromDocument $ parseLBS_ def body
    in read $ T.unpack $ head $ cursor $/ element "{http://schemas.xmlsoap.org/soap/envelope/}Body" &/ element "{urn:schemas-upnp-org:service:AVTransport:1}AddURIToQueueResponse" &/ element "FirstTrackNumberEnqueued" &// content

serve = do
    args <- parseArgs
    WS.runSpock 5006 $ WS.spock (WS.defaultSpockCfg Nothing WS.PCNoDatabase Nothing) (routes args)

playLikeR :: WS.Path '[String, String]
playLikeR = "like" WS.<//> WS.var WS.<//> WS.var

routes args = do
    WS.get playLikeR $ \room like -> do
        liftIO $ soap args room like
        return ()
