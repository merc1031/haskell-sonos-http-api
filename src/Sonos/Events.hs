{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Sonos.Events where

import Network.Info
import Network.Wreq
import Sonos.Types
import Control.Lens                         ((^?), (.~), (&))
import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Char (toLower)
import Data.List
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)

--getLocalAddr = ipv4 . getNetworkInterfaces

sub :: ZonePlayer
    -> String
    -> Int
    -> IO ()
sub zp me port = do
    print $ "Subscribing to " ++ show zp
    let loc = zpLocation zp
        addr = lUrl loc ++ ":" ++ lPort loc

    let opts =  defaults & header "CALLBACK" .~ [BSC.pack $ "<http://" ++ me ++ ":" ++ show port ++ "/eventSub>"]
                         & header "NT" .~  ["upnp:event"]
    print opts
    resp <- customMethodWith "SUBSCRIBE" opts ("http://" ++ addr ++ "/MediaRenderer/AVTransport/Event")
    print $ resp ^? responseBody
    print $ resp ^? responseStatus
    print $ resp ^? responseHeaders
    let Just headers = fmap M.fromList $ resp ^? responseHeaders
        Just timeout = M.lookup "TIMEOUT" headers
        Just sid = M.lookup "SID" headers
        timeoutSeconds :: Maybe Int
        timeoutSeconds = case map toLower $ BSC.unpack timeout of
                           (stripPrefix "second-" -> Just rest) -> Just $ read rest
                           "infinite" -> Nothing

    case timeoutSeconds of
      Just seconds -> do
          let timeoutReal = floor $ fromIntegral seconds * (0.85 :: Double)
          let resub = do
                _ <- async $ do
                    print "Start async renew"
                    let renew' t = do
                            print $ "Renewing at rate" ++ (show $ t * 1000000)
                            threadDelay $ t * 1000000
                            newT <- renew zp sid timeout
                            case newT of
                                Just sec -> renew' sec
                                Nothing -> return ()
                    renew' timeoutReal
                return ()
          resub
          return ()

      Nothing -> return ()
    return ()


renew :: ZonePlayer
    -> BS.ByteString
    -> BS.ByteString
    -> IO (Maybe Int)
renew zp sid timeoutH = do
    print $ "Renewing to " ++ show zp
    let loc = zpLocation zp
        addr = lUrl loc ++ ":" ++ lPort loc

    let opts =  defaults & header "SID" .~ [sid]
                         & header "TIMEOUT" .~  [timeoutH]
    print opts
    resp <- customMethodWith "SUBSCRIBE" opts ("http://" ++ addr ++ "/MediaRenderer/AVTransport/Event")
    print $ resp ^? responseBody
    print $ resp ^? responseStatus
    print $ resp ^? responseHeaders
    let Just headers = fmap M.fromList $ resp ^? responseHeaders
        Just timeout = M.lookup "TIMEOUT" headers
        timeoutSeconds :: Maybe Int
        timeoutSeconds = case map toLower $ BSC.unpack timeout of
                            (stripPrefix "second-" -> Just rest) -> Just $ read rest
                            "infinite" -> Nothing

    return timeoutSeconds
