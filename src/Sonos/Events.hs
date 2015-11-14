{-# LANGUAGE OverloadedStrings #-}
module Sonos.Events where

import Network.Info
import Network.Wreq
import Sonos.Types
import Control.Lens                         ((^?), (.~), (&))
import qualified Data.ByteString.Char8   as BSC

--getLocalAddr = ipv4 . getNetworkInterfaces

sub :: ZonePlayer
    -> String
    -> Int
    -> IO ()
sub zp me port = do
    let loc = zpLocation zp
        addr = lUrl loc ++ ":" ++ lPort loc

    let opts =  defaults & header "CALLBACK" .~ [BSC.pack $ "<http://" ++ me ++ ":" ++ show port ++ "/eventSub>"]
                         & header "NT" .~  ["upnp:event"]
    print opts
    resp <- customMethodWith "SUBSCRIBE" opts ("http://" ++ addr ++ "/MediaRenderer/AVTransport/Event")
    print $ resp ^? responseBody
    print $ resp ^? responseStatus
    return ()



