module Discover where
import Network.Socket
import Network.Multicast
import Control.Concurrent.Async
import Control.Concurrent.MVar

discover = withSocketsDo $ do
    (sock, addr) <- multicastSender "239.255.255.250" 1900
--    socks <- multicastReceiver "239.255.255.250" 1900
    let loop = do
           let msg = [ "M-SEARCH * HTTP/1.1"
                   , "HOST: 239.255.255.250:1900"
                   , "MAN: \"ssdp:discover\""
                   , "MX: 1"
                   , "ST: urn:schemas-upnp-org:device:ZonePlayer:1"
                   ]
               msg' = unlines msg
           sendTo sock msg' addr
           (msg, _, addr1) <- recvFrom sock 1024
           print  (msg, addr1)
           loop
    loop

