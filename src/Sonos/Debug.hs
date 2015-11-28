module Sonos.Debug where

import System.IO                            ( hPutStrLn
                                            , stderr
                                            )


putStrLnErr = hPutStrLn stderr
