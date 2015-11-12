{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Sonos.Serve where

import Control.Monad.IO.Class
import Sonos.Lib
import Sonos.Types
import Sonos.Discover (getTopology)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.Async (async)
import qualified Web.Spock as WS


serve st' args = do
    WS.runSpock 5006 $ WS.spock (WS.defaultSpockCfg Nothing WS.PCNoDatabase st') (routes args)



stateStuff tv = do
    let loop = do
            d <- getTopology
            threadDelay 10000000
            atomically $ swapTVar tv d
            loop
    async $ forever $ loop

playLikeR :: WS.Path '[String, String]
playLikeR = "like" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var

enqueueLikeR :: WS.Path '[String, String]
enqueueLikeR = "like" WS.<//> "enqueue" WS.<//> WS.var WS.<//> WS.var

listR :: WS.Path '[]
listR = "list"

joinR :: WS.Path '[String, String]
joinR = "join" WS.<//> WS.var WS.<//> WS.var

unjoinR :: WS.Path '[String]
unjoinR = "unjoin" WS.<//> WS.var

accessState :: WS.ActionCtxT () (WS.WebStateM () (Maybe a) (TVar [ZonePlayer])) [ZonePlayer]
accessState = do
    st <- WS.getState
    v <- liftIO $ atomically $ readTVar st
    return $ v

routes args = do
    st <- WS.getState
    liftIO $ stateStuff st
    WS.get playLikeR $ \room like -> do
        rst <- accessState
        let room' = getRoom rst room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            queueAndPlayTrackLike rst args room' like
        return ()
    WS.get enqueueLikeR $ \room like -> do
        rst <- accessState
        let room' = getRoom rst room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            queueTrackLike rst args room' like
        return ()
    WS.get joinR $ \a b -> do
        rst <- accessState
        let roomA = getRoom rst a
        let roomB = getRoom rst b
        liftIO $ do
            putStrLn $ "RoomA was: " ++ a
            putStrLn $ "RoomB was: " ++ b
            groupRoom rst args roomA roomB
        return ()
    WS.get unjoinR $ \room -> do
        rst <- accessState
        let room' = getRoom rst room
        liftIO $ do
            putStrLn $ "RoomA was: " ++ room
            ungroupRoom rst args room'
        return ()
    WS.get listR $ do
        rst <- accessState
        WS.json rst
