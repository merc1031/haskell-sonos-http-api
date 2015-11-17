{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Sonos.Serve where

import Control.Monad.IO.Class
import Sonos.Lib
import Sonos.Types
import Sonos.Discover (getTopology)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.Async (async)
import qualified Web.Spock as WS
import Network.Wai.Middleware.RequestLogger
import Data.Default
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T


serve st' args = do
    WS.runSpock 5006 $ WS.spock (WS.defaultSpockCfg Nothing WS.PCNoDatabase st') (routes args)




playLikeR :: WS.Path '[String, String]
playLikeR = "like" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var

enqueueLikeR :: WS.Path '[String, String]
enqueueLikeR = "like" WS.<//> "enqueue" WS.<//> WS.var WS.<//> WS.var

playLikeArtistR :: WS.Path '[String, String]
playLikeArtistR = "likeArtist" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var

enqueueLikeArtistR :: WS.Path '[String, String]
enqueueLikeArtistR = "likeArtist" WS.<//> "enqueue" WS.<//> WS.var WS.<//> WS.var

playPandoraRadioLikeR :: WS.Path '[String, String]
playPandoraRadioLikeR = "pandora" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var

browseContentDirectoryR :: WS.Path '[T.Text, T.Text, Int, Int, T.Text]
browseContentDirectoryR = "browse" WS.<//> WS.var WS.<//> WS.var WS.<//> WS.var WS.<//> WS.var WS.<//> WS.var

listR :: WS.Path '[]
listR = "list"

joinR :: WS.Path '[String, String]
joinR = "join" WS.<//> WS.var WS.<//> WS.var

unjoinR :: WS.Path '[String]
unjoinR = "unjoin" WS.<//> WS.var

eventSubR :: WS.Path '[]
eventSubR = "eventSub"

eventSub1R :: WS.Path '[String]
eventSub1R = "eventSub" WS.<//> WS.var

accessState :: WS.ActionCtxT () (WS.WebStateM () (Maybe a) (TVar [ZonePlayer])) [ZonePlayer]
accessState = do
    st <- WS.getState
    v <- liftIO $ atomically $ readTVar st
    return $ v

routes args = do
    st <- WS.getState

    reqLogger <- liftIO $ mkRequestLogger def
    WS.middleware $ reqLogger
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
    WS.get playLikeArtistR $ \room like -> do
        rst <- accessState
        let room' = getRoom rst room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            queueAndPlayArtistLike rst args room' like
        return ()
    WS.get enqueueLikeArtistR $ \room like -> do
        rst <- accessState
        let room' = getRoom rst room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            queueArtistLike rst args room' like
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
    WS.hookRouteCustom "NOTIFY" eventSub1R $ \x -> do
        liftIO $ print "In event sub 1"
        b <- WS.body
        liftIO $ print b
        liftIO $ print x
        return () --WS.raw $ print data
    WS.hookRouteCustom "NOTIFY" eventSubR $ do
        liftIO $ print "In event sub"
        b <- WS.body
        liftIO $ print $ xmlEvent $ BSL.fromStrict b
        return () --WS.raw $ print data
    WS.get playPandoraRadioLikeR $ \room like -> do
        rst <- accessState
        let room' = getRoom rst room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            playPandoraStationLike rst args room' like
        return ()
    WS.get browseContentDirectoryR $ \cat filt s c so -> do
        rst <- accessState
        liftIO $ do
            browseContentDirectory rst args cat filt s c so
        return ()
    WS.get listR $ do
        rst <- accessState
        WS.json rst
