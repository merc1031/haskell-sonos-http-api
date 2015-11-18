{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
import qualified Data.Map.Strict as M


serve state args = do
    WS.runSpock 5006 $ WS.spock (WS.defaultSpockCfg Nothing WS.PCNoDatabase state) (routes args)




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

inspectArtistsR :: WS.Path '[Int, Int]
inspectArtistsR = "inspect" WS.<//> "artists" WS.<//> WS.var WS.<//> WS.var

inspectAlbumsR :: WS.Path '[Int, Int]
inspectAlbumsR = "inspect" WS.<//> "albums" WS.<//> WS.var WS.<//> WS.var

inspectTracksR :: WS.Path '[Int, Int]
inspectTracksR = "inspect" WS.<//> "tracks" WS.<//> WS.var WS.<//> WS.var

inspectArtistsLikeR :: WS.Path '[T.Text]
inspectArtistsLikeR = "inspect" WS.<//> "artists" WS.<//> "like" WS.<//> WS.var

inspectAlbumsLikeR :: WS.Path '[T.Text]
inspectAlbumsLikeR = "inspect" WS.<//> "albums" WS.<//> "like" WS.<//> WS.var

inspectTracksLikeR :: WS.Path '[T.Text]
inspectTracksLikeR = "inspect" WS.<//> "tracks" WS.<//> "like" WS.<//> WS.var

getAtomicState :: ( MonadIO m, WS.HasSpock (WS.ActionCtxT ctx m)
                  , WS.SpockState (WS.ActionCtxT ctx m) ~ State)
               => (State -> TVar a)
               -> WS.ActionCtxT ctx m a
getAtomicState access = do
    state <- WS.getState
    a <- liftIO $ atomically $ readTVar $ access state
    return a

routes :: ( MonadIO m, WS.HasSpock (WS.SpockCtxT ctx m)
          , WS.HasSpock (WS.ActionCtxT ctx m)
          , WS.SpockState (WS.SpockCtxT ctx m) ~ State
          , WS.SpockState (WS.ActionCtxT ctx m) ~ State)
       => CliArguments
       -> WS.SpockCtxT ctx m ()
routes args = do
    state <- WS.getState
    zps' <- liftIO $ atomically $ readTVar $ zps state

    reqLogger <- liftIO $ mkRequestLogger def
    WS.middleware $ reqLogger
    WS.get playLikeR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            queueAndPlayTrackLike state args room' like
        return ()
    WS.get enqueueLikeR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            queueTrackLike state args room' like
        return ()
    WS.get playLikeArtistR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            queueAndPlayArtistLike state args room' like
        return ()
    WS.get enqueueLikeArtistR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            queueArtistLike state args room' like
        return ()
    WS.get joinR $ \a b -> do
        let roomA = getRoom zps' a
        let roomB = getRoom zps' b
        liftIO $ do
            putStrLn $ "RoomA was: " ++ a
            putStrLn $ "RoomB was: " ++ b
            groupRoom state args roomA roomB
        return ()
    WS.get unjoinR $ \room -> do
        let room' = getRoom zps' room
        liftIO $ do
            putStrLn $ "RoomA was: " ++ room
            ungroupRoom state args room'
        return ()
    WS.hookRouteCustom "NOTIFY" eventSubR $ do
        liftIO $ print ("In event sub" :: String)
        b <- WS.body
        liftIO $ print $ ("EventSub body " ++ show (xmlEvent $ BSL.fromStrict b))
        return ()
    WS.get playPandoraRadioLikeR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            putStrLn $ "Room was: " ++ room
            playPandoraStationLike state args room' like
        return ()
    WS.get browseContentDirectoryR $ \cat filt s c so -> do
        res <- liftIO $ do
            browseContentDirectory state args cat filt s c so
        WS.text $ T.pack $ show res
        return ()
    WS.get listR $ do
        WS.json zps'
    WS.get inspectArtistsR $ \s c -> do
        adb <- getAtomicState (artists . mdb)
        let res = take c $ drop s $ M.toList adb
        WS.text $ T.pack $ show res
    WS.get inspectAlbumsR $ \s c -> do
        adb <- liftIO $ atomically $ readTVar $ albums $ mdb state
        let res = take c $ drop s $ M.toList adb
        WS.text $ T.pack $ show res
    WS.get inspectTracksR $ \s c -> do
        adb <- liftIO $ atomically $ readTVar $ tracks $ mdb state
        let res = take c $ drop s $ M.toList adb
        WS.text $ T.pack $ show res
    WS.get inspectArtistsLikeR $ \s -> do
        adb <- getAtomicState (artists . mdb)
        let res = lookupMany s adb
        WS.text $ T.pack $ show res
    WS.get inspectAlbumsLikeR $ \s -> do
        adb <- liftIO $ atomically $ readTVar $ albums $ mdb state
        let res = lookupMany s adb
        WS.text $ T.pack $ show res
    WS.get inspectTracksLikeR $ \s -> do
        adb <- liftIO $ atomically $ readTVar $ tracks $ mdb state
        liftIO $ putStrLn $ show $ length $ M.toList adb
        let res = lookupMany s adb
        WS.text $ T.pack $ show res
