{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Sonos.Serve where

import Control.Monad.IO.Class
import Sonos.Lib
import Sonos.Types
import Network.Wai
import Control.Concurrent.STM
import Network.Wai.Middleware.RequestLogger
import Data.Default

import Sonos.Events                     (handleEvent)
import Data.Monoid                      ((<>))
import Sonos.Discover                   (getTopology)
import Control.Concurrent               (threadDelay)
import Control.Concurrent.Async         (async)
import System.IO                        (stderr)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Map.Strict        as M
import qualified Web.Spock              as WS


serve state args = do
    WS.runSpock 5006 $ WS.spock (WS.defaultSpockCfg Nothing WS.PCNoDatabase state) (routes args)



playR :: WS.Path '[Room]
playR = "play" WS.<//> WS.var

playFavoriteR :: WS.Path '[Room, String]
playFavoriteR = "play" WS.<//> "favorite" WS.<//> WS.var WS.<//> WS.var

pauseR :: WS.Path '[Room]
pauseR = "pause" WS.<//> WS.var

playallR :: WS.Path '[]
playallR = "playall"

pauseallR :: WS.Path '[]
pauseallR = "pauseall"

clearQueueR :: WS.Path '[Room]
clearQueueR = "clearQueue" WS.<//> WS.var


volumeR :: WS.Path '[Room, Op, Int]
volumeR = "volume" WS.<//> WS.var WS.<//> WS.var WS.<//> WS.var

groupVolumeR :: WS.Path '[Room, Op, Int]
groupVolumeR = "groupVolume" WS.<//> WS.var WS.<//> WS.var WS.<//> WS.var


playLikeR :: WS.Path '[Room, String]
playLikeR = "like" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var

enqueueLikeR :: WS.Path '[Room, String]
enqueueLikeR = "like" WS.<//> "enqueue" WS.<//> WS.var WS.<//> WS.var

playLikeArtistR :: WS.Path '[Room, String]
playLikeArtistR = "likeArtist" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var

playLikeAlbumR :: WS.Path '[Room, String]
playLikeAlbumR = "likeAlbum" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var

playLikeTrackR :: WS.Path '[Room, String]
playLikeTrackR = "likeTrack" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var


enqueueLikeArtistR :: WS.Path '[Room, String]
enqueueLikeArtistR = "likeArtist" WS.<//> "enqueue" WS.<//> WS.var WS.<//> WS.var

playPandoraRadioLikeR :: WS.Path '[Room, String]
playPandoraRadioLikeR = "pandora" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var

playSongzaRadioLikeR :: WS.Path '[Room, String]
playSongzaRadioLikeR = "songza" WS.<//> "play" WS.<//> WS.var WS.<//> WS.var

playRadioLikeR :: WS.Path '[T.Text, Room, String]
playRadioLikeR = "radio" WS.<//> WS.var WS.<//> "play" WS.<//> WS.var WS.<//> WS.var


browseContentDirectoryR :: WS.Path '[T.Text, Filter, Int, Int, Sort]
browseContentDirectoryR = "browse" WS.<//> WS.var WS.<//> WS.var WS.<//> WS.var WS.<//> WS.var WS.<//> WS.var

browseMetaDataR :: WS.Path '[T.Text]
browseMetaDataR = "browseMetaData" WS.<//> WS.var

nextTrackR :: WS.Path '[Room]
nextTrackR = "next" WS.<//> WS.var

previousTrackR :: WS.Path '[Room]
previousTrackR = "previous" WS.<//> WS.var

stateR :: WS.Path '[Room]
stateR = "state" WS.<//> WS.var

speakerInfoR :: WS.Path '[Room]
speakerInfoR = "info" WS.<//> WS.var

listR :: WS.Path '[]
listR = "list"

joinR :: WS.Path '[Room, Room]
joinR = "join" WS.<//> WS.var WS.<//> WS.var

unjoinR :: WS.Path '[Room]
unjoinR = "unjoin" WS.<//> WS.var

eventSubR :: WS.Path '[]
eventSubR = "eventSub"

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

routes :: ( MonadIO m
          , WS.HasSpock (WS.SpockCtxT ctx m)
          , WS.HasSpock (WS.ActionCtxT ctx m)
          , WS.SpockState (WS.SpockCtxT ctx m) ~ State
          , WS.SpockState (WS.ActionCtxT ctx m) ~ State)
       => CliArguments
       -> WS.SpockCtxT ctx m ()
routes args = do
    state <- WS.getState
    zps' <- liftIO $ atomically $ readTVar $ zps state

    reqLogger <- liftIO $ mkRequestLogger $ def { destination = Handle stderr }
    WS.middleware $ reqLogger

    WS.get nextTrackR $ \room -> do
        let room' = getRoom zps' room
        liftIO $ nextTrack state args room'
    WS.get previousTrackR $ \room -> do
        let room' = getRoom zps' room
        liftIO $ previousTrack state args room'
    WS.get stateR $ \room -> do
        let room' = getRoom zps' room
        res <- liftIO $ getState state args room'
        WS.json res
    WS.get speakerInfoR $ \room -> do
        let room' = getRoom zps' room
        res <- liftIO $ speakerInfo state args room'
        WS.json res
    WS.get playFavoriteR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            playFavorite state args room' like
        return ()
    WS.get playLikeR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            queueAndPlayTrackLike state args room' like
        return ()
    WS.get enqueueLikeR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            queueTrackLike state args room' like
        return ()
    WS.get playLikeArtistR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            queueAndPlayLike state args room' artists like
        return ()
    WS.get playLikeAlbumR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            queueAndPlayLike state args room' albums like
        return ()
    WS.get playLikeTrackR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            queueAndPlayLike state args room' tracks like
        return ()
    WS.get enqueueLikeArtistR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            queueLike state args room' artists like
        return ()
    WS.get clearQueueR $ \room -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            clearQueue state args room'
        return ()
    WS.get playR $ \room -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            play state args room'
        return ()
    WS.get pauseR $ \room -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            pause state args room'
        return ()
    WS.get playallR $ do
        liftIO $
            playall state args
        return ()
    WS.get pauseallR $ do
        liftIO $
            pauseall state args
        return ()
    WS.get volumeR $ \room op vol -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            volume state args room' op vol
        return ()
    WS.get groupVolumeR $ \room op vol -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            groupVolume state args room' op vol
        return ()

    WS.get joinR $ \a b -> do
        let roomA = getRoom zps' a
        let roomB = getRoom zps' b
        liftIO $ do
            TIO.putStrLn $ "RoomA was: " <> (unRoom a)
            TIO.putStrLn $ "RoomB was: " <> (unRoom b)
            groupRoom state args roomA roomB
        return ()
    WS.get unjoinR $ \room -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "RoomA was: " <> (unRoom room)
            ungroupRoom state args room'
        return ()

    WS.hookRouteCustom "NOTIFY" eventSubR $ do
        b <- WS.body
        req <- WS.request
        liftIO $ handleEvent state req b

        return ()

    WS.get playPandoraRadioLikeR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            playPandoraStationLike state args room' like
        return ()

    WS.get playSongzaRadioLikeR $ \room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            playSongzaStationLike state args room' like
        return ()
    WS.get playRadioLikeR $ \radio room like -> do
        let room' = getRoom zps' room
        liftIO $ do
            TIO.putStrLn $ "Radio was: " <> (radio)
            TIO.putStrLn $ "Room was: " <> (unRoom room)
            let radioList =
                    M.fromList [ ("pandora", playPandoraStationLike state args room' like)
                               , ("songza", playSongzaStationLike state args room' like)
                               ]
                closestRadio = lookupDistance radio radioList
            closestRadio
        return ()

    WS.get browseContentDirectoryR $ \cat filt s c so -> do
        liftIO $ putStrLn $ show cat
        liftIO $ putStrLn $ show filt
        liftIO $ putStrLn $ show s
        liftIO $ putStrLn $ show c
        liftIO $ putStrLn $ show so
        res <- liftIO $ do
            browseContentDirectory state args cat filt s c so
        WS.text $ T.pack $ show res
        return ()
    WS.get browseMetaDataR $ \cat -> do
        res <- liftIO $ do
            browseMetaData state args cat
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
        let res = lookupDistance s adb--lookupMany s adb
        WS.text $ T.pack $ show res
    WS.get inspectAlbumsLikeR $ \s -> do
        adb <- liftIO $ atomically $ readTVar $ albums $ mdb state
        let res = lookupDistance s adb--lookupMany s adb
        WS.text $ T.pack $ show res
    WS.get inspectTracksLikeR $ \s -> do
        adb <- liftIO $ atomically $ readTVar $ tracks $ mdb state
        liftIO $ putStrLn $ show $ length $ M.toList adb
        let res = lookupDistance s adb--lookupMany s adb
        WS.text $ T.pack $ show res
