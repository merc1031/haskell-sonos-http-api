{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
module Sonos.Lib where

import Network.Wreq
import Control.Concurrent.STM
import Sonos.Types
import Sonos.Commands
import Sonos.XML
import Text.EditDistance

import Web.PathPieces                       (PathPiece(toPathPiece))
import Data.Maybe                           ( fromJust)
import Sonos.Util                           ( findCoordinatorForIp
                                            , findCoordinators
                                            )
import Control.Monad                        ( forever
                                            , when
                                            , void
                                            )
import Control.Lens                         ( (^?))
import Formatting                           ( stext
                                            , (%)
                                            , int
                                            , sformat
                                            )
import Data.Char                            (toLower, isAlphaNum)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TLB
import qualified Sonos.Plugins.Pandora      as Pandora
import qualified Sonos.Plugins.Songza       as Songza
import qualified HTMLEntities.Builder       as HTML
import qualified HTMLEntities.Decoder       as HTML

getZPs = atomically . readTVar . zps

lookupDistance :: T.Text
               -> M.Map T.Text a
               -> a
lookupDistance s m = snd $ lookupDistance' s m

lookupDistance' :: T.Text
               -> M.Map T.Text a
               -> (Int, a)
lookupDistance' s m =
    let lev = levenshteinDistance defaultEditCosts
        clean t = T.map toLower $ T.filter isAlphaNum t
    in M.findMin
     $ M.fromList
     $ M.elems
     $ M.mapWithKey (\k v -> ( lev (T.unpack $ clean k) (T.unpack $ clean s), v))
                    m



getRoom :: [ZonePlayer]
        -> Room
        -> ZonePlayer
getRoom zps room =
    let rooms = M.fromList
              $ map (\zp@(ZonePlayer {..}) ->
                    let name = zpName
                    in (T.toLower name, zp)
                    )
                    zps

        Just room' = M.lookup (T.toLower $ unRoom room) rooms
    in room'

groupRoom :: State
          -> CliArguments
          -> ZonePlayer
          -> ZonePlayer
          -> IO ()
groupRoom state args a b = do
    zps <- getZPs state
    let coordA = findCoordinatorForIp (zpLocation a) zps
        coordB = findCoordinatorForIp (zpLocation b) zps
        addr = let l = zpLocation coordA
               in urlFmt (lUrl l) (lPort l)
    let avMessage = setAVTransportURITemplate (fmtRincon (zpUUID coordB)) ""
    avSoapAction addr avMessage
    return ()

ungroupRoom :: State
            -> CliArguments
            -> ZonePlayer
            -> IO ()
ungroupRoom state args room = do
    let addr = let l = zpLocation room
               in urlFmt (lUrl l) (lPort l)
    let avMessage = becomeCoordinatorOfStandaloneGroup
    avSoapAction addr avMessage
    return ()



getSpeakerState state zp = do
    let speakersData = speakers state
        Just rsStateV = M.lookup (zpUUID zp) speakersData

    rsState <- atomically $ readTVar rsStateV
    return rsState

modVolume (SpeakerState {..}) op val = case op of
    E -> val
    Pl -> ssVolume + val
    Mi -> ssVolume - val

volume :: State
       -> CliArguments
       -> ZonePlayer
       -> Op
       -> Int
       -> IO ()
volume state args host op value = do
    let addr = let l = zpLocation host
               in urlFmt (lUrl l) (lPort l)
    ss <- getSpeakerState state host

    let newVol = modVolume ss op value
    rcSoapAction addr (volumeTemplate newVol)
    return ()

groupVolume :: State
            -> CliArguments
            -> ZonePlayer
            -> Op
            -> Int
            -> IO ()
groupVolume state args host op value = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)
    ss <- getSpeakerState state coord

    let newVol = modVolume ss op value

    grcSoapAction addr (groupVolumeTemplate newVol)
    return ()

clearQueue :: State
           -> CliArguments
           -> ZonePlayer
           -> IO ()
clearQueue state args host = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)


    void $ avSoapAction' addr removeAllTracksFromQueueTemplate
    return ()

play :: State
     -> CliArguments
     -> ZonePlayer
     -> IO ()
play state args host = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    avSoapAction addr playTemplate
    return ()

pause :: State
      -> CliArguments
      -> ZonePlayer
      -> IO ()
pause state args host = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)


    ss <- getSpeakerState state coord
    when (ssPlayerState ss == "PLAYING") $
        void $ avSoapAction' addr pauseTemplate
    return ()

playall :: State
        -> CliArguments
        -> IO ()
playall state args = do
    zps <- getZPs state
    let coords = findCoordinators zps
        toAddr zp = let l = zpLocation zp
               in urlFmt (lUrl l) (lPort l)

    mapM_ (\zp -> avSoapAction (toAddr zp) playTemplate) coords
    return ()

pauseall :: State
         -> CliArguments
         -> IO ()
pauseall state args = do
    zps <- getZPs state
    let coords = findCoordinators zps
        toAddr zp = let l = zpLocation zp
               in urlFmt (lUrl l) (lPort l)

    putStrLn "Pausing all"
    putStrLn $ show coords
    mapM_ (\zp -> do
          ss <- getSpeakerState state zp
          when (ssPlayerState ss == "PLAYING") $
            void $ avSoapAction (toAddr zp) pauseTemplate) coords
    return ()

playFavorite :: State
             -> CliArguments
             -> ZonePlayer
             -> String
             -> IO ()
playFavorite state args host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)
    (_,_,cd) <- browseContentDirectory state
                                       args
                                       "FV:2"
                                       FNone
                                       0
                                       1000
                                       SNone
    let m = M.fromList cd
        (DBData t d md) = lookupDistance (T.pack like) m

    let avMessage = setAVTransportURITemplate (TL.toStrict $ TLB.toLazyText $ HTML.text d) (TL.toStrict $ TLB.toLazyText $ HTML.text md)

    avSoapAction addr avMessage
    avSoapAction addr playTemplate

    return ()


queueAndPlayTrackLike :: State
                      -> CliArguments
                      -> ZonePlayer
                      -> String
                      -> IO ()
queueAndPlayTrackLike state args host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
    queuedBodys <- queueTrackLike state args host like
    let trackNo = getTrackNum $ fromJust $ head $ queuedBodys
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    uuid <- fetchUUID addr
    let avMessage = setAVTransportURITemplate (fmtRinconQueue uuid) ""

    avSoapAction addr avMessage
    avSoapAction addr (seekTrackTemplate trackNo)
    avSoapAction addr playTemplate


    return ()

queueTrackLike :: State
               -> CliArguments
               -> ZonePlayer
               -> String
               -> IO [Maybe BSL.ByteString]
queueTrackLike state args host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
    tracksM <- atomically $ readTVar $ tracks $ mdb state
    let (DBData t tracks tracksMD) = lookupDistance (T.pack like) tracksM
        tracks' = [(like, tracks)]
    putStrLn $ "Tracks are:" ++ show tracks'


    let soapMessage track = addURIToQueueTemplate track
                                                  ""
                                                  0
                                                  0
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    mapM (\t -> (\r -> r ^? responseBody) <$> avSoapAction addr (soapMessage $ snd t)) tracks'

queueAndPlayLike :: State
                 -> CliArguments
                 -> ZonePlayer
                 -> (MusicDB -> TVar (M.Map T.Text DBData))
                 -> String
                 -> IO ()
queueAndPlayLike state args host selector like = do
    putStrLn $ "queueAndPlayLike " ++ show like
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
    queuedBodys <- queueLike state args host selector like
    let qd = getQueueData $ fromJust $ head $ queuedBodys
        trackNo = sqdFirstTrackOfNewQueue qd
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    uuid <- fetchUUID addr
    let avMessage = setAVTransportURITemplate (fmtRinconQueue uuid) ""

    putStrLn $ "QD: " ++ show qd
    putStrLn $ "First track: " ++ show trackNo
    avSoapAction addr avMessage
    avSoapAction addr (setPlayModeTemplate PlayMode { repeat = False, shuffle = False, crossfade = False })
    avSoapAction addr (seekTrackTemplate trackNo)
    avSoapAction addr playTemplate


    return ()

queueLike :: State
          -> CliArguments
          -> ZonePlayer
          -> (MusicDB -> TVar (M.Map T.Text DBData))
          -> String
          -> IO [Maybe BSL.ByteString]
queueLike state args host selector like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
    selectM <- atomically $ readTVar $ selector $ mdb state
    let (DBData t tracks tracksMD) = lookupDistance (T.pack like) selectM
        tracks' = [(like, tracks)]
    putStrLn $ "Tracks are:" ++ show tracks'


    let soapMessage track = addURIToQueueTemplate track
                                                  ""
                                                  0
                                                  0
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    mapM (\t -> (\r -> r ^? responseBody) <$> avSoapAction addr (soapMessage $ snd t)) tracks'


nextTrack :: State
          -> CliArguments
          -> ZonePlayer
          -> IO ()
nextTrack state args host = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)
    avSoapAction addr nextTrackTemplate
    return ()

previousTrack :: State
              -> CliArguments
              -> ZonePlayer
              -> IO ()
previousTrack state args host = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)
    avSoapAction addr previousTrackTemplate
    return ()


getState :: State
         -> CliArguments
         -> ZonePlayer
         -> IO ReplyState
getState state args host = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        speakersData = speakers state
        Just rsStateV = M.lookup (zpUUID host) speakersData
        Just rsCoordinatorStateV = M.lookup (zpUUID coord) speakersData

    putStrLn $ "Geting state for " ++ show host
    rsState <- atomically $ readTVar rsStateV
    rsCoordinatorState <- atomically $ readTVar rsCoordinatorStateV

    let rsCurrentTrack = ssCurrentTrack rsCoordinatorState
        rsNextTrack = ssNextTrack rsCoordinatorState
        rsVolume = ssVolume rsState
        rsMute = ssMute rsState
        rsTrackNo = ssTrackNo rsCoordinatorState
        rsElapsedTime = ssElapsedTime rsCoordinatorState
        rsElapsedTimeFormatted = ssElapsedTimeFormatted rsCoordinatorState
        rsZoneState = ssPlayerState rsCoordinatorState
        rsPlayerState = ssPlayerState rsState
        rsZonePlayMode = ssCurrentPlayMode rsCoordinatorState

    return $ ReplyState {..}



pandoraRadioFmt = sformat ("pndrradio:" % stext % "?sn=6")

headErr m l = case l of
    [] -> error m
    _ -> head l

playPandoraStationLike :: State
                       -> CliArguments
                       -> ZonePlayer
                       -> String
                       -> IO ()
playPandoraStationLike state args@(CliArguments{..}) host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    pw <- Pandora.login (unPandoraEmail caPandoraEmail)
                        (unPandoraPassword caPandoraPassword)
    st <- Pandora.searchStation pw $ T.pack like
    st' <- Pandora.createStation pw (Pandora.artistMusicToken $ headErr "No pandora artists" $ Pandora.msrArtists st)

    let station = Pandora.csrStation st'
        stationId = Pandora.sStationId station
        stationName = Pandora.sStationName station
        metadata = didlTemplate stationId
                                stationName
                                (unPandoraEmail caPandoraEmail)
        avMessage = setAVTransportURITemplate (pandoraRadioFmt stationId)
                                              metadata

    avSoapAction addr avMessage
    avSoapAction addr playTemplate


    return ()


playSongzaStationLike :: State
                       -> CliArguments
                       -> ZonePlayer
                       -> String
                       -> IO ()
playSongzaStationLike state args@(CliArguments{..}) host like = do
    zps <- getZPs state
    let coord = findCoordinatorForIp (zpLocation host) zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)

    st <- Songza.getStationList $ T.pack like
    let firstSt = head st

    let stationId = Songza.slrId firstSt
        stationName = Songza.slrName firstSt
        metadata = Songza.mkMetaData stationId
                                     "0" -- Not sure how to discover the situationid taht goes here
                                     stationName
                                     (unSongzaId caSongzaId)
        avMessage = setAVTransportURITemplate (Songza.mkUri stationId)
                                              metadata

    avSoapAction addr avMessage
    avSoapAction addr playTemplate


    return ()

browseContentDirectory :: State
                       -> CliArguments
                       -> T.Text -- Category A:ARTIST A:ALBUM A:TRACK
                       -> Filter -- Filter?
                       -> Int -- Start
                       -> Int -- Count
                       -> Sort --sort criteria
                       -> IO (Int, Int, [(T.Text, DBData)])
browseContentDirectory state args cat filt s c sor = do
    zps <- getZPs state
    let coord = head zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)


        cdMessage = browseContentDirectoryTemplate cat
                                                   "BrowseDirectChildren"
                                                   (toPathPiece filt)
                                                   s
                                                   c
                                                   (toPathPiece sor)

    resp <- cdSoapAction addr cdMessage

    when (cat == "0" || cat == "FV:2" || cat == "FV:3") $
        print resp
    let Just body = resp ^? responseBody
        structured = browsedContent (BrowseDefault cat) body
    return structured

browseMetaData :: State
               -> CliArguments
               -> T.Text -- Category A:ARTIST A:ALBUM A:TRACK
               -> IO (Int, Int, [(T.Text, DBData)])
browseMetaData state args cat = do
    zps <- getZPs state
    let coord = head zps
        addr = let l = zpLocation coord
               in urlFmt (lUrl l) (lPort l)


        cdMessage = getMetaData cat

    resp <- cdSoapAction addr cdMessage

    let Just body = resp ^? responseBody
        structured = browsedContent (BrowseSpecified "container") body

    putStrLn $ show body
    return structured

speakerInfo :: State
            -> CliArguments
            -> ZonePlayer
            -> IO AlexaSpeakResponse
speakerInfo state args host = do
    speakerState <- getSpeakerState state host
    let trackFormat TrackState {..} =
            sformat (stext % " by " % stext % " from the album " % stext) 
                    tsTitle
                    tsArtist
                    tsAlbum
        sentences =
            [ sformat ("The current track is " % stext) (trackFormat $ ssCurrentTrack speakerState)
            , sformat ("The next track is " % stext) (trackFormat $ ssNextTrack speakerState)
            , sformat ("Volume in the " % stext % " is " % int) (zpName host) (ssVolume speakerState)
            ]
        words = T.intercalate ". " sentences
    return $ AlexaSpeakResponse $ Lit words
