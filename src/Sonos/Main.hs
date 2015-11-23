{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Sonos.Main where

import Sonos.Types
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Default

import Control.Concurrent           (threadDelay)
import Control.Monad                (forever)
import Options.Applicative          ( Parser
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
                                    , option
                                    , auto
                                    )
import Data.Monoid                  ((<>))
import Sonos.Discover               (getTopology)
import Sonos.Serve                  (serve)
import Sonos.Types                  ( CliArguments(..)
                                    , State(..)
                                    , MusicDB(..)
                                    , ZonePlayer(..)
                                    , PandoraEmail(..)
                                    , PandoraPassword(..)
                                    , SongzaId(..)
                                    )
import Sonos.Events                 (subAll)
import Sonos.Lib                    (browseContentDirectory)

import qualified Data.Text as T
import qualified Data.Map.Strict as M

parseArgs :: IO CliArguments
parseArgs = execParser $ info (helper <*> parseCliArgs) fullDesc

parseCliArgs :: Parser CliArguments
parseCliArgs =
    let pandoraEmail :: Parser PandoraEmail
        pandoraEmail = fmap (PandoraEmail . T.pack) $ strOption
            ( long "pandora-email"
            <> help "email for pandora"
            )
        pandoraPassword :: Parser PandoraPassword
        pandoraPassword = fmap (PandoraPassword . T.pack) $ strOption
            ( long "pandora-password"
            <> help "password for pandora"
            )
        songzaId :: Parser SongzaId
        songzaId = fmap (SongzaId . T.pack) $ strOption
            ( long "songza-id"
            <> help "user id for songza"
            )
    in CliArguments <$> pandoraEmail <*> pandoraPassword <*> songzaId


stateStuff topoV = do
    let pop = do
            topo <- getTopology
            atomically $ swapTVar topoV topo

    let loop = do
            pop
            threadDelay 10000000
    pop
    async $ forever $ loop

dbStuff state args = do
    let MusicDB {..} = mdb state
    putStrLn "Prepping music db"
    let fetch :: [[(T.Text,(T.Text, T.Text))]]
              -> (Int -> IO (Int, Int, [(T.Text, (T.Text, T.Text))]))
              -> Int
              -> IO [[(T.Text, (T.Text, T.Text))]]
        fetch !xs fn !s = do
            (nr, tm, !res) <- fn s
            if (s + nr) < tm
              then fetch (res:xs) fn (s + nr)
              else return $ res:xs
    let pop = do
            putStrLn "**********************Refreshing music db**********************"

            putStrLn "**********************Refreshing artists**********************"
            allArtists <- fetch [[]] (\s -> do
                                putStrLn $ "Fetching artists offset at" ++ show s
                                browseContentDirectory state args "A:ARTIST" FAll s 250 SAll) 0
            let !mapArtists = M.fromList $ map (\(k,v) -> (T.toLower k, v)) $ concat allArtists
            atomically $ swapTVar artists mapArtists
            putStrLn "**********************Refreshed artists**********************"

            putStrLn "**********************Refreshing albums**********************"
            allAlbums <- fetch [[]] (\s -> do
                               putStrLn $ "Fetching albums offset at" ++ show s
                               browseContentDirectory state args "A:ALBUM" FAll s 250 SAll) 0
            let !mapAlbums = M.fromList $ map (\(k,v) -> (T.toLower k, v)) $ concat allAlbums
            atomically $ swapTVar albums mapAlbums
            putStrLn "**********************Refreshed albums**********************"

            putStrLn "**********************Refreshing tracks**********************"
            allTracks <- fetch [[]] (\s -> do
                               putStrLn $ "Fetching tracks offset at" ++ show s
                               browseContentDirectory state args "A:TRACKS" FAll s 250 SAll) 0
            let !mapTracks = M.fromList $ map (\(k,v) -> (T.toLower k, v)) $ concat allTracks
            atomically $ swapTVar tracks mapTracks
            putStrLn "**********************Refreshed tracks**********************"

            putStrLn "**********************Refreshed music db**********************"
    let loop = do
            pop
            threadDelay $ 3600 * 1000000
    async $ forever $ loop

prepState args = do
    zps <- newTVarIO []
    artists <- newTVarIO M.empty
    albums <- newTVarIO M.empty
    tracks <- newTVarIO M.empty

    stateStuff zps
    zpsI <- atomically $ readTVar zps
    speakers <- M.fromList <$> mapM (\zp -> do
            tvar <- newTVarIO def
            return (zpUUID zp, tvar)
         ) zpsI
    let mdb = MusicDB {..}
        state = State {..}
    dbStuff state args

    let s' = do
            _ <- async $ do
                print "Async subscribe"
                mapM_ (\s -> subAll s "192.168.1.137" 5006) zpsI
            return ()
    s'
    return state

main = do
    args <- parseArgs
    state <- prepState args


    putStrLn ""
    putStrLn ""
    putStrLn "Ready To Serve Content"
    putStrLn ""
    putStrLn ""
    serve state args
