{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Sonos.Main where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Options.Applicative     ( Parser
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
import Data.Monoid ((<>))
import Sonos.Discover (getTopology)
import Sonos.Serve (serve)
import Sonos.Types (CliArguments(..), State(..), MusicDB(..))
import Sonos.Events (sub)
import Sonos.Lib (browseContentDirectory)
import qualified Data.Text as T
import qualified Data.Map.Strict as M

parseArgs :: IO CliArguments
parseArgs = execParser $ info (helper <*> parseCliArgs) fullDesc

parseCliArgs :: Parser CliArguments
parseCliArgs =
    let directory = strOption
            ( long "directory"
            <> short 'd'
            <> help "Directory to search in"
            )
        email :: Parser T.Text
        email = fmap T.pack $ strOption
            ( long "email"
            <> short 'e'
            <> help "email for pandora"
            )
        password :: Parser T.Text
        password = fmap T.pack $ strOption
            ( long "password"
            <> short 'p'
            <> help "password for pandora"
            )
    in CliArguments <$> directory <*> email <*> password


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
    let fetch :: [[(T.Text,T.Text)]] -> (Int -> IO (Int, Int, [(T.Text, T.Text)])) -> Int -> IO [[(T.Text, T.Text)]]
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
                                browseContentDirectory state args "A:ARTIST" "*" s 250 "*") 0
            let !mapArtists = M.fromList $ map (\(k,v) -> (T.toLower k, v)) $ concat allArtists
            atomically $ swapTVar artists mapArtists
            putStrLn "**********************Refreshed artists**********************"

            putStrLn "**********************Refreshing albums**********************"
            allAlbums <- fetch [[]] (\s -> do
                               putStrLn $ "Fetching albums offset at" ++ show s
                               browseContentDirectory state args "A:ALBUM" "*" s 250 "*") 0
            let !mapAlbums = M.fromList $ map (\(k,v) -> (T.toLower k, v)) $ concat allAlbums
            atomically $ swapTVar albums mapAlbums
            putStrLn "**********************Refreshed albums**********************"

            putStrLn "**********************Refreshing tracks**********************"
            allTracks <- fetch [[]] (\s -> do
                               putStrLn $ "Fetching tracks offset at" ++ show s
                               browseContentDirectory state args "A:TRACKS" "*" s 250 "*") 0
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

    let mdb = MusicDB {..}
        state = State {..}
    stateStuff zps
    dbStuff state args

    zpsI <- atomically $ readTVar zps
    let s' = do
            _ <- async $ do
                print "Async subscribe"
                mapM_ (\s -> sub s "192.168.1.137" 5006) zpsI
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
