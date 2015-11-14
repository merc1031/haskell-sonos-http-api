{-# LANGUAGE OverloadedStrings #-}
module Sonos.Main where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import           Database.SQLite.Simple
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
                               )
import Data.Monoid ((<>))
import Sonos.Discover (getTopology)
import Sonos.Serve (serve)
import Sonos.Types (CliArguments(..))
import Sonos.Events (sub)

parseArgs :: IO CliArguments
parseArgs = execParser $ info (helper <*> parseCliArgs) fullDesc

parseCliArgs :: Parser CliArguments
parseCliArgs =
    let directory = strOption
            ( long "directory"
            <> short 'd'
            <> help "Directory to search in"
            )
    in CliArguments <$> directory

setupDB = do
    conn <- open "sonos.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS tracks (id INTEGER PRIMARY KEY, path TEXT, track_no INTEGER, track_name TEXT, album INTEGER, artist INTEGER)"
    execute_ conn "CREATE TABLE IF NOT EXISTS album (id INTEGER PRIMARY KEY, path TEXT, album_name TEXT, artist INTEGER)"
    execute_ conn "CREATE TABLE IF NOT EXISTS artist (id INTEGER PRIMARY KEY, path TEXT, artist_name TEXT)"
    execute_ conn "CREATE INDEX IF NOT EXISTS path_idx ON tracks (path)"
    execute_ conn "CREATE INDEX IF NOT EXISTS album_name_idx ON album (album_name)"
    execute_ conn "CREATE INDEX IF NOT EXISTS path_idx ON album (path)"
    close conn


st = do
    topo <- getTopology
    newTVarIO topo

main = do
    args <- parseArgs
    st' <- st
    setupDB
    stI <- atomically $ readTVar st'
    let s' = do
            _ <- async $ do
                sub (head stI) "192.168.1.137" 5006
                threadDelay 85000000
                s'
            return ()
    s'

    serve st' args
