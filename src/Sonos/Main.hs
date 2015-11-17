{-# LANGUAGE OverloadedStrings #-}
module Sonos.Main where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
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
                               , option
                               , auto
                               )
import Data.Monoid ((<>))
import Sonos.Discover (getTopology)
import Sonos.Serve (serve)
import Sonos.Types (CliArguments(..))
import Sonos.Events (sub)
import qualified Data.Text as T

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

setupDB = do
    conn <- open "sonos.db"
    execute_ conn "CREATE TABLE IF NOT EXISTS tracks (id INTEGER PRIMARY KEY, path TEXT, track_no INTEGER, track_name TEXT, album INTEGER, artist INTEGER)"
    execute_ conn "CREATE TABLE IF NOT EXISTS album (id INTEGER PRIMARY KEY, path TEXT, album_name TEXT, artist INTEGER)"
    execute_ conn "CREATE TABLE IF NOT EXISTS artist (id INTEGER PRIMARY KEY, path TEXT, artist_name TEXT)"
    execute_ conn "CREATE INDEX IF NOT EXISTS path_idx ON tracks (path)"
    execute_ conn "CREATE INDEX IF NOT EXISTS album_name_idx ON album (album_name)"
    execute_ conn "CREATE INDEX IF NOT EXISTS path_idx ON album (path)"
    close conn

stateStuff tv = do
    let loop = do
            d <- getTopology
            threadDelay 10000000
            atomically $ swapTVar tv d
    async $ forever $ loop

st = do
    topo <- getTopology
    newTVarIO topo

main = do
    args <- parseArgs
    st' <- st
    setupDB
    stI <- atomically $ readTVar st'
    stateStuff st'
    let s' = do
            _ <- async $ do
                print "Async subscribe"
                mapM_ (\s -> sub s "192.168.1.137" 5006) stI
            return ()
    s'

    serve st' args
