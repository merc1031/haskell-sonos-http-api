module Sonos.Main where

import Control.Concurrent.STM
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

st = do
    st <- getTopology
    newTVarIO st

main = do
    args <- parseArgs
    st' <- st
    serve st' args
