module Types where

data Location = Location
    { lProto :: String
    , lUrl :: String
    , lPort :: String
    , lEndpoint :: String
    } deriving Show

data SonosDiscovery = SonosDiscovery
    { sdLocation :: Location
    , sdServer :: String
    , sdST :: String
    , sdUSN :: String
    , sdHousehold :: String
    , sdBootSeq :: String
    } deriving Show

data ZonePlayer = ZonePlayer 
    { zpGroup :: String
    , zpCoordinator :: Bool
    , zpLocation :: Location
    , zpBootSeq :: String
    , zpUUID :: String
    , zpName :: String
    } deriving (Show)

