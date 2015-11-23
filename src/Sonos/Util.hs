{-# LANGUAGE RecordWildCards #-}
module Sonos.Util where

import Sonos.Discover
import Sonos.Types

import qualified Data.Map.Strict as M


findCoordinatorIpForIp :: Location
                       -> [ZonePlayer]
                       -> Location
findCoordinatorIpForIp loc sds = zpLocation $ findCoordinatorForIp loc sds

findCoordinatorForIp :: Location
                     -> [ZonePlayer]
                     -> ZonePlayer
findCoordinatorForIp loc sds =
    let (Location _ u _ _) = loc
        m = M.fromList $ map (\zp@(ZonePlayer {..}) ->
                                    let Location {..} = zpLocation
                                    in (lUrl, zp)
                             ) sds
        Just res = M.lookup u m
        g = zpGroup res
        c = M.filter (\ZonePlayer {..} ->
                        let groupMatch = g == zpGroup
                            coord = zpCoordinator
                        in groupMatch && coord
                     ) m
        me = head $ M.elems c
    in me

findCoordinators :: [ZonePlayer]
                 -> [ZonePlayer]
findCoordinators sds =
    let m = M.fromListWithKey findCoord $ map (\zp@(ZonePlayer {..}) ->
                                                    (zpGroup, zp)
                                              ) sds
        findCoord :: String
                  -> ZonePlayer
                  -> ZonePlayer
                  -> ZonePlayer
        findCoord k l r =
            let lCoord = zpCoordinator l
                rCoord = zpCoordinator r
            in case (lCoord, rCoord) of
              (False, False) -> l
              (True, False) -> l
              (False, True) -> r
              _ -> error "cannot have 2 coordinators"
    in M.elems m
