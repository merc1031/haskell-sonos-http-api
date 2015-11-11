{-# LANGUAGE RecordWildCards #-}
module Sonos.Util where

import Sonos.Discover

import Sonos.Types

import qualified Data.Map.Strict as M


findCoordinatorIpForIp :: Location -> [ZonePlayer] -> Location
findCoordinatorIpForIp loc sds = zpLocation $ findCoordinatorForIp loc sds

findCoordinatorForIp :: Location -> [ZonePlayer] -> ZonePlayer
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

