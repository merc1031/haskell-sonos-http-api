{-# LANGUAGE RecordWildCards #-}
module Util where

import Discover

import Types

import qualified Data.Map.Strict as M


findCoordinatorIpForIp :: Location -> [ZonePlayer] -> Location
findCoordinatorIpForIp loc sds =
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
    in zpLocation me

