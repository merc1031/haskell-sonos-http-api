{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -w #-}
module Compare where

import Test.Tasty
import Test.Tasty.TH

import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import Control.Monad (forM, forM_)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Sonos.Events
import Sonos.Types
import Sonos.Lib (lookupDistance')
import Text.EditDistance

case_comparisons = do
    let albums =
            [ "...And the Night Swept Us Away"
            , "Týr"
            , "Epicloud"
            , "Unplugged"
            , "Ziltoid The Omniscient"
            , "Orphaned Land"
            , "The Heart of the Matter"
            , "Triosphere"
            , "Leprous"
            , "The Congregation"
            , "Agalloch"
            , "Birch White"
            , "Ayreon"
            , "Between the Buried and Me"
            , "Coma Ecliptic"
            ]
        albumM = M.fromList $ zip albums albums
        expectation =
            [ [("And the Night Swept Us Away", 0)]
            , [("Tyr", 1), ("Tear", 2)]
            , [("Epic Loud", 0)]
            , [("Unplugged", 0)]
            , [("Ziltoid the Omniscient", 0)]
            , [("Orphaned Land", 0)]
            , [("The Heart of the Matter", 0), ("Heart of the Matter", 0)]
            , [("Triosphere", 0), ("Try Sphere", 2)]
            , [("Leprous", 0)]
            , [("The Congregation", 0)]
            , [("Agalloch", 0)]
            , [("Birch White", 0)]
            , [("Ayreon", 0), ("Air Ion", 2)]
            , [("Between the Buried and Me", 0)]
            , [("Coma Ecliptic", 0)]
            ]

    forM_ expectation $ \(cases) ->
        forM cases $ \(text, distance) -> do
            let d = lookupDistance' text albumM
            assertEqual (concat ["Distance was ", T.unpack text]) distance (fst d)

tests = $(testGroupGenerator)

main = defaultMain tests
