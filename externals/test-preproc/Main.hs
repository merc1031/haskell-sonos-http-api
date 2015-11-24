{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where
import System.FilePath.Find (find, always, fileName)
import System.FilePath.Posix (dropExtension)
import Control.Monad (liftM)
import Data.List (isSuffixOf, stripPrefix, intercalate)
import System.Environment (getArgs)
import Text.InterpolatedString.Perl6 (qc, qq)
import Control.Applicative ((<$>))
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

import Debug.Trace

replace old new = intercalate new . splitOn old

doMain :: [String] -> IO ()
doMain args = do
    let orig = args !! 0
        input = args !! 1
        out = args !! 2
        tests = findTests
    content <- fmap testFile tests
    writeFile "/tmp/test-preproc" content
    writeFile out content

findTests = find always ((isSuffixOf "Test.hs") `liftM` fileName) "tests"

dropDir :: String -> Maybe String
dropDir = stripPrefix "tests/"

noExts :: String -> String
noExts = dropExtension

noSlash :: String -> String
noSlash = replace "/" "."

putTogether :: String -> String
putTogether = ("import qualified " ++)

createTest :: String -> String
createTest = ((flip (++)) ".tests")

importTests :: [String] -> [String]
importTests = mapMaybe ((fmap $ putTogether . noSlash . noExts) . dropDir)

importTestsS :: [String] -> String
importTestsS = unlines . importTests

testsSection :: [String] -> [String]
testsSection = mapMaybe ((fmap $ createTest . noSlash . noExts) . dropDir)

testsSectionS :: [String] -> String
testsSectionS = intercalate "\n  , " . testsSection

testFile :: [String] -> String
testFile tests = [qq|
module Main where

import Test.Tasty as Tasty

{ importTestsS tests }

allTests :: TestTree
allTests = testGroup "all tests"
  [
    { testsSectionS tests }
  ]
main :: IO ()
main = Tasty.defaultMain allTests
|]

main = do
    putStrLn $ "Starting preprocessor"
    args <- getArgs
    doMain args
