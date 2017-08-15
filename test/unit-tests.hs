{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Main where

import           ClassyPrelude

import qualified Data.List as L
import qualified Data.ByteString.Char8 as C8

import           Test.Tasty
import           Test.Tasty.HUnit as HU
import           Test.Tasty.HUnit ((@?=))

import qualified Exercise as Exercise

ts1 :: ByteString
ts1 = "23.04.2017 14:15"

ts2 :: ByteString
ts2 = "2017-04-23T14:15:45"

user :: ByteString
user = "lauri"

message :: ByteString
message = "hello"

testByteStrings :: [ByteString]
testByteStrings =
   [ "{ \"username\": \"" <> user <> "\", \"message\": \"" <> message <> "\" }" -- missing timestamp
   , "{ \"timestamp\": \"23.04.2017-14:15\", \"username\": \"" <> user <> "\", \"message\": \"" <> message <> "\" }" -- invalid timestamp
   , "{ \"timestamp\": \"" <> ts1 <> "\", \"message\": \"" <> message <> "\" }" -- missing username
   , "{ \"timestamp\": \"" <> ts1 <> "\", \"username\": \"" <> user <> "\" }" -- missing message 
   , "{ \"timestamp\": \"" <> ts1 <> "\", \"username\": \"" <> user <> "\", \"message\": \"" <> message <> "\" }" -- valid with first timestamp
   , "{ \"timestamp\": \"" <> ts2 <> "\", \"username\": \"" <> user <> "\", \"message\": \"" <> message <> "\" }" -- valid with second timestamp
   ]

testReports :: IO [Exercise.Report]
testReports = do
   time1 <- Exercise.parseTimestamp "%d.%m.%Y %H:%M" (C8.unpack ts1)
   time2 <- Exercise.parseTimestamp "%Y-%m-%dT%H:%M:%S" (C8.unpack ts2)
   return [ Exercise.Report time1 (C8.unpack user) (C8.unpack message)
          , Exercise.Report time2 (C8.unpack user) (C8.unpack message) ]

main :: IO ()
main = do
   valids <- testReports
   defaultMain $ testGroup "tests"
      [ readReportTests valids
      , readValidReportsTests valids
      ]

readReportTests :: [Exercise.Report] -> TestTree
readReportTests valids = testGroup "readReport"
   [ HU.testCase "readReport with no timestamp" $
      Exercise.readReport (testByteStrings L.!! 0) @?= Nothing
   , HU.testCase "Exercise.readReport with invalid timestamp" $
      Exercise.readReport (testByteStrings L.!! 1) @?= Nothing
   , HU.testCase "Exercise.readReport with no username" $
      Exercise.readReport (testByteStrings L.!! 2) @?= Nothing
   , HU.testCase "Exercise.readReport with no message" $
      Exercise.readReport (testByteStrings L.!! 3) @?= Nothing
   , HU.testCase "Exercise.readReport with timetamp '%d.%m.%Y %H:%M'" $
      Exercise.readReport (testByteStrings L.!! 4) @?= Just (valids L.!! 0)
   , HU.testCase "Exercise.readReport with timetamp '%Y-%m-%dT%H:%M:%S'" $
      Exercise.readReport (testByteStrings L.!! 5) @?= Just (valids L.!! 1)
   ]

readValidReportsTests :: [Exercise.Report] -> TestTree
readValidReportsTests valids = testGroup "readValidReports"
   [ HU.testCase "Exercise.readValidReports" $
      Exercise.readValidReports testByteStrings @?= valids
   ]
