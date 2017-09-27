{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module         : Exercise
-- Maintainer     : Joonas Laukka <joonas.laukka@relexsolutions.com>
-- Stability      : experimental
-- Portability    : non-portable
--
-- This short exercise simulates real world haskell programming.
--
-- Many of the necessary functions are already provided. Please use them as much
-- as possible.
------------------------------------------------------------------------------
module Exercise where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import           Data.Aeson.Lens
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (mapMaybe)

import           Control.Lens
import           Control.Monad.Plus
import           Control.Applicative

-- * Part 1: Reading a report
--
-- This Haskell program receives JSON encoded reports from multiple sources.
-- These reports don't follow a unified JSON schema but most of them contain
-- the following string values:
--
--   1) username
--   2) message
--   3) timestamp (formatted as '%d.%m.%Y %H:%M' or '%Y-%m-%dT%H:%M:%S')
--
-- These values can be extracted with the prewritten functions:
-- `getUser`, `getMessage`, `getTimestamp`.
--
-- Please implement functions for parsing a JSON object (`readReport`)
-- and reading all given reports that are valid (`readValidReports`).

-- ** Report

type Username = String

data Report = Report
   { timestamp    :: UTCTime
   , username     :: Username
   , message      :: String
   } deriving (Show, Read, Eq)

-- | Extracts timestamp from JSON encoded bytestring.
getTimestamp :: ByteString -> Maybe String
getTimestamp bs = T.unpack <$> bs ^? key "timestamp" . _String

-- | Extracts message from JSON encoded bytestring.
getMessage :: ByteString -> Maybe String
getMessage bs = T.unpack <$> bs ^? key "message" . _String

-- | Extracts user from JSON encoded bytestring.
getUser :: ByteString -> Maybe String
getUser bs = T.unpack <$> bs ^? key "username" . _String

-- | Map a bytestring to a `Report` data type if all required
-- fields are present in the input.
--
readReport :: ByteString -> Maybe Report
readReport bs =
  do { u <- getUser bs
    ; m <- getMessage bs
    ; t <- getTimestamp bs
    ; t <- parseTimestamp "%d.%m.%Y %H:%M" t <|> parseTimestamp "%Y-%m-%dT%H:%M:%S" t
    ; return Report { username = u, message = m, timestamp = t }
  }

-- parseTimestamp "%Y-%m-%dT%H:%M:%S"

-- | Read all valid reports from a list of bytestrings.
-- One bytestring per report.
--
readValidReports :: [ByteString] -> [Report]
readValidReports = mapMaybe readReport

-- ** Utility

-- | Parses input string with given format and produces an instance
-- of `ParseTime` such as `UTCTime`.
parseTimestamp :: (Monad m, ParseTime t)
               => String   -- ^ Format string
               -> String   -- ^ Input string
               -> m t
parseTimestamp = parseTimeM True defaultTimeLocale
