{-# LANGUAGE TemplateHaskell #-}
module Data.DayOne where

import Data.Aeson.TH
import Data.Time (UTCTime)

data Metadata = Metadata { version :: String }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Metadata)

data Entry = Entry
  { tags :: Maybe [String]
  , uuid :: Maybe String
  , starred :: Bool
  , text :: String
  , creationDate :: UTCTime
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Entry)

data DayOne = DayOne
  { metadata :: Metadata
  , entries :: [Entry]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''DayOne)
