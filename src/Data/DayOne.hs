{-# LANGUAGE TemplateHaskell #-}
module Data.DayOne where

import Data.DayOne.JSON (defaultOptions')
import Data.Aeson.TH
import Data.Time (UTCTime)
import Data.Char (toUpper)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Directory (getModificationTime, listDirectory)
import System.FilePath ((</>))

data Metadata = Metadata { version :: String }
  deriving (Eq, Show)

defaultMetadata :: Metadata
defaultMetadata = Metadata { version = "1.0" }

$(deriveJSON defaultOptions' ''Metadata)

data Entry = Entry
  { tags :: Maybe [String]
  , uuid :: String
  , starred :: Bool
  , text :: String
  , creationDate :: UTCTime
  } deriving (Eq, Show)

$(deriveJSON defaultOptions' ''Entry)

data DayOne = DayOne
  { metadata :: Metadata
  , entries :: [Entry]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions' ''DayOne)

getUUID :: IO String
getUUID = do
  uuid <- nextRandom
  return . map toUpper . filter (/= '-') . toString $ uuid

fromFile :: FilePath -> IO Entry
fromFile path = do
  modTime <- getModificationTime path
  contents <- readFile path
  uuid <- getUUID
  return $ Entry { tags = Nothing
                 , uuid = uuid
                 , starred = False
                 , text = contents
                 , creationDate = modTime
                 }

fromDirectory :: FilePath -> IO DayOne
fromDirectory path = do
  files <- listDirectory path
  let files' = map (path </>) files
  entries <- mapM fromFile files'
  return $ DayOne { metadata = defaultMetadata
                  , entries = entries
                  }
