{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.DayOne where

import Data.Aeson (decode, encode)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import Data.Function (on)
import Data.DayOne.Opts hiding (Options)
import Options.Applicative (execParser)
import qualified Data.DayOne.Opts as Opts
import Data.List (intercalate, groupBy)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Char (toUpper)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import System.Directory (createDirectoryIfMissing, getModificationTime, listDirectory)
import System.FilePath ((</>), takeDirectory)

data Metadata = Metadata { version :: String }
  deriving (Eq, Show)

defaultMetadata :: Metadata
defaultMetadata = Metadata { version = "1.0" }

$(deriveJSON defaultOptions ''Metadata)

data Entry = Entry
  { tags :: Maybe [String]
  , uuid :: String
  , starred :: Bool
  , text :: String
  , creationDate :: UTCTime
  } deriving (Eq, Show)

$(deriveJSON defaultOptions { omitNothingFields = True } ''Entry)

data DayOne = DayOne
  { metadata :: Metadata
  , entries :: [Entry]
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''DayOne)

type FileContents = String
type TextFile = (FilePath, FileContents)

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

toFile :: Entry -> TextFile
toFile (Entry {creationDate, text}) = (convertDate creationDate, text)
  where
    convertDate = formatTime defaultTimeLocale "%Y/%m/%d.txt"

toFiles :: DayOne -> [TextFile]
toFiles (DayOne _ entries) = flattened
  where
    ungroupedFiles = map toFile entries
    groupedFiles = groupBy ((==) `on` fst) ungroupedFiles
    flattenFileGroup xs@((x, _):_) = (x, intercalate "\n\n" (map snd xs))
    flattened = map flattenFileGroup groupedFiles

toDirectory :: FilePath -> DayOne -> IO ()
toDirectory path = mapM_ writePlainText . toFiles
  where
    writePlainText (fname, contents) = do
      let fname' = path </> fname
      createDirectoryIfMissing True (takeDirectory fname')
      writeFile fname' contents

cliMain :: IO ()
cliMain = cliMain' =<< execParser Opts.opts

cliMain' :: Opts.Options -> IO ()
cliMain' (Opts.Options (NValtToJSON (NValtToJSONOptions {inDir, outFile}))) = do
  dayOne <- fromDirectory inDir
  BL.writeFile outFile (encode dayOne)
cliMain' (Opts.Options (JSONToFlatFiles (JSONToFlatFilesOptions {inFile, outDir}))) = do
  Just dayOne <- (decode <$> BL.readFile inFile)
  toDirectory outDir dayOne
