{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.DayOne where

import Control.Arrow ((&&&))
import Data.Aeson (decode, encode)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (getArgs)
import Data.Semigroup
import Data.DayOne.Opts hiding (Options)
import Options.Applicative (execParser)
import qualified Data.DayOne.Opts as Opts
import Data.List.NonEmpty (groupWith)
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

instance Semigroup Entry where
  e1 <> e2 = Entry
    { tags = Nothing
    , uuid = uuid e1
    , starred = starred e1
    , text = text e1 <> "\n\n" <> text e2
    , creationDate = creationDate e1
    }

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

getEntryFilename :: Entry -> FilePath
getEntryFilename (Entry {creationDate}) =
  formatTime defaultTimeLocale "%Y/%m/%Y-%m-%d.txt" creationDate

toFile :: Entry -> TextFile
toFile = getEntryFilename &&& text

toFiles :: DayOne -> [TextFile]
toFiles (DayOne _ entries) = files
  where
    groupedEntries = groupWith (getEntryFilename) entries
    files = map (toFile . sconcat) groupedEntries

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
