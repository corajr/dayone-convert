module Data.DayOne.Opts where

import Options.Applicative
import Data.Semigroup ((<>))


data NValtToJSONOptions = NValtToJSONOptions
  { inDir :: FilePath
  , outFile :: FilePath
  }

data JSONToFlatFilesOptions = JSONToFlatFilesOptions
  { inFile :: FilePath
  , outDir :: FilePath
  }

data Command
  = NValtToJSON NValtToJSONOptions
  | JSONToFlatFiles JSONToFlatFilesOptions

data Options = Options
  { optCommand :: Command
  }

fromNvaltOptions :: Parser Command
fromNvaltOptions = NValtToJSON <$> (NValtToJSONOptions
  <$> argument str (metavar "DIRECTORY")
  <*> argument str (metavar "out.json")
  )

toFilesOptions :: Parser Command
toFilesOptions = JSONToFlatFiles <$> (JSONToFlatFilesOptions
  <$> argument str (metavar "in.json")
  <*> argument str (metavar "DIRECTORY")
  )

sample = Options
  <$> subparser
  ( command "from-nvalt" (info fromNvaltOptions ( progDesc "convert NValt export in DIRECTORY => out.json" ))
  <> command "to-files" (info toFilesOptions ( progDesc "convert in.json => flat files in DIRECTORY" ))
  )

opts = info (sample <**> helper)
 ( fullDesc
 <> progDesc "Convert to (or from) Day One JSON format"
 <> header "dayone - a converter for Day One" )
