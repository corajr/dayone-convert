{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.DayOneSpec (main, spec) where

import Test.Hspec

import Data.String.Here
import GHC.Exts
import Data.Aeson
import Data.Monoid ((<>))
import Data.Time
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.DayOne
import System.Directory (getModificationTime, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

exampleJson :: String
exampleJson = [hereFile|test/example.json|]

getModTime :: IO UTCTime
getModTime = getModificationTime "test/example/example.txt"

t1 :: UTCTime
t1 = UTCTime (fromGregorian 2016 8 15) 44837

t2 :: UTCTime
t2 = UTCTime (fromGregorian 2016 8 15) 45004

entryText1, entryText2, combinedEntryText :: String
entryText1 = "Lorem ipsum dolor sic amet"
entryText2 = "Adisciping."
combinedEntryText = entryText1 <> "\n\n" <> entryText2

entry1, entry2 :: Entry
entry1 = Entry
  { tags = Just ["Dream"]
  , uuid = "8E757B6783E24915802E98DD41743E45"
  , starred = False
  , text = entryText1
  , creationDate = t1
  }
entry2 = Entry
  { tags = Nothing
  , uuid = "870317EE535145D5A41F49B50461F792"
  , starred = False
  , text = entryText2
  , creationDate = t2
  }

file1, file2 :: TextFile
file1 = ("2016/08/2016-08-15.txt", entryText1)
file2 = ("2016/08/2016-08-15.txt", entryText2)

exampleParsed :: DayOne
exampleParsed = DayOne
  { metadata = Metadata {version = "1.0"}
  , entries = [entry1, entry2]
  }

exampleParsedFiles :: [TextFile]
exampleParsedFiles =
  [ ("2016" </> "08" </> "2016-08-15.txt", combinedEntryText)
  ]

exampleEntry :: UTCTime -> Entry
exampleEntry modTime = Entry
  { tags = Nothing
  , uuid = ""
  , starred = False
  , text = "This is a test.\n"
  , creationDate = modTime
  }

blankUUID :: Entry -> Entry
blankUUID entry = entry { uuid = "" }

exampleDayone :: UTCTime -> DayOne
exampleDayone modTime = DayOne
  { metadata = defaultMetadata
  , entries = [exampleEntry modTime]
  }

exampleEntryObject :: UTCTime -> Value
exampleEntryObject modTime =
  Object $ fromList [ ("text",String "This is a test.\n")
                    , ("uuid",String "")
                    , ("starred",Bool False)
                    , ("creationDate", toJSON modTime)]

exampleDayoneObject :: UTCTime -> Value
exampleDayoneObject modTime =
  Object $ fromList [ ("entries", Array (fromList [exampleEntryObject modTime]))
                    , ("metadata", Object (fromList [("version",String "1.0")]))]

spec :: Spec
spec = do
  describe "FromJSON instance" $ do
    it "parses a valid DayOne JSON example" $ do
      decode (BL.pack exampleJson) `shouldBe` Just exampleParsed
  describe "ToJSON instance" $ do
    it "outputs valid JSON for a DayOne value" $ do
      modTime <- getModTime
      toJSON (exampleDayone modTime) `shouldBe` exampleDayoneObject modTime
  describe "fromFile" $ do
    it "takes a path to a text file and returns an `Entry`" $ do
      modTime <- getModTime
      fmap blankUUID (fromFile "test/example/example.txt") `shouldReturn` exampleEntry modTime
  describe "fromDirectory" $ do
    it "takes a directory path and converts all files into a DayOne value" $ do
      let f x = x { entries = map blankUUID (entries x) }
      modTime <- getModTime
      fmap f (fromDirectory "test/example") `shouldReturn` exampleDayone modTime
  describe "toFile" $ do
    it "converts an Entry into a TextFile" $
      toFile entry1 `shouldBe` file1
  describe "toFiles" $ do
    it "converts a DayOne value to a list of file paths and their contents" $
      toFiles (exampleParsed) `shouldBe` exampleParsedFiles
  describe "toDirectory" $ do
    it "writes files as plain text in a directory" $
      withSystemTempDirectory "journal" $ \path -> do
        toDirectory path exampleParsed
        listDirectory path `shouldReturn` ["2016"]
        listDirectory (path </> "2016") `shouldReturn` ["08"]
        listDirectory (path </> "2016" </> "08") `shouldReturn` ["2016-08-15.txt"]
        readFile (path </> "2016" </> "08" </> "2016-08-15.txt") `shouldReturn` combinedEntryText
