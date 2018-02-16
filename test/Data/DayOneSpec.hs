{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.DayOneSpec (main, spec) where

import Test.Hspec

import Data.String.Here
import GHC.Exts
import Data.Aeson
import Data.Time
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.DayOne
import System.Directory (getModificationTime)

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

exampleParsed :: DayOne
exampleParsed = DayOne
  { metadata = Metadata {version = "1.0"}
  , entries = [ Entry { tags = Just ["Dream"]
                      , uuid = "8E757B6783E24915802E98DD41743E45"
                      , starred = False
                      , text = "Lorem ipsum dolor sic amet"
                      , creationDate = t1
                      }
              , Entry { tags = Nothing
                      , uuid = "870317EE535145D5A41F49B50461F792"
                      , starred = False
                      , text = "Adisciping."
                      , creationDate = t2
                      }
              ]
  }

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
