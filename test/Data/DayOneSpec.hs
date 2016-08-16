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

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

exampleJson :: String
exampleJson = [hereFile|test/example.json|]

t1 :: UTCTime
t1 = UTCTime (fromGregorian 2016 8 15) 44837

t2 :: UTCTime
t2 = UTCTime (fromGregorian 2016 8 15) 45004

modTime :: UTCTime
modTime = UTCTime (fromGregorian 2016 8 15) 80131

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

exampleEntry :: Entry
exampleEntry = Entry
  { tags = Nothing
  , uuid = ""
  , starred = False
  , text = "This is a test.\n"
  , creationDate = modTime
  }

blankUUID :: Entry -> Entry
blankUUID entry = entry { uuid = "" }

exampleDayone :: DayOne
exampleDayone = DayOne
  { metadata = defaultMetadata
  , entries = [exampleEntry]
  }

exampleEntryObject :: Value
exampleEntryObject =
  Object $ fromList [ ("text",String "This is a test.\n")
                    , ("uuid",String "")
                    , ("starred",Bool False)
                    , ("creationDate",String "2016-08-15T22:15:31Z")]

exampleDayoneObject :: Value
exampleDayoneObject =
  Object $ fromList [ ("entries", Array (fromList [exampleEntryObject]))
                    , ("metadata", Object (fromList [("version",String "1.0")]))]

spec :: Spec
spec = do
  describe "FromJSON instance" $ do
    it "parses a valid DayOne JSON example" $ do
      decode (BL.pack exampleJson) `shouldBe` Just exampleParsed
  describe "ToJSON instance" $ do
    it "outputs valid JSON for a DayOne value" $ do
      toJSON exampleDayone `shouldBe` exampleDayoneObject
  describe "fromFile" $ do
    it "takes a path to a text file and returns an `Entry`" $ do
      fmap blankUUID (fromFile "test/example/example.txt") `shouldReturn` exampleEntry
  describe "fromDirectory" $ do
    it "takes a directory path and converts all files into a DayOne value" $ do
      let f x = x { entries = map blankUUID (entries x) }
      fmap f (fromDirectory "test/example") `shouldReturn` exampleDayone
