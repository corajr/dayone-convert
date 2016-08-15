module Main where

import Data.DayOne
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (decode)

main :: IO ()
main = do
  json <- BL.getContents
  print (decode json :: Maybe DayOne)
