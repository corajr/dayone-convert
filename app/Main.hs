module Main where

import System.Environment (getArgs)
import Data.DayOne
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (encode)

main :: IO ()
main = do
  [path] <- getArgs
  journal <- fromDirectory path
  BL.putStrLn (encode journal)
