module Data.DayOne.JSON where

import Data.Aeson.TH

defaultOptions' :: Options
defaultOptions' = defaultOptions { omitNothingFields = True }
