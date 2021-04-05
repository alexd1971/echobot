{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Location where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

data Location = Location
  { longitude :: Float,
    latitude :: Float,
    horizontalAccuracy :: Maybe Float,
    livePeriod :: Maybe Integer,
    heading :: Maybe Integer,
    proximityAlertRadius :: Maybe Integer
  } deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Location
 )
