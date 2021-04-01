module Telegram.Types.Location where

data Location = Location
  { longitude :: Float,
    latitude :: Float,
    horizontalAccuracy :: Maybe Float,
    livePeriod :: Maybe Integer,
    heading :: Maybe Integer,
    proximityAlertRadius :: Maybe Integer
  }
