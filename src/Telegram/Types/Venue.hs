module Telegram.Types.Venue where

import Telegram.Types.Location

data Venue = Venue
  { location :: Location,
    title :: String,
    address :: String,
    foursquareId :: Maybe String,
    foursquareType :: Maybe String,
    googlePlaceId :: Maybe String,
    googlePlaceType :: Maybe String
  }
