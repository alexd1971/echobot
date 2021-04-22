{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Venue where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)
import Telegram.Types.Location (Location)

data Venue = Venue
  { location :: Location,
    title :: String,
    address :: String,
    foursquareId :: Maybe String,
    foursquareType :: Maybe String,
    googlePlaceId :: Maybe String,
    googlePlaceType :: Maybe String
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Venue
 )
