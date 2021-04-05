{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ShippingAddress where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)

data ShippingAddress = ShippingAddress
  { countryCode :: String,
    state :: String,
    city :: String,
    streetLine1 :: String,
    streetLine2 :: String,
    postCode :: String
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''ShippingAddress
 )
