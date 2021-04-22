{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.OrderInfo where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics
import Telegram.Types.ShippingAddress

data OrderInfo = OrderInfo
  { name :: Maybe String,
    phoneNumber :: Maybe String,
    email :: Maybe String,
    shippingAddress :: Maybe ShippingAddress
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''OrderInfo
 )
