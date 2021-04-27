{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ShippingQuery where

import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    camelTo2,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Telegram.Types.ShippingAddress (ShippingAddress)
import Telegram.Types.User (User)

data ShippingQuery = ShippingQuery
  { queryId :: String,
    from :: User,
    invoicePayload :: String,
    shippingAddress :: ShippingAddress
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "queryId" -> "id"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''ShippingQuery
 )
