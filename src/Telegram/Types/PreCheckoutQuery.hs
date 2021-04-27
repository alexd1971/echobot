{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.PreCheckoutQuery where

import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    camelTo2,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Telegram.Types.OrderInfo (OrderInfo)
import Telegram.Types.User (User)

data PreCheckoutQuery = PreCheckoutQuery
  { queryId :: String,
    from :: User,
    currency :: String,
    totalAmount :: Integer,
    invoicePayload :: String,
    shippingOptionId :: Maybe String,
    orderInfo :: Maybe OrderInfo
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "queryId" -> "id"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''PreCheckoutQuery
 )
