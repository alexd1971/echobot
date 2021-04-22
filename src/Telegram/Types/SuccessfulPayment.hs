{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.SuccessfulPayment where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics
import Telegram.Types.OrderInfo

data SuccessfulPayment = SuccessfulPayment
  { currency :: String,
    totalAmount :: Integer,
    invoicePayload :: String,
    shippingOptionId :: Maybe String,
    orderInfo :: Maybe OrderInfo,
    telegramPaymentChargeId :: String,
    providerPaymentChargeId :: String
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''SuccessfulPayment
 )
