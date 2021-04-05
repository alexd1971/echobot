{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.SuccessfulPayment where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
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
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''SuccessfulPayment
 )
