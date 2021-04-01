module Telegram.Types.SuccessfulPayment where

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
