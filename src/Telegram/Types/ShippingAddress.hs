module Telegram.Types.ShippingAddress where

data ShippingAddress = ShippingAddress
  { countryCode :: String,
    state :: String,
    city :: String,
    streetLine1 :: String,
    streetLine2 :: String,
    postCode :: String
  }
