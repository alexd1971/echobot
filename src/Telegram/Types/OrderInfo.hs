module Telegram.Types.OrderInfo where

import Telegram.Types.ShippingAddress

data OrderInfo = OrderInfo
  { name :: Maybe String,
    phoneNumber :: Maybe String,
    email :: Maybe String,
    shippingAddress :: Maybe ShippingAddress
  }
