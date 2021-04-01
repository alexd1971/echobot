module Telegram.Types.Invoice where

data Invoice = Invoice
  { title :: String,
    description :: String,
    startParameter :: String,
    currency :: String,
    totalAmount :: Integer
  }
