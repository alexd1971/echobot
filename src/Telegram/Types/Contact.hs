module Telegram.Types.Contact where

data Contact = Contact
  { phoneNumber :: String,
    firstName :: String,
    lastName :: Maybe String,
    userId :: Maybe Integer,
    vCard :: Maybe String
  }
