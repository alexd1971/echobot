module Telegram.Types.LoginUrl where

data LoginUrl = LoginUrl
  { url :: String,
    forwardText :: Maybe String,
    botUsername :: Maybe String,
    requestWriteAccess :: Maybe Bool
  }
