module Telegram.Types.InlineKeyboardButton where

import Telegram.Types.LoginUrl (LoginUrl)

data InlineKeyboardButton = InlineKeyboardButton
  { text :: String,
    url :: Maybe String,
    loginUrl :: Maybe LoginUrl,
    callbackData :: Maybe String,
    switchInlineQuery :: Maybe String,
    switchInlineQueryCurrentChat :: Maybe String,
    callbackGame :: Maybe CallbackGame,
    pay :: Maybe Bool
  }

data CallbackGame
