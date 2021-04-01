module Telegram.Types.InlineKeyboardMarkup where

import Telegram.Types.InlineKeyboardButton

newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboard :: [[InlineKeyboardButton]]
  }
