module Telegram.Types.Game where

import Telegram.Types.Animation
import Telegram.Types.MessageEntity
import Telegram.Types.PhotoSize

data Game = Game
  { title :: String,
    description :: String,
    photo :: [PhotoSize],
    text :: Maybe String,
    textEntities :: Maybe [MessageEntity],
    animation :: Maybe Animation
  }
