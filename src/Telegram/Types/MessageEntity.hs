module Telegram.Types.MessageEntity where

import Telegram.Types.User (User)

data MessageEntity = MessageEntity
  { messageEntityType :: String,
    offset :: Integer,
    length :: Integer,
    url :: Maybe String,
    user :: Maybe User,
    language :: Maybe String
  }
