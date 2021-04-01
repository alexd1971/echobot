{-# LANGUAGE OverloadedStrings #-}

module Telegram.Types.Chat where

import Data.Aeson
import Telegram.Types.Message

data Chat = Chat
  { chatId :: Integer,
    chatType :: String,
    title :: Maybe String,
    username :: Maybe String,
    firstName :: Maybe String,
    lastName :: Maybe String,
    photo :: Maybe ChatPhoto,
    bio :: Maybe String,
    description :: Maybe String,
    inviteLink :: Maybe String
  }

data ChatPhoto = ChatPhoto
  { smallFileId :: String,
    smallFileUniqueId :: String,
    bigFileId :: String,
    bigFileUniqueId :: String
  }

instance FromJSON ChatPhoto where
  parseJSON = withObject "ChatPhoto" $ \v ->
    ChatPhoto
      <$> v .: "small-file_id"
      <*> v .: "small_file_unique_id"
      <*> v .: "big_file_id"
      <*> v .: "big_file_unique_id"
