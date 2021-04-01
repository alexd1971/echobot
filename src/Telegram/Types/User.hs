{-# LANGUAGE OverloadedStrings #-}

module Telegram.Types.User where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))

data User = User
  { userId :: Integer,
    isBot :: Bool,
    firstName :: String,
    lastName :: Maybe String,
    username :: Maybe String,
    languageCode :: Maybe String,
    canJoinGroups :: Maybe Bool,
    canReadAllGroupMessages :: Maybe Bool,
    supportsInlineQueries :: Maybe Bool
  }

instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User
      <$> v .: "id"
      <*> v .: "is_bot"
      <*> v .: "first_name"
      <*> v .:? "last_name"
      <*> v .:? "username"
      <*> v .:? "language_code"
      <*> v .:? "can_join_groups"
      <*> v .:? "can_read_all_group_messages"
      <*> v .:? "supports_inline_queries"
