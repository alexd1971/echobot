{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Update where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)
import Telegram.Types.CallbackQuery (CallbackQuery)
import Telegram.Types.ChatMemberUpdated (ChatMemberUpdated)
import Telegram.Types.ChatMessage (Message)
import Telegram.Types.ChosenInlineResult (ChosenInlineResult)
import Telegram.Types.InlineQuery (InlineQuery)
import Telegram.Types.Poll (Poll)
import Telegram.Types.PollAnswer (PollAnswer)
import Telegram.Types.PreCheckoutQuery (PreCheckoutQuery)
import Telegram.Types.ShippingQuery (ShippingQuery)

data Update = Update
  { updateId :: Integer,
    message :: Maybe Message,
    editedMessage :: Maybe Message,
    channelPost :: Maybe Message,
    editedChannelPost :: Maybe Message,
    inlineQuery :: Maybe InlineQuery,
    chosenInlineResult :: Maybe ChosenInlineResult,
    callbackQuery :: Maybe CallbackQuery,
    shippingQuery :: Maybe ShippingQuery,
    preCheckoutQuery :: Maybe PreCheckoutQuery,
    poll :: Maybe Poll,
    pollAnswer :: Maybe PollAnswer,
    myChatMember :: ChatMemberUpdated,
    chatMember :: ChatMemberUpdated
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''Update
 )
