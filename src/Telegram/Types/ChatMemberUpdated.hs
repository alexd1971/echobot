{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ChatMemberUpdated where

import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    camelTo2,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Telegram.Types.ChatInviteLink (ChatInviteLink)
import Telegram.Types.ChatMember (ChatMember)
import Telegram.Types.ChatMessage (Chat)
import Telegram.Types.User (User)

data ChatMemberUpdated = ChatMemberUpdated
  { chat :: Chat,
    from :: User,
    date :: Integer,
    oldChatMember :: ChatMember,
    newChatMember :: ChatMember,
    inviteLink :: Maybe ChatInviteLink
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''ChatMemberUpdated
 )
