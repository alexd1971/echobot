{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ChatMember where

import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    camelTo2,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Telegram.Types.User (User)

data ChatMember = ChatMember
  { user :: User,
    status :: String,
    customTitle :: Maybe String,
    isAnonymous :: Maybe Bool,
    canBeEdited :: Maybe Bool,
    canManageChat :: Maybe Bool,
    canPostMessages :: Maybe Bool,
    canEditMessages :: Maybe Bool,
    canDeleteMessages :: Maybe Bool,
    canManageVoiceChats :: Maybe Bool,
    canRestrictMembers :: Maybe Bool,
    canPromoteMembers :: Maybe Bool,
    canChangeInfo :: Maybe Bool,
    canInviteUsers :: Maybe Bool,
    canPinMessages :: Maybe Bool,
    isMember :: Maybe Bool,
    canSendMessages :: Maybe Bool,
    canSendMediaMessages :: Maybe Bool,
    canSendPolls :: Maybe Bool,
    canSendOtherMessages :: Maybe Bool,
    canAddWebPagePreviews :: Maybe Bool,
    untilDate :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''ChatMember
 )
