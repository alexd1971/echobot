{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ChatPermissions where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)

data ChatPermissions = ChatPermissions
  { canSendMessages :: Maybe Bool,
    canSendMediaMessages :: Maybe Bool,
    canSendPolls :: Maybe Bool,
    canSendOtherMessages :: Maybe Bool,
    canAddWebPagePreviews :: Maybe Bool,
    canChangeInfo :: Maybe Bool,
    canInviteUsers :: Maybe Bool,
    canPinMessages :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''ChatPermissions
 )
