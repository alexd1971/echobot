{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.ChosenInlineResult where

import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    camelTo2,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Telegram.Types.Location (Location)
import Telegram.Types.User (User)

data ChosenInlineResult = ChosenInlineResult
  { resultId :: String,
    from :: User,
    location :: Maybe Location,
    inlineMessageId :: Maybe String,
    query :: String
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = camelTo2 '_',
         omitNothingFields = True
       }
     ''ChosenInlineResult
 )
