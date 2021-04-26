{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.InlineQuery where

import Data.Aeson
  ( Options (fieldLabelModifier, omitNothingFields),
    camelTo2,
    defaultOptions,
  )
import Data.Aeson.TH (deriveJSON)
import GHC.Generics (Generic)
import Telegram.Types.Location (Location)
import Telegram.Types.User (User)

data InlineQuery = InlineQuery
  { queryId :: String,
    from :: User,
    location :: Maybe Location,
    query :: String,
    offset :: String
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "queryId" -> "id"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''InlineQuery
 )
