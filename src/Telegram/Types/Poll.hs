{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Poll where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import GHC.Generics (Generic)
import Telegram.Types.MessageEntity (MessageEntity)
import Telegram.Types.PollOption (PollOption)

data Poll = Poll
  { pollId :: String,
    question :: String,
    options :: [PollOption],
    totalVoterCount :: Integer,
    isClosed :: Bool,
    isAnonymous :: Bool,
    pollType :: String,
    allowsMultipleAnswers :: Bool,
    correctOptionId :: Maybe Integer,
    explanation :: Maybe String,
    explanationEntities :: Maybe [MessageEntity],
    openPeriod :: Maybe Integer,
    closeDate :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "pollId" -> "id"
           "pollType" -> "type"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''Poll
 )
