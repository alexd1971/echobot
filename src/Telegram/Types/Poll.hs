{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Types.Poll where

import Data.Aeson.TH
  ( Options (fieldLabelModifier, omitNothingFields),
    defaultOptions,
    deriveJSON,
  )
import Data.Aeson.Types (camelTo2)
import Telegram.Types.MessageEntity
import Telegram.Types.PollOption

data Poll = Poll
  { pollId :: String,
    question :: String,
    options :: [PollOption],
    totalVoterCount :: Integer,
    isClosed :: Bool,
    isAnonimous :: Bool,
    pollType :: String,
    allowsMultipleAnswers :: Bool,
    correctOptionId :: Maybe Integer,
    explanation :: Maybe String,
    explanationEntities :: Maybe [MessageEntity],
    openPeriod :: Maybe Integer,
    closeDate :: Maybe Integer
  }
  deriving (Show)

$( deriveJSON
     defaultOptions
       { fieldLabelModifier = \case
           "pollId" -> "id"
           a -> camelTo2 '_' a,
         omitNothingFields = True
       }
     ''Poll
 )
