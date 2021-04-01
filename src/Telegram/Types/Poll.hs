module Telegram.Types.Poll where

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
