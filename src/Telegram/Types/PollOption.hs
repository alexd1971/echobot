module Telegram.Types.PollOption where

data PollOption = PollOption
  { text :: String,
    voterCount :: Integer
  }
