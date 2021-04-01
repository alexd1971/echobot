module Telegram.Types.Voice where

data Voice = Voice
  { fileId :: String,
    fileUniqueId :: String,
    duration :: Integer,
    mimeType :: Maybe String,
    fileSize :: Maybe Integer
  }
