{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config as C
import Data.Aeson
import Data.Aeson.Types (emptyObject, parseMaybe)
import Data.Text
import Test.Hspec

configJSON :: Value
configJSON =
  object
    [ "help_message" .= ("Help message" :: Text),
      "log_level" .= ("Info" :: String),
      "repeat"
        .= object
          [ "message" .= ("Repeat message" :: Text),
            "default_count" .= (3 :: Int)
          ],
      "telegram"
        .= object
          [ "api_url" .= ("https://telegram.api.org" :: Text),
            "api_key" .= ("apikey" :: Text)
          ]
    ]

main :: IO ()
main = hspec $ do
  describe "Config" $ do
    it "should decode from JSON" $
      parseMaybe parseJSON configJSON
        `shouldBe` Just
          ( Config
              { helpMessage = "Help message",
                C.repeat =
                  RepeatConf
                    { message = "Repeat message",
                      defaultCount = 3
                    },
                logLevel = "Info",
                telegram =
                  TelegramConf
                    { apiUrl = "https://telegram.api.org",
                      apiKey = "apikey"
                    }
              }
          )
    it "should have default values" $
      parseMaybe parseJSON emptyObject
        `shouldBe` Just
          ( Config
              { helpMessage = "Echo Bot",
                C.repeat =
                  RepeatConf
                    { message = "Set repeat count",
                      defaultCount = 1
                    },
                logLevel = "info",
                telegram =
                  TelegramConf
                    { apiUrl = "https://api.telegram.org",
                      apiKey = ""
                    }
              }
          )
