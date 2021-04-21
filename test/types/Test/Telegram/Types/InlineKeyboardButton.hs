{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.InlineKeyboardButton (testInlineKeyboardButton) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.InlineKeyboardButton
import Telegram.Types.LoginUrl
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.LoginUrl

instance Arbitrary InlineKeyboardButton where
  arbitrary = genericArbitraryU

instance Arbitrary CallbackGame where
  arbitrary = genericArbitraryU

instance JSONTestable InlineKeyboardButton

allKeys =
  sort
    [ "text",
      "url",
      "login_url",
      "callback_data",
      "switch_inline_query",
      "switch_inline_query_current_chat",
      "callback_game",
      "pay"
    ]

generators ::
  Gen (Maybe LoginUrl)
    :+ Gen (Maybe String)
    :+ Gen (Maybe CallbackGame)
    :+ Gen (Maybe Bool)
generators =
  genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO InlineKeyboardButton
objectWithAllKeys = generate $ genericArbitraryUG generators

testInlineKeyboardButton :: Spec
testInlineKeyboardButton = do
  describe "Test InlineKeyboardButton JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty InlineKeyboardButton)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
