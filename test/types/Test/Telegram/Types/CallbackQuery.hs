{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.CallbackQuery (testCallbackQuery) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.CallbackQuery (CallbackQuery)
import Telegram.Types.ChatMessage (Message)
import Telegram.Types.User (User)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.ChatMessage
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.User

instance Arbitrary CallbackQuery where
  arbitrary = genericArbitraryU

instance JSONTestable CallbackQuery

allKeys =
  sort
    [ "id",
      "from",
      "message",
      "inline_message_id",
      "chat_instance",
      "data",
      "game_short_name"
    ]

generators :: Gen (Maybe String) :+ Gen (Maybe Message)
generators = genAlwaysJust :+ genAlwaysJust

objectWithAllKeys :: IO CallbackQuery
objectWithAllKeys = generate $ genericArbitraryUG generators

testCallbackQuery :: Spec
testCallbackQuery = do
  describe "Test CallbackQuery JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty CallbackQuery)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
