{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Update (testUpdate) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.CallbackQuery (CallbackQuery)
import Telegram.Types.ChatMemberUpdated (ChatMemberUpdated)
import Telegram.Types.ChatMessage (Message)
import Telegram.Types.ChosenInlineResult (ChosenInlineResult)
import Telegram.Types.InlineQuery (InlineQuery)
import Telegram.Types.Poll (Poll)
import Telegram.Types.PollAnswer (PollAnswer)
import Telegram.Types.PreCheckoutQuery (PreCheckoutQuery)
import Telegram.Types.ShippingQuery (ShippingQuery)
import Telegram.Types.Update
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.CallbackQuery
import Test.Telegram.Types.ChatMemberUpdated
import Test.Telegram.Types.ChatMessage
import Test.Telegram.Types.ChosenInlineResult
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.InlineQuery
import Test.Telegram.Types.Poll
import Test.Telegram.Types.PollAnswer
import Test.Telegram.Types.PreCheckoutQuery
import Test.Telegram.Types.ShippingQuery

instance Arbitrary Update where
  arbitrary = genericArbitraryU

instance JSONTestable Update

allKeys =
  sort
    [ "update_id",
      "message",
      "edited_message",
      "channel_post",
      "edited_channel_post",
      "inline_query",
      "chosen_inline_result",
      "callback_query",
      "shipping_query",
      "pre_checkout_query",
      "poll",
      "poll_answer",
      "my_chat_member",
      "chat_member"
    ]

generators ::
  Gen (Maybe Message)
    :+ Gen (Maybe CallbackQuery)
    :+ Gen (Maybe ChatMemberUpdated)
    :+ Gen (Maybe ChosenInlineResult)
    :+ Gen (Maybe InlineQuery)
    :+ Gen (Maybe Poll)
    :+ Gen (Maybe PollAnswer)
    :+ Gen (Maybe PreCheckoutQuery)
    :+ Gen (Maybe ShippingQuery)
generators =
  genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO Update
objectWithAllKeys = generate $ genericArbitraryUG generators

testUpdate :: Spec
testUpdate = do
  describe "Update" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty Update)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
