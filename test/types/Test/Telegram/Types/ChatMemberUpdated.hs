{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ChatMemberUpdated (testChatMemberUpdated) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.ChatInviteLink (ChatInviteLink)
import Telegram.Types.ChatMemberUpdated (ChatMemberUpdated)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.ChatInviteLink
import Test.Telegram.Types.ChatMember
import Test.Telegram.Types.ChatMessage
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary ChatMemberUpdated where
  arbitrary = genericArbitraryU

instance JSONTestable ChatMemberUpdated

allKeys =
  sort
    [ "chat",
      "from",
      "date",
      "old_chat_member",
      "new_chat_member",
      "invite_link"
    ]

generators :: Gen (Maybe ChatInviteLink)
generators = genAlwaysJust

objectWithAllKeys :: IO ChatMemberUpdated
objectWithAllKeys = generate $ genericArbitraryUG generators

testChatMemberUpdated :: Spec
testChatMemberUpdated = do
  describe "Test ChatMemberUpdated JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty ChatMemberUpdated)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
