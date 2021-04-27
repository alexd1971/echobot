{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ChatMember (testChatMember) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.ChatMember (ChatMember)
import Telegram.Types.User (User)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.User

instance Arbitrary ChatMember where
  arbitrary = genericArbitraryU

instance JSONTestable ChatMember

allKeys =
  sort
    [ "user",
      "status",
      "custom_title",
      "is_anonymous",
      "can_be_edited",
      "can_manage_chat",
      "can_post_messages",
      "can_edit_messages",
      "can_delete_messages",
      "can_manage_voice_chats",
      "can_restrict_members",
      "can_promote_members",
      "can_change_info",
      "can_invite_users",
      "can_pin_messages",
      "is_member",
      "can_send_messages",
      "can_send_media_messages",
      "can_send_polls",
      "can_send_other_messages",
      "can_add_web_page_previews",
      "until_date"
    ]

generators :: Gen (Maybe Integer) :+ Gen (Maybe String) :+ Gen (Maybe Bool)
generators = genAlwaysJust :+ genAlwaysJust :+ genAlwaysJust

objectWithAllKeys :: IO ChatMember
objectWithAllKeys = generate $ genericArbitraryUG generators

testChatMember :: Spec
testChatMember = do
  describe "Test ChatMember JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty ChatMember)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
