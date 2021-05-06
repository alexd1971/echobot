{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ChatPermissions (testChatPermissions) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.ChatPermissions
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary ChatPermissions where
  arbitrary = genericArbitraryU

instance JSONTestable ChatPermissions

allKeys =
  sort
    [ "can_send_messages",
      "can_send_media_messages",
      "can_send_polls",
      "can_send_other_messages",
      "can_add_web_page_previews",
      "can_change_info",
      "can_invite_users",
      "can_pin_messages"
    ]

generators :: Gen (Maybe Bool)
generators = genAlwaysJust

objectWithAllKeys :: IO ChatPermissions
objectWithAllKeys = generate $ genericArbitraryUG generators

testChatPermissions :: Spec
testChatPermissions = do
  describe "ChatPermissions" $ do
    prop "JSON-encode/decode" (propJSON :: JSONProperty ChatPermissions)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
