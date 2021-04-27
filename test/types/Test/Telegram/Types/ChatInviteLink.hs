{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ChatInviteLink (testChatInviteLink) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.ChatInviteLink (ChatInviteLink)
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

instance Arbitrary ChatInviteLink where
  arbitrary = genericArbitraryU

instance JSONTestable ChatInviteLink

allKeys =
  sort
    [ "invite_link",
      "creator",
      "is_primary",
      "is_revoked",
      "expire_date",
      "member_limit"
    ]

generators :: Gen (Maybe Integer)
generators = genAlwaysJust

objectWithAllKeys :: IO ChatInviteLink
objectWithAllKeys = generate $ genericArbitraryUG generators

testChatInviteLink :: Spec
testChatInviteLink = do
  describe "Test ChatInviteLink JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty ChatInviteLink)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
