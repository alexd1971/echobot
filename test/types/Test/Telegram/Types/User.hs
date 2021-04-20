{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.User (testUser) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.User
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary User where
  arbitrary = genericArbitraryU

instance JSONTestable User

allKeys =
  sort
    [ "id",
      "is_bot",
      "first_name",
      "last_name",
      "username",
      "language_code",
      "can_join_groups",
      "can_read_all_group_messages",
      "supports_inline_queries"
    ]

generators ::
  Gen (Maybe Bool)
    :+ Gen (Maybe String)
generators =
  genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO User
objectWithAllKeys = generate $ genericArbitraryUG generators

testUser :: Spec
testUser = do
  describe "Test User JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty User)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
