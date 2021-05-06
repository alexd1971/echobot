{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.PollAnswer (testPollAnswer) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.PollAnswer (PollAnswer)
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

instance Arbitrary PollAnswer where
  arbitrary = genericArbitraryU

instance JSONTestable PollAnswer

allKeys =
  sort
    [ "poll_id",
      "user",
      "option_ids"
    ]

objectWithAllKeys :: IO PollAnswer
objectWithAllKeys = generate $ arbitrary

testPollAnswer :: Spec
testPollAnswer = do
  describe "PollAnswer" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty PollAnswer)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
