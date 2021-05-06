{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.PollOption (testPollOption) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.PollOption
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary PollOption where
  arbitrary = genericArbitraryU

instance JSONTestable PollOption

allKeys =
  sort
    [ "text",
      "voter_count"
    ]

objectWithAllKeys :: IO PollOption
objectWithAllKeys = generate arbitrary

testPollOption :: Spec
testPollOption = do
  describe "PollOption" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty PollOption)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
