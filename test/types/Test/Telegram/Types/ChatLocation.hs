{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ChatLocation (testChatLocation) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.ChatLocation (ChatLocation)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.Location

instance Arbitrary ChatLocation where
  arbitrary = genericArbitraryU

instance JSONTestable ChatLocation

allKeys =
  sort
    [ "location",
      "address"
    ]

objectWithAllKeys :: IO ChatLocation
objectWithAllKeys = generate arbitrary

testChatLocation :: Spec
testChatLocation = do
  describe "ChatLocation" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty ChatLocation)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
