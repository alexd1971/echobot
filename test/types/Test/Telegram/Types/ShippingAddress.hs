{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ShippingAddress (testShippingAddress) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.ShippingAddress
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary ShippingAddress where
  arbitrary = genericArbitraryU

instance JSONTestable ShippingAddress

allKeys =
  sort
    [ "country_code",
      "state",
      "city",
      "street_line1",
      "street_line2",
      "post_code"
    ]

objectWithAllKeys :: IO ShippingAddress
objectWithAllKeys = generate arbitrary

testShippingAddress :: Spec
testShippingAddress = do
  describe "ShippingAddress" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty ShippingAddress)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
