{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ShippingQuery (testShippingQuery) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.ShippingAddress (ShippingAddress)
import Telegram.Types.ShippingQuery (ShippingQuery)
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
import Test.Telegram.Types.ShippingAddress
import Test.Telegram.Types.User

instance Arbitrary ShippingQuery where
  arbitrary = genericArbitraryU

instance JSONTestable ShippingQuery

allKeys =
  sort
    [ "id",
      "from",
      "invoice_payload",
      "shipping_address"
    ]

objectWithAllKeys :: IO ShippingQuery
objectWithAllKeys = generate $ arbitrary

testShippingQuery :: Spec
testShippingQuery = do
  describe "Test ShippingQuery JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty ShippingQuery)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
