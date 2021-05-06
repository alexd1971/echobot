{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Invoice (testInvoice) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.Invoice
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary Invoice where
  arbitrary = genericArbitraryU

instance JSONTestable Invoice

allKeys =
  sort
    [ "title",
      "description",
      "start_parameter",
      "currency",
      "total_amount"
    ]

objectWithAllKeys :: IO Invoice
objectWithAllKeys = generate arbitrary

testInvoice :: Spec
testInvoice = do
  describe "Invoice" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty Invoice)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
