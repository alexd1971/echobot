{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.PassportData (testPassportData) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.PassportData
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.EncryptedCredentials
import Test.Telegram.Types.EncryptedPassportElement
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary PassportData where
  arbitrary = genericArbitraryU

instance JSONTestable PassportData

allKeys =
  sort
    [ "data",
      "credentials"
    ]

objectWithAllKeys :: IO PassportData
objectWithAllKeys = generate arbitrary

testPassportData :: Spec
testPassportData = do
  describe "PassportData" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty PassportData)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
