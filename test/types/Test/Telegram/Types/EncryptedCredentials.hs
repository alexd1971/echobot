{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.EncryptedCredentials (testEncryptedCredentials) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.EncryptedCredentials
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary EncryptedCredentials where
  arbitrary = genericArbitraryU

instance JSONTestable EncryptedCredentials

allKeys =
  sort
    [ "data",
      "hash",
      "secret"
    ]

objectWithAllKeys :: IO EncryptedCredentials
objectWithAllKeys = generate arbitrary

testEncryptedCredentials :: Spec
testEncryptedCredentials = do
  describe "Test EncryptedCredentials JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty EncryptedCredentials)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
