{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.PassportFile (testPassportFile) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.PassportFile
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary PassportFile where
  arbitrary = genericArbitraryU

instance JSONTestable PassportFile

allKeys =
  sort
    [ "file_id",
      "file_unique_id",
      "file_size",
      "file_date"
    ]

objectWithAllKeys :: IO PassportFile
objectWithAllKeys = generate arbitrary

testPassportFile :: Spec
testPassportFile = do
  describe "Test PassportFile JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty PassportFile)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
