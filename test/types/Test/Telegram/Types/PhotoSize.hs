{-# LANGUAGE OverloadedStrings #-}

module Test.Telegram.Types.PhotoSize (testPhotoSize) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random (genericArbitraryU, genericArbitraryUG)
import Telegram.Types.PhotoSize (PhotoSize)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary PhotoSize where
  arbitrary = genericArbitraryU

instance JSONTestable PhotoSize

allKeys =
  sort
    [ "file_id",
      "height",
      "width",
      "file_unique_id",
      "file_size"
    ]

generators :: Gen (Maybe Integer)
generators = genAlwaysJust

objectWithAllKeys :: IO PhotoSize
objectWithAllKeys = generate $ genericArbitraryUG generators

testPhotoSize :: Spec
testPhotoSize = do
  describe "Test PhotoSize JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty PhotoSize)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
