{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Audio (testAudio) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.Audio
import Telegram.Types.PhotoSize
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.PhotoSize

instance Arbitrary Audio where
  arbitrary = genericArbitraryU

instance JSONTestable Audio

allKeys =
  sort
    [ "file_id",
      "file_unique_id",
      "duration",
      "performer",
      "title",
      "file_name",
      "mime_type",
      "file_size",
      "thumb"
    ]

generators :: Gen (Maybe Integer) :+ Gen (Maybe String) :+ Gen (Maybe PhotoSize)
generators = genAlwaysJust :+ genAlwaysJust :+ genAlwaysJust

objectWithAllKeys :: IO Audio
objectWithAllKeys = generate $ genericArbitraryUG generators

testAudio :: Spec
testAudio = do
  describe "Test Audio JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty Audio)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
