{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Video (testVideo) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.PhotoSize
import Telegram.Types.Video
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

instance Arbitrary Video where
  arbitrary = genericArbitraryU

instance JSONTestable Video

allKeys =
  sort
    [ "file_id",
      "file_unique_id",
      "width",
      "height",
      "duration",
      "thumb",
      "file_name",
      "mime_type",
      "file_size"
    ]

generators ::
  Gen (Maybe Integer)
    :+ Gen (Maybe String)
    :+ Gen (Maybe PhotoSize)
generators =
  genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO Video
objectWithAllKeys = generate $ genericArbitraryUG generators

testVideo :: Spec
testVideo = do
  describe "Test Video JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty Video)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
