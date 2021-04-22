{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Sticker (testSticker) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.MaskPosition
import Telegram.Types.PhotoSize
import Telegram.Types.Sticker
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.MaskPosition
import Test.Telegram.Types.PhotoSize

instance Arbitrary Sticker where
  arbitrary = genericArbitraryU

instance JSONTestable Sticker

allKeys =
  sort
    [ "file_id",
      "file_unique_id",
      "width",
      "height",
      "is_animated",
      "thumb",
      "emoji",
      "set_name",
      "mask_position",
      "file_size"
    ]

generators ::
  Gen (Maybe Integer)
    :+ Gen (Maybe String)
    :+ Gen (Maybe PhotoSize)
    :+ Gen (Maybe MaskPosition)
generators =
  genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO Sticker
objectWithAllKeys = generate $ genericArbitraryUG generators

testSticker :: Spec
testSticker = do
  describe "Test Sticker JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty Sticker)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
