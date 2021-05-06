{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Animation (testAnimation) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.Animation (Animation)
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
import Test.Telegram.Types.PhotoSize ()

instance Arbitrary Animation where
  arbitrary = genericArbitraryU

instance JSONTestable Animation

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

objectWithAllKeys :: IO Animation
objectWithAllKeys = generate $ genericArbitraryUG generators

testAnimation :: Spec
testAnimation = do
  describe "Animation" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty Animation)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
