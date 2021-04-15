{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Animation where

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

animationKeys =
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

generators :: Gen (Maybe Integer) :+ Gen (Maybe String) :+ Gen (Maybe PhotoSize)
generators = genAlwaysJust :+ genAlwaysJust :+ genAlwaysJust

animationWithAllKeys :: IO Animation
animationWithAllKeys = generate $ genericArbitraryUG generators

testAnimation :: Spec
testAnimation = do
  describe "Test Animation JSON encode/decode" $
    modifyMaxSuccess (const 5) $ do
      prop "encode/decode" (propJSON :: JSONProperty Animation)
      object <- runIO animationWithAllKeys
      it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just animationKeys
