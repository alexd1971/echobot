{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.VideoNote (testVideoNote) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.PhotoSize
import Telegram.Types.VideoNote
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

instance Arbitrary VideoNote where
  arbitrary = genericArbitraryU

instance JSONTestable VideoNote

allKeys =
  sort
    [ "file_id",
      "file_unique_id",
      "length",
      "duration",
      "thumb",
      "file_size"
    ]

generators ::
  Gen (Maybe Integer)
    :+ Gen (Maybe PhotoSize)
generators =
  genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO VideoNote
objectWithAllKeys = generate $ genericArbitraryUG generators

testVideoNote :: Spec
testVideoNote = do
  describe "Test VideoNote JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty VideoNote)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
