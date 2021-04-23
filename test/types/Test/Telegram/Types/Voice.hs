{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Voice (testVoice) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.Voice
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary Voice where
  arbitrary = genericArbitraryU

instance JSONTestable Voice

allKeys =
  sort
    [ "file_id",
      "file_unique_id",
      "duration",
      "mime_type",
      "file_size"
    ]

generators ::
  Gen (Maybe Integer)
    :+ Gen (Maybe String)
generators =
  genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO Voice
objectWithAllKeys = generate $ genericArbitraryUG generators

testVoice :: Spec
testVoice = do
  describe "Test Voice JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty Voice)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
