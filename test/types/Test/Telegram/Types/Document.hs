{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Document (testDocument) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.Document
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

instance Arbitrary Document where
  arbitrary = genericArbitraryU

instance JSONTestable Document

allKeys =
  sort
    [ "file_id",
      "file_unique_id",
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

objectWithAllKeys :: IO Document
objectWithAllKeys = generate $ genericArbitraryUG generators

testDocument :: Spec
testDocument = do
  describe "Document" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty Document)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
