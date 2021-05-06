{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.InlineQuery (testInlineQuery) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.InlineQuery (InlineQuery)
import Telegram.Types.Location (Location)
import Telegram.Types.User (User)
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.Location
import Test.Telegram.Types.User

instance Arbitrary InlineQuery where
  arbitrary = genericArbitraryU

instance JSONTestable InlineQuery

allKeys =
  sort
    [ "id",
      "from",
      "location",
      "query",
      "offset"
    ]

generators :: Gen (Maybe Location)
generators = genAlwaysJust

objectWithAllKeys :: IO InlineQuery
objectWithAllKeys = generate $ genericArbitraryUG generators

testInlineQuery :: Spec
testInlineQuery = do
  describe "InlineQuery" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty InlineQuery)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
