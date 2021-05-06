{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Venue (testVenue) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.Location
import Telegram.Types.Venue
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

instance Arbitrary Venue where
  arbitrary = genericArbitraryU

instance JSONTestable Venue

allKeys =
  sort
    [ "location",
      "title",
      "address",
      "foursquare_id",
      "foursquare_type",
      "google_place_id",
      "google_place_type"
    ]

generators :: Gen (Maybe String)
generators = genAlwaysJust

objectWithAllKeys :: IO Venue
objectWithAllKeys = generate $ genericArbitraryUG generators

testVenue :: Spec
testVenue = do
  describe "Venue" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty Venue)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
