{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Location (testLocation) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.Location
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary Location where
  arbitrary = genericArbitraryU

instance JSONTestable Location

allKeys =
  sort
    [ "longitude",
      "latitude",
      "horizontal_accuracy",
      "live_period",
      "heading",
      "proximity_alert_radius"
    ]

generators ::
  Gen (Maybe Integer)
    :+ Gen (Maybe Float)
generators =
  genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO Location
objectWithAllKeys = generate $ genericArbitraryUG generators

testLocation :: Spec
testLocation = do
  describe "Test Location JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty Location)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
