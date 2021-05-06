{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ProximityAlertTriggered (testProximityAlertTriggered) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.ProximityAlertTriggered
import Telegram.Types.User
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.User

instance Arbitrary ProximityAlertTriggered where
  arbitrary = genericArbitraryU

instance JSONTestable ProximityAlertTriggered

allKeys =
  sort
    [ "traveler",
      "watcher",
      "distance"
    ]

objectWithAllKeys :: IO ProximityAlertTriggered
objectWithAllKeys = generate arbitrary

testProximityAlertTriggered :: Spec
testProximityAlertTriggered = do
  describe "ProximityAlertTriggered" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty ProximityAlertTriggered)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
