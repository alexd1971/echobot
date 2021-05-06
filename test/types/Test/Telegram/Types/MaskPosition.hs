{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.MaskPosition (testMaskPosition) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.MaskPosition
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary MaskPosition where
  arbitrary = genericArbitraryU

instance JSONTestable MaskPosition

allKeys =
  sort
    [ "point",
      "x_shift",
      "y_shift",
      "scale"
    ]

objectWithAllKeys :: IO MaskPosition
objectWithAllKeys = generate arbitrary

testMaskPosition :: Spec
testMaskPosition = do
  describe "MaskPosition" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty MaskPosition)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
