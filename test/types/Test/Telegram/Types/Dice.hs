{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Dice (testDice) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.Dice
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary Dice where
  arbitrary = genericArbitraryU

instance JSONTestable Dice

allKeys =
  sort
    [ "emoji",
      "value"
    ]

objectWithAllKeys :: IO Dice
objectWithAllKeys = generate arbitrary

testDice :: Spec
testDice = do
  describe "Test Dice JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty Dice)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
