{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.MessageAutoDeleteTimerChanged (testMessageAutoDeleteTimerChanged) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.MessageAutoDeleteTimerChanged
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary MessageAutoDeleteTimerChanged where
  arbitrary = genericArbitraryU

instance JSONTestable MessageAutoDeleteTimerChanged

allKeys = ["message_auto_delete_time"]

objectWithAllKeys :: IO MessageAutoDeleteTimerChanged
objectWithAllKeys = generate arbitrary

testMessageAutoDeleteTimerChanged :: Spec
testMessageAutoDeleteTimerChanged = do
  describe "MessageAutoDeleteTimerChanged" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty MessageAutoDeleteTimerChanged)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
