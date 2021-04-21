{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.InlineKeyboardMarkup (testInlineKeyboardMarkup) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
  )
import Telegram.Types.InlineKeyboardButton
import Telegram.Types.InlineKeyboardMarkup
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.InlineKeyboardButton

instance Arbitrary InlineKeyboardMarkup where
  arbitrary = genericArbitraryU

instance JSONTestable InlineKeyboardMarkup

allKeys = sort ["inline_keyboard"]

generators :: Gen (Maybe [[InlineKeyboardButton]])
generators = genAlwaysJust

objectWithAllKeys :: IO InlineKeyboardMarkup
objectWithAllKeys = generate $ genericArbitraryUG generators

testInlineKeyboardMarkup :: Spec
testInlineKeyboardMarkup = do
  describe "Test InlineKeyboardMarkup JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty InlineKeyboardMarkup)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
