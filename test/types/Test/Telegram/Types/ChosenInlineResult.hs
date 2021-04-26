{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.ChosenInlineResult (testChosenInlineResult) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.ChosenInlineResult (ChosenInlineResult)
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

instance Arbitrary ChosenInlineResult where
  arbitrary = genericArbitraryU

instance JSONTestable ChosenInlineResult

allKeys =
  sort
    [ "result_id",
      "from",
      "location",
      "inline_message_id",
      "query"
    ]

generators :: Gen (Maybe String) :+ Gen (Maybe Location)
generators = genAlwaysJust :+ genAlwaysJust

objectWithAllKeys :: IO ChosenInlineResult
objectWithAllKeys = generate $ genericArbitraryUG generators

testChosenInlineResult :: Spec
testChosenInlineResult = do
  describe "Test ChosenInlineResult JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty ChosenInlineResult)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
