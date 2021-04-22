{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.OrderInfo (testOrderInfo) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.OrderInfo
import Telegram.Types.ShippingAddress
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.ShippingAddress

instance Arbitrary OrderInfo where
  arbitrary = genericArbitraryU

instance JSONTestable OrderInfo

allKeys =
  sort
    [ "name",
      "phone_number",
      "email",
      "shipping_address"
    ]

generators ::
  Gen (Maybe String)
    :+ Gen (Maybe ShippingAddress)
generators =
  genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO OrderInfo
objectWithAllKeys = generate $ genericArbitraryUG generators

testOrderInfo :: Spec
testOrderInfo = do
  describe "Test OrderInfo JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty OrderInfo)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
