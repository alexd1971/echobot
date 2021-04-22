{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.SuccessfulPayment (testSuccessfulPayment) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.OrderInfo
import Telegram.Types.SuccessfulPayment
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.OrderInfo

instance Arbitrary SuccessfulPayment where
  arbitrary = genericArbitraryU

instance JSONTestable SuccessfulPayment

allKeys =
  sort
    [ "currency",
      "total_amount",
      "invoice_payload",
      "shipping_option_id",
      "order_info",
      "telegram_payment_charge_id",
      "provider_payment_charge_id"
    ]

generators ::
  Gen (Maybe String)
    :+ Gen (Maybe OrderInfo)
generators =
  genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO SuccessfulPayment
objectWithAllKeys = generate $ genericArbitraryUG generators

testSuccessfulPayment :: Spec
testSuccessfulPayment = do
  describe "Test SuccessfulPayment JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty SuccessfulPayment)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
