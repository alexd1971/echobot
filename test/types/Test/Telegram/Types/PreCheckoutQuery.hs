{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.PreCheckoutQuery (testPreCheckoutQuery) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.OrderInfo (OrderInfo)
import Telegram.Types.PreCheckoutQuery (PreCheckoutQuery)
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
import Test.Telegram.Types.OrderInfo
import Test.Telegram.Types.User

instance Arbitrary PreCheckoutQuery where
  arbitrary = genericArbitraryU

instance JSONTestable PreCheckoutQuery

allKeys =
  sort
    [ "id",
      "from",
      "currency",
      "total_amount",
      "invoice_payload",
      "shipping_option_id",
      "order_info"
    ]

generators :: Gen (Maybe String) :+ Gen (Maybe OrderInfo)
generators = genAlwaysJust :+ genAlwaysJust

objectWithAllKeys :: IO PreCheckoutQuery
objectWithAllKeys = generate $ genericArbitraryUG generators

testPreCheckoutQuery :: Spec
testPreCheckoutQuery = do
  describe "Test PreCheckoutQuery JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty PreCheckoutQuery)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
