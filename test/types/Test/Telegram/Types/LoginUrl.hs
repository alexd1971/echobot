{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.LoginUrl (testLoginUrl) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.LoginUrl
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary LoginUrl where
  arbitrary = genericArbitraryU

instance JSONTestable LoginUrl

allKeys =
  sort
    [ "url",
      "forward_text",
      "bot_username",
      "request_write_access"
    ]

generators ::
  Gen (Maybe String)
    :+ Gen (Maybe Bool)
generators =
  genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO LoginUrl
objectWithAllKeys = generate $ genericArbitraryUG generators

testLoginUrl :: Spec
testLoginUrl = do
  describe "LoginUrl" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty LoginUrl)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
