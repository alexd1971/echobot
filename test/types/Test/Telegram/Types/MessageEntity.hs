{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.MessageEntity (testMessageEntity) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.MessageEntity
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

instance Arbitrary MessageEntity where
  arbitrary = genericArbitraryU

instance JSONTestable MessageEntity

allKeys =
  sort
    [ "type",
      "offset",
      "length",
      "url",
      "user",
      "language"
    ]

generators ::
  Gen (Maybe String)
    :+ Gen (Maybe User)
generators =
  genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO MessageEntity
objectWithAllKeys = generate $ genericArbitraryUG generators

testMessageEntity :: Spec
testMessageEntity = do
  describe "Test MessageEntity JSON" $ do
    prop "encode/decode" (propJSON :: JSONProperty MessageEntity)
    object <- runIO objectWithAllKeys
    it "correct key names encoding" $ objectKeys (toJSON object) `shouldBe` Just allKeys
