{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Contact (testContact) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.Contact
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )

instance Arbitrary Contact where
  arbitrary = genericArbitraryU

instance JSONTestable Contact

allKeys =
  sort
    [ "phone_number",
      "first_name",
      "last_name",
      "user_id",
      "vcard"
    ]

generators ::
  Gen (Maybe Integer)
    :+ Gen (Maybe String)
generators =
  genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO Contact
objectWithAllKeys = generate $ genericArbitraryUG generators

testContact :: Spec
testContact = do
  describe "Contact" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty Contact)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
