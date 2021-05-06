{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.EncryptedPassportElement (testEncryptedPassportElement) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.EncryptedPassportElement
import Telegram.Types.PassportFile
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.PassportFile

instance Arbitrary EncryptedPassportElement where
  arbitrary = genericArbitraryU

instance JSONTestable EncryptedPassportElement

allKeys =
  sort
    [ "type",
      "data",
      "phone_number",
      "email",
      "files",
      "front_side",
      "reverse_side",
      "selfie",
      "translation",
      "hash"
    ]

generators ::
    Gen (Maybe String)
    :+ Gen (Maybe PassportFile)
    :+ Gen (Maybe [PassportFile])
generators =
  genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO EncryptedPassportElement
objectWithAllKeys = generate $ genericArbitraryUG generators

testEncryptedPassportElement :: Spec
testEncryptedPassportElement = do
  describe "EncryptedPassportElement" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty EncryptedPassportElement)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
