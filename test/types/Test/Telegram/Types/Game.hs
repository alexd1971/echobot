{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Game (testGame) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.Animation
import Telegram.Types.Game
import Telegram.Types.MessageEntity
import Telegram.Types.PhotoSize
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.Animation
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.MessageEntity
import Test.Telegram.Types.PhotoSize

instance Arbitrary Game where
  arbitrary = genericArbitraryU

instance JSONTestable Game

allKeys =
  sort
    [ "title",
      "description",
      "photo",
      "text",
      "text_entities",
      "animation"
    ]

generators ::
  Gen (Maybe Animation)
    :+ Gen (Maybe String)
    :+ Gen (Maybe [MessageEntity])
generators =
  genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO Game
objectWithAllKeys = generate $ genericArbitraryUG generators

testGame :: Spec
testGame = do
  describe "Game" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty Game)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
