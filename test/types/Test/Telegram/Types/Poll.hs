{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Telegram.Types.Poll (testPoll) where

import Data.Aeson (ToJSON (toJSON))
import Data.List (sort)
import Generic.Random
  ( genericArbitraryU,
    genericArbitraryUG,
    type (:+) (..),
  )
import Telegram.Types.MessageEntity
import Telegram.Types.Poll
import Telegram.Types.PollOption
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, generate)
import Test.Telegram.Types.General
  ( JSONProperty,
    JSONTestable (..),
    genAlwaysJust,
    objectKeys,
  )
import Test.Telegram.Types.MessageEntity
import Test.Telegram.Types.PollOption

instance Arbitrary Poll where
  arbitrary = genericArbitraryU

instance JSONTestable Poll

allKeys =
  sort
    [ "id",
      "question",
      "options",
      "total_voter_count",
      "is_closed",
      "is_anonymous",
      "type",
      "allows_multiple_answers",
      "correct_option_id",
      "explanation",
      "explanation_entities",
      "open_period",
      "close_date"
    ]

generators ::
  Gen (Maybe Integer)
    :+ Gen (Maybe String)
    :+ Gen (Maybe [MessageEntity])
generators =
  genAlwaysJust
    :+ genAlwaysJust
    :+ genAlwaysJust

objectWithAllKeys :: IO Poll
objectWithAllKeys = generate $ genericArbitraryUG generators

testPoll :: Spec
testPoll = do
  describe "Poll" $ do
    prop "JSON encode/decode" (propJSON :: JSONProperty Poll)
    object <- runIO objectWithAllKeys
    it "has correct JSON-key names" $ objectKeys (toJSON object) `shouldBe` Just allKeys
