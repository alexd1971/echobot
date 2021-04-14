module Test.Telegram.Types.General where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    Value,
    decode,
    encode,
    withObject,
  )
import Data.Aeson.Types (parseMaybe)
import Data.HashMap.Lazy (keys)
import Data.List (sort)
import Data.Text (Text)
import Generic.Random (genericArbitrary, (%))
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck (Arbitrary, Gen)

type JSONProperty a = a -> Expectation

class (FromJSON a, ToJSON a, Eq a, Show a) => JSONTestable a where
  propJSON :: JSONProperty a
  propJSON x = (decode . encode $ x) `shouldBe` Just x

genAlwaysJust :: Arbitrary a => Gen (Maybe a)
genAlwaysJust = genericArbitrary (0 % 1 % ())

objectKeys :: Value -> Maybe [Text]
objectKeys v = sort <$> parseMaybe (withObject "Object" (\o -> do return $ keys o)) v
