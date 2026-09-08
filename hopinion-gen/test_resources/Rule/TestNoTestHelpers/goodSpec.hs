module GoodSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  let expected = "test_resources"
  it "names its resources" (expected `shouldBe` "test_resources")
  describe "a group" $ do
    let twice :: Int -> Int
        twice n = n + n
    it "doubles" (twice 2 `shouldBe` 4)
