module BadSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  it "names its resources" (resourceDir `shouldBe` "test_resources")
  it "doubles" (twice 2 `shouldBe` 4)

resourceDir :: String
resourceDir = "test_resources"

twice :: Int -> Int
twice n = n + n
