module GoodSpec (spec) where

import Test.Syd

spec :: Spec
spec = it "adds" (1 + 1 `shouldBe` (2 :: Int))
