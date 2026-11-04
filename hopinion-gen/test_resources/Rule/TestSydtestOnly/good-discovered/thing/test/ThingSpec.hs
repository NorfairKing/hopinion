module ThingSpec (spec) where

import Test.Syd
import Thing (double)

spec :: Spec
spec = it "doubles" (double 2 `shouldBe` 4)
