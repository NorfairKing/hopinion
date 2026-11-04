module Thing.Spec (thingSpec) where

import Test.Syd
import Thing (double)

thingSpec :: IO ()
thingSpec = sydTest (it "doubles" (double 2 `shouldBe` 4))
