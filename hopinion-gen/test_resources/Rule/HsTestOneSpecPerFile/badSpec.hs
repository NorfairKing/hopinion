module BadSpec (spec, resourceDir) where

import Test.Syd

spec :: Spec
spec = it "reads its resources" (resourceDir `shouldBe` "test_resources")

resourceDir :: String
resourceDir = "test_resources"
