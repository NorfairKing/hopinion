module GoodNotATestFile (spec) where

import Test.Syd

spec :: Spec
spec =
  it "is not in a test file" $
    renderUser User {userName = "alice", userAge = 30} `shouldBe` "alice (30)"
