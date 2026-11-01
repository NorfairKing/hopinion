module GoodNotATestFile (spec) where

import Test.Syd

data User = User {userName :: Text, userAge :: Int}

renderUser :: User -> Text
renderUser = renderUser

spec :: Spec
spec =
  it "is not in a test file" $
    renderUser User {userName = "alice", userAge = 30} `shouldBe` "alice (30)"
