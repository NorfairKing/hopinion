module BadSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  it "renders a user" $
    renderUser User {userName = "alice", userAge = 30} `shouldBe` "alice (30)"
  it "parses a negative number" (parseNumber "-5" `shouldBe` Just (-5))
  describe "a group" $
    specify "counts characters" $ do
      let c = 'x'
      countChar c (replicate 3 c) `shouldBe` 3
  prop "adds an offset" $ \n ->
    n + 0.5 `shouldSatisfy` (> n)
