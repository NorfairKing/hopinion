module BadSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  it "renders a user" $
    renderUser User {userName = "alice", userAge = 30} `shouldBe` "alice (30)"
  it "takes the absolute value of a negative number" (absolute (-5) `shouldBe` 5)
  describe "a group" $
    specify "counts characters" $ do
      let c = 'x'
      countChar c (replicate 3 c) `shouldBe` 3
  prop "adds an offset" $ \n ->
    n + 0.5 `shouldSatisfy` (> n)
  it "renders a user of any age" $
    forAllValid $ \age ->
      renderUser User {userName = "bob", userAge = age} `shouldSatisfy` isInfixOf "bob"
  describe "bound outside the test" $ do
    let name = "carol"
    let user = User {userName = name, userAge = 40}
    it "renders a user set up for every test" $
      renderUser user `shouldSatisfy` not . null
  describe "bound to the result of an action" $ do
    limit <- runIO (pure 7)
    it "stays under a limit read at setup" $
      forAllValid $ \n -> min limit n `shouldSatisfy` (<= limit)
  modifyMaxSuccess (min 10) $
    it "runs fewer examples" $
      forAllValid $ \user ->
        renderUser user {userName = "dave"} `shouldSatisfy` not . null
  it "renders every user in a list" $
    map renderUserName ["erin"] `shouldSatisfy` not . null
