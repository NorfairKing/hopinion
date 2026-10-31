module GoodSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  let subject = "the renderer"
  describe (unwords ["a description is not a value, even when", subject, "is bound outside"]) $
    it "renders a user" $
      forAllValid $ \user ->
        renderUser user `shouldBe` concat [userName user, " (", show (userAge user), ")"]
  let separator = " ("
  it "asserts what it expects, even when that is bound outside" $
    forAllValid $ \user -> do
      renderUser user `shouldBe` concat [userName user, separator, show (userAge user), ")"]
      shouldBe (length (renderUser user)) (length (show user))
      renderUser user `shouldSatisfy` isInfixOf "("
  it "compares against a golden file" $
    goldenTextFile "test_resources/user.txt" (renderUser <$> generate genValid)
  it "fails with a message" $
    expectationFailure "this message is not a value the test runs on"
  it "generates within bounds" $
    forAll (choose (1, 10)) $ \n -> n `shouldSatisfy` (> 0)
  it "runs a program in a temporary directory" $
    forAllValid $ \user ->
      withSystemTempDir "user-test" $ \dir ->
        readProcess "ormolu" ["--mode", "check"] (renderUser user) `shouldReturn` ""
  -- [allow:TestNoDummyValues] The edge case the renderer used to crash on.
  it "renders the empty name" $
    forAllValid $ \user ->
      renderUser user {userName = ""} `shouldSatisfy` not . null
  it "suppresses one statement of several" $
    forAllValid $ \user -> do
      -- [allow:TestNoDummyValues] The input from the bug report.
      let reported = user {userName = "Robert'); DROP TABLE users;--"}
      renderUser reported `shouldSatisfy` not . null
