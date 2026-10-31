module GoodSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  let fixture = User {userName = "set up outside every test", userAge = 30}
  describe "a description is not a value" $
    it "renders a user" $
      forAllValid $ \user ->
        renderUser user `shouldBe` concat [userName user, " (", show (userAge user), ")"]
  it "asserts what it expects" $ do
    length (renderUser fixture) `shouldBe` 30
    shouldBe (length (renderUser fixture)) 30
    renderUser fixture `shouldSatisfy` isInfixOf "outside"
  it "compares against a golden file" $
    pureGoldenTextFile "test_resources/user.txt" (renderUser fixture)
  it "fails with a message" $
    expectationFailure "this message is not a value the test runs on"
  it "generates within bounds" $
    forAll (choose (1, 10)) $ \n -> n `shouldSatisfy` (> 0)
  it "runs a program in a temporary directory" $
    withSystemTempDir "user-test" $ \dir ->
      readProcess "ormolu" ["--mode", "check"] (renderUser fixture) `shouldReturn` ""
  -- [allow:TestNoDummyValues] The edge case the renderer used to crash on.
  it "renders the empty name" $
    renderUser fixture {userName = ""} `shouldSatisfy` not . null
  it "suppresses one statement of several" $ do
    -- [allow:TestNoDummyValues] The input from the bug report.
    let user = fixture {userName = "Robert'); DROP TABLE users;--"}
    renderUser user `shouldSatisfy` not . null
