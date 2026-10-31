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
  it "renders a fixed user into a golden file, which has to be the same every run" $
    pureGoldenTextFile "test_resources/fixed-user.txt" $
      renderUser User {userName = "alice", userAge = 30}
  it "renders through a golden helper of the project's own" $
    explainGoldenTest pool "user/by-name" $ \now ->
      selectUserNamed now "alice"
  it "writes a table with a raw query" $
    forAllValid $ \pool -> do
      runSqlPool (rawExecute "CREATE TABLE users (name VARCHAR NOT NULL)" []) pool
      tables <- runSqlPool (rawSql "SELECT name FROM sqlite_master WHERE type = 'table'" []) pool
      tables `shouldSatisfy` not . null
  it "parses a specimen of the format" $
    parseDuration "2w" `shouldBe` Right (Duration 1209600)
  it "decodes a specimen of the format" $
    eitherDecode "{\"name\":\"alice\"}" `shouldBe` Right (Person "alice")
  it "parses every specimen in a list" $
    map parseState ["active", "inactive"] `shouldBe` [StateActive, StateInactive]
  it "parses every specimen in a list, mapped infix" $
    (parseState <$> ["active", "inactive"]) `shouldBe` [StateActive, StateInactive]
  it "submits a form and looks at the page it renders" $ \yc ->
    runYesodClientM yc $ do
      forAllValid' $ \owner -> do
        request $ do
          setMethod "POST"
          addPostParam "owner" owner
          addGetParam "page" (renderPage firstPage)
        rows <- htmlQuery "tbody tr"
        headers <- responseHeaders <$> requireResponse
        lookup "Content-Type" headers `shouldBe` Just "text/html"
        rows `shouldSatisfy` not . null
  it "retries a few times" $
    forAllValid $ \request' ->
      runWithRetries (limitRetries 3 <> capDelay 1_000 (constantDelay 10)) request' `shouldReturn` ()
  it "reads a value from under the keys the format fixes" $
    forAllValid $ \user ->
      userFromObject (object ["name" .= userName user, "age" .= userAge user]) `shouldBe` Just user
  it "numbers what it renders" $
    forAllValid $ \users ->
      map fst (zip [0 :: Int ..] (map renderUser users)) `shouldBe` take (length users) [0 ..]
  it "reads its settings from the environment and from a JSON object under the names the program fixes" $
    forAllValid $ \(user, url) -> do
      parserEnvTest settingsParser [("USER_NAME", userName user), ("BASE_URL", url)] (Settings user url)
      settingsFromObject (object [("name", toJSON (userName user)), ("url", toJSON url)]) `shouldBe` Just (Settings user url)
  it "says what it expects in a value named for it" $
    forAllValid $ \user ->
      runScenario
        Scenario
          { scenarioInput = user,
            scenarioExpected = ExpectedRender {expectedText = "(", expectedLines = 1}
          }
  it "goes one past a generated value" $
    forAllValid $ \users ->
      lookupUser (length users + 1) users `shouldBe` Nothing
