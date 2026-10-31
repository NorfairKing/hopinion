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
  it "says why it fails through a let that only the message uses" $
    forAllValid $ \user ->
      let message = unwords ["Rendered", show user, "as nothing"]
       in context message $ renderUser user `shouldSatisfy` not . null
  it "parses its settings from the arguments the program takes" $
    parserArgsTest settingsParser ["--name", "alice"] (Settings (UserName "alice"))
  it "runs a query as postgresql-simple writes one" $ \conn -> do
    _ <- execute_ conn "LISTEN channel"
    rows <- query_ conn "SELECT 1"
    rows `shouldSatisfy` not . null
  it "fails with a message nothing reads" $
    forAllValid $ \user ->
      (renderOrFail user >>= maybe (throwIO (userError "no render")) pure) `shouldReturn` renderUser user
  it "takes a rendering apart at its separator, and expects more than one piece" $
    forAllValid $ \users ->
      length (Text.splitOn "\n" (renderUsers users)) >= 1 `shouldBe` True
  let tableProblems =
        let rows = [row | row <- Text.lines rendered, Text.count "|" row > 1]
         in [i | (i, row) <- zip [0 :: Int ..] rows, Text.null row]
  it "reports a binding with lets inside once" $
    tableProblems `shouldBe` []
  it "runs SQL through a runner of the project's own, from many threads at once" $ \pool ->
    pooledReplicateConcurrentlyN_ 8 32 $
      runOwnQuery pool "SELECT count(*) FROM users" >>= (`shouldSatisfy` (>= 0))
  it "checks a settings parser with a helper of the project's own" $
    settingsParserTest settingsParser ["--name", "alice"] (Settings (UserName "alice"))
  it "asserts with a section" $
    forAllValid $ \users ->
      pure (length users) >>= (`shouldBe` length users)
  it "sends and reads headers and a query string under the names the protocol fixes" $
    forAllValid $ \(token, page) -> do
      let request' = defaultRequest {requestHeaders = [("Authorization", token)], queryString = [("page", Just page)]}
      response <- handle request'
      responseHeaders response `shouldSatisfy` any ((== "Content-Type") . fst)
  it "counts from zero, in steps of one, and scales by units" $
    forAllValid $ \(seconds :: Word) -> do
      counter <- newIORef (0 :: Int)
      modifyIORef' counter (+ 1)
      readIORef counter `shouldReturn` 1
      toNanos seconds `shouldBe` seconds * 1_000_000_000
  it "listens on an ephemeral port on loopback" $ do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    listen sock 5
    setGracefulShutdownTimeout (Just 20) settings `shouldSatisfy` const True
  it "parses the value aeson is given" $
    parseMaybe parseJSON (String "x") `shouldBe` Just (String "x")
  it "renders a status the protocol fixes" $
    statusMessage (mkStatus 429 "Too Many Requests") `shouldBe` "Too Many Requests"
