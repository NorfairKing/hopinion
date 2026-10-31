module GoodSpec (spec) where

import qualified Data.Text as Text
import Test.Syd
import Test.Syd.Validity

-- The code under test. A real test imports it from the project; without a
-- build, only what a module defines itself is known to be the project's.
data User = User {userName :: Text, userAge :: Int}

renderUser :: User -> Text
renderUser = renderUser

data Level = LevelError | LevelWarn

levelCode :: Level -> Int
levelCode = levelCode

withRenderer :: User -> IO () -> IO ()
withRenderer = withRenderer

spec :: Spec
spec = do
  describe "a description is not a value" $
    it "renders any user" $
      forAllValid $ \user ->
        renderUser user `shouldBe` Text.concat [userName user, " (", Text.pack (show (userAge user)), ")"]
  it "configures libraries with literals" $ do
    threadDelay 10
    Text.splitOn ":" "a:b" `shouldBe` ["a", "b"]
  it "compares against what it expects" $
    forAllValid $ \user ->
      Text.length (renderUser user) `shouldSatisfy` (> 3)
  it "compares against a golden file" $
    pureGoldenTextFile "test_resources/user.txt" "a fixed rendering"
  it "builds a request out of what the protocol fixes" $ \yc ->
    runYesodClientM yc $ do
      request $ do
        setMethod "POST"
        addRequestHeader ("Accept", "text/html")
      statusIs 200
  let message = "a message only a library is given"
  it "says why it fails with a binding only a library is given" $
    forAllValid $ \user ->
      context message $ renderUser user `shouldSatisfy` not . Text.null
  it "maps every constructor to the code the protocol gives it" $
    forAllValid $ \level ->
      ( \case
          LevelError -> 0
          LevelWarn -> 1
      )
        level
        `shouldBe` levelCode level
  -- [allow:TestNoDummyValues] The edge case the renderer used to crash on.
  it "renders the empty name" $
    renderUser User {userName = "", userAge = 0} `shouldSatisfy` not . Text.null
  it "suppresses one statement of several" $
    forAllValid $ \user -> do
      -- [allow:TestNoDummyValues] The input from the bug report.
      let reported = user {userName = "Robert'); DROP TABLE users;--"}
      renderUser reported `shouldSatisfy` not . Text.null
  it "expects a value built with the project's own constructor" $
    forAllValid $ \user ->
      user {userName = Text.empty} `shouldBe` User {userName = "", userAge = userAge user}
  it "configures a library inside a block handed to the project's code" $
    forAllValid $ \user ->
      withRenderer user $ do
        threadDelay 10
        setLocale "C"
