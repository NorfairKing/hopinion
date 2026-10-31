module BadSpec (spec) where

import qualified Data.Text as Text
import Test.Syd

-- The code under test. A real test imports it from the project; without a
-- build, only what a module defines itself is known to be the project's.
data User = User {userName :: Text, userAge :: Int}

renderUser :: User -> Text
renderUser = renderUser

renderAge :: Int -> Text
renderAge = renderAge

renderName :: Text -> Text
renderName = renderName

renderMaybe :: Maybe Text -> Text
renderMaybe = renderMaybe

spec :: Spec
spec = do
  it "renders a user built from made-up values" $
    renderUser User {userName = "alice", userAge = 30} `shouldBe` "alice (30)"
  it "renders an age worked out from literals by library code" $
    renderAge (20 + 1) `shouldSatisfy` not . Text.null
  it "renders a name a library function made" $
    renderName (Text.pack "bob") `shouldSatisfy` not . Text.null
  it "renders a value a library constructor holds" $
    renderMaybe (Just "carol") `shouldSatisfy` not . Text.null
  describe "a binding outside the test" $ do
    let name = "dave"
    it "renders a name bound for every test" $
      renderName name `shouldSatisfy` not . Text.null
  withTimeout 1000 $
    it "renders a made-up name within a timeout" $
      renderName "erin" `shouldSatisfy` not . Text.null
  it "renders a made-up name bound inside the test" $ do
    let frank = "frank"
    renderName frank `shouldSatisfy` not . Text.null
  describe "a helper of this file's own" $ do
    let mk name = User {userName = name, userAge = 30}
    let renders user = renderUser user `shouldSatisfy` not . Text.null
    it "renders a user a helper builds from a made-up name" $
      renderUser (mk "gina") `shouldSatisfy` not . Text.null
    it "hands a helper a made-up value it gives the code under test" $
      renders User {userName = "hal", userAge = 2}
