-- | Names spelled the same in different scopes, which only a build can tell
-- apart.
module ShadowSpec (spec) where

import Stub

renderName :: String -> String
renderName = renderName

spec :: Spec
spec = do
  describe "a binding the code under test is handed" $ do
    let x = "erin"
    it "renders it" $ renderName x == ""
  describe "a binding spelled the same that is only expected" $ do
    let x = "frank"
    it "compares against it" $ "frank" == x
  describe "a helper that hands its argument on" $ do
    let check n = renderName n == ""
    it "checks a rendered name" $ check "gina"
  describe "a helper spelled the same that only compares" $ do
    let check n = n == "hal"
    it "checks a name" $ check "ivan"
  describe "a function a library hands the test" $
    it "hands it a literal" $ withCallback (\respond -> respond "jill")
