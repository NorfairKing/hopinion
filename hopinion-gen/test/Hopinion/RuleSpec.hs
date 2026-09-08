{-# LANGUAGE OverloadedStrings #-}

-- | One spec for every rule, so that adding a rule adds resources and no test
-- code, and one test for every resource, so that adding a case adds a file and
-- no test code either. It also carries the meta-properties, which therefore
-- cost nothing per rule.
module Hopinion.RuleSpec (spec) where

import Data.List (sort)
import qualified Data.Text as T
import Hopinion.Rule
import Hopinion.Rule.Gen (shippedRules)
import Hopinion.Rule.Id
import Hopinion.Rule.Registry (builtinRules)
import Hopinion.TestUtils (ruleResourcesDir, ruleSpec)
import Path (parseRelDir)
import Path.IO (listDirRel)
import Test.Syd
import Text.Colour (TerminalCapabilities (..), renderChunksText)

spec :: Spec
spec = do
  -- Two rules by one name, or a name that is not an id, is a value that fails
  -- to be a set, and this is where the rules that ship are held to being one.
  it "ships a set of rules" $
    case ruleSet builtinRules [] of
      Right _ -> pure ()
      Left err -> expectationFailure (T.unpack (renderChunksText WithoutColours (renderRuleSetError err)))

  -- And this is what stops the rest of the suite passing over no rules when they
  -- are not: shippedRules falls back to the empty set, which every spec below
  -- would otherwise read as a rule that found nothing.
  it "runs the specs below over exactly the rules that ship" $
    map ruleId (ruleSetRules shippedRules) `shouldBe` map ruleId builtinRules

  -- Both directions at once: a rule with no resources fails here, and so does a
  -- directory left behind by a renamed or deleted rule.
  it "has one resource directory per rule and no others" $ do
    (dirs, files) <- listDirRel ruleResourcesDir
    expected <-
      traverse
        (parseRelDir . T.unpack . ruleIdText . ruleId)
        (ruleSetRules shippedRules)
    sort dirs `shouldBe` sort expected
    files `shouldBe` []

  -- An id is what a suppression names, so it has to survive being written down
  -- and read back. Both directions, since a rule whose text no longer parses
  -- would take every suppression naming it with it.
  it "reads every rule id back from the text it is written as" $
    traverse (parseRuleId . ruleIdText . ruleId) (ruleSetRules shippedRules)
      `shouldBe` Just (map ruleId (ruleSetRules shippedRules))

  mapM_ ruleSpec (ruleSetRules shippedRules)
