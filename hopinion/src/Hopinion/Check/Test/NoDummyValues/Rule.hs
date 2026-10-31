{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Test.NoDummyValues.Rule (rule) where

import qualified Data.Text as T
import Hopinion.Check.Test.NoDummyValues.Fact
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "TestNoDummyValues",
      ruleText = "A test runs on generated values, not on literals made up for it.",
      ruleWhy =
        "A made-up value tests one input and says it tests all of them. \
        \A generator tests the claim the test is making, and finds the inputs \
        \nobody thought to write down. \
        \Generate the value with forAllValid or genValid instead. \
        \A literal can still be the point of a test, like a regression input or \
        \the edge case a bug was found at: suppress it and say which one it is.",
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> CheckResult
check ctx
  | not (moduleContextIsSpecFile ctx) = noResult
  | otherwise =
      findingsResult
        [ Finding
            { findingRule = ruleId rule,
              findingScope = testLiteralScope l,
              findingSpan = testLiteralSpan l,
              findingMessage = T.concat ["A literal in a test: ", testLiteralText l, "."]
            }
        | l <- moduleContextTestLiterals ctx
        ]
