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
        "A made-up value tests one input and reads as if it tested all of them.\n\
        \The ideal replacement is a property test: quantify over the value with\n\
        \forAllValid, so the test states the claim it is making and the\n\
        \generator finds the inputs nobody thought to write down.\n\
        \Binding the literal somewhere else does not make it less made up, so a\n\
        \value is followed into every binding of this module the test uses.\n\
        \When the literal is the point of the test, such as a regression input or\n\
        \the edge case a bug was found at, suppress it and say which one it is.",
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
