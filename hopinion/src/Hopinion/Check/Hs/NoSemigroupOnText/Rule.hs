{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoSemigroupOnText.Rule (rule) where

import Hopinion.Check.Hs.NoSemigroupOnText.Fact
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoSemigroupOnText",
      ruleText = "Concatenate strings and text with a list, not with <> or ++.",
      ruleWhy = "A literal list of the pieces formats better than a <> chain.",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = concatChainScope cc,
          findingSpan = concatChainSpan cc,
          findingMessage = "A string literal concatenated with <> or ++."
        }
    | cc <- moduleContextConcatChains mf,
      OperandStringLiteral `elem` concatChainOperands cc
    ]
