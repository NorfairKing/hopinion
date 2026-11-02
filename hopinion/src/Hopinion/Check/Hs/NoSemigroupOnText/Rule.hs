{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.NoSemigroupOnText.Rule (rule) where

import Hopinion.Check.Hs.NoSemigroupOnText.Fact
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoSemigroupOnText",
      ruleText = "Concatenate strings and text with a list, not with <> or ++.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
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
      ConcatOperandStringLiteral `elem` concatChainOperands cc
    ]
