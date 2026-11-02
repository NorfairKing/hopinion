{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.NoSemigroupOnText.Rule (rule) where

import Hopinion.Check.Hs.NoSemigroupOnText.Extract
import Hopinion.Check.Hs.NoSemigroupOnText.Fact
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule)
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

check :: ModuleContext -> ParsedModule -> CheckResult
check mf parsed =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = concatChainScope cc,
          findingSpan = concatChainSpan cc,
          findingMessage = "A string literal concatenated with <> or ++."
        }
    | cc <- concatChainsOf mf parsed,
      ConcatOperandStringLiteral `elem` concatChainOperands cc
    ]
