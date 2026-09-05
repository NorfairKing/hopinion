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
      ruleText = "Do not use <> or ++ to concatenate strings or text. Put the pieces in a list.",
      ruleWhy = "<> does not format well, literal lists do.",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = concatChainScope cc,
          findingSpan = concatChainSpan cc,
          findingMessage = "A string literal concatenated with <> or ++. Put the pieces in a list and use concat, unwords or unlines."
        }
    | cc <- moduleContextConcatChains mf,
      OperandStringLiteral `elem` concatChainOperands cc
    ]
