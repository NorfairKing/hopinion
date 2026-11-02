{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.NoSemigroupOnText.Rule (rule) where

import GHC.Hs (GhcPs, LHsExpr)
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
      ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
    }

check :: ModuleContext -> ParsedModule -> [LHsExpr GhcPs] -> CheckResult
check mf _ exprs =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = concatChainScope cc,
          findingSpan = concatChainSpan cc,
          findingMessage = "A string literal concatenated with <> or ++."
        }
    | cc <- concatChainsOf mf exprs,
      ConcatOperandStringLiteral `elem` concatChainOperands cc
    ]
