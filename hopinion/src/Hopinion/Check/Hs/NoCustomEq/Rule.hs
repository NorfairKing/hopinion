{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoCustomEq.Rule (rule) where

import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoCustomEq",
      ruleText = "Eq is derived.",
      ruleWhy =
        "If you need a custom equality operation, use a separate function, not\
        \ the Eq instance.",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = instanceFactScope inst,
          findingSpan = instanceFactSpan inst,
          findingMessage =
            "This Eq instance is written out. Derive it instead, so that what\
            \ it compares is the fields of the type."
        }
    | inst <- moduleContextInstances mf,
      originIsWrittenOut (instanceFactOrigin inst),
      instanceFactClass inst == "Eq"
    ]
