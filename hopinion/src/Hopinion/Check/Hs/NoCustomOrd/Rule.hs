{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoCustomOrd.Rule (rule) where

import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoCustomOrd",
      ruleText = "Ord is derived.",
      ruleWhy =
        "If you need a custom ordering operation, use a separate function, not\
        \ the Ord instance.",
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
            "This Ord instance is written out. Derive it instead, so that what\
            \ it orders by is the fields of the type."
        }
    | inst <- moduleContextInstances mf,
      originIsWrittenOut (instanceFactOrigin inst),
      instanceFactClass inst == "Ord"
    ]
