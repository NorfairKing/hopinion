{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoCustomEq.Rule (rule) where

import qualified Data.Text as T
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoCustomEq",
      ruleText = "Eq is derived.",
      ruleWhy =
        "Custom equality belongs in a function, not in the instance that every\
        \ == goes through.",
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
            T.concat ["Eq ", typeHeadText (instanceFactType inst), " is written out."]
        }
    | inst <- moduleContextInstances mf,
      originIsWrittenOut (instanceFactOrigin inst),
      instanceFactClass inst == "Eq"
    ]
