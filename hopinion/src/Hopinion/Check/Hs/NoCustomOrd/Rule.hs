{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoCustomOrd.Rule (rule) where

import qualified Data.Text as T
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoCustomOrd",
      ruleText = "Ord is derived.",
      ruleWhy =
        "Custom ordering belongs in a function, not in the instance that every\
        \ sort, Map and Set goes through.",
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
            T.concat ["Ord ", typeHeadText (instanceFactType inst), " is written out."]
        }
    | inst <- moduleContextInstances mf,
      originIsWrittenOut (instanceFactOrigin inst),
      instanceFactClass inst == "Ord"
    ]
