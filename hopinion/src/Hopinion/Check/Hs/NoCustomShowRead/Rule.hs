{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.NoCustomShowRead.Rule (rule) where

import qualified Data.Text as T
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoCustomShowRead",
      ruleText = "Show and Read are derived, or their methods ignore the value.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = instanceFactScope inst,
          findingSpan = instanceFactSpan inst,
          findingMessage =
            T.concat
              [ instanceFactClass inst,
                " ",
                unTypeHead (instanceFactType inst),
                " is written out and uses the value."
              ]
        }
    | inst <- moduleContextInstances mf,
      instanceFactOrigin inst == InstanceOriginInstanceDecl InstanceMethodsUseArguments,
      instanceFactClass inst `elem` ["Show", "Read"]
    ]
