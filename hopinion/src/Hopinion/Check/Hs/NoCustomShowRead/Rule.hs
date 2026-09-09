{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoCustomShowRead.Rule (rule) where

import qualified Data.Text as T
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoCustomShowRead",
      ruleText = "Show and Read are derived, unless the methods ignore the value.",
      ruleWhy =
        "A written Show is an undeclared second serialisation, and nothing keeps\
        \ it, or a Read written to match it, in step with the type. An instance\
        \ that ignores the value has nothing to drift from, which is how a\
        \ secret stays out of the logs.",
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
            T.concat
              [ instanceFactClass inst,
                " ",
                typeHeadText (instanceFactType inst),
                " is written out, and uses the value."
              ]
        }
    | inst <- moduleContextInstances mf,
      instanceFactOrigin inst == OriginInstanceDecl MethodsUseArguments,
      instanceFactClass inst `elem` ["Show", "Read"]
    ]
