{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoCustomShowRead.Rule (rule) where

import qualified Data.Text as T
import Hopinion.Facts.Instance
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoCustomShowRead",
      ruleText = "Show and Read are derived, unless every method discards what it is given.",
      ruleWhy =
        "A written Show is a second, undeclared serialisation of the type, and\
        \ the one every debugger, test failure and log line goes through. It\
        \ drifts from the type it prints without anything noticing, and a Read\
        \ written to match it drifts separately, so the pair stops round\
        \ tripping while still compiling. Derive them and the compiler keeps\
        \ them honest. An instance whose methods discard their arguments is\
        \ none of that: what it produces cannot depend on the value, so there\
        \ is nothing for it to drift from, and keeping a secret out of every\
        \ log line is a good reason to write one.",
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
              [ "This ",
                instanceFactClass inst,
                " instance is written out. Derive it instead, or discard what\
                \ it is given if the point is to hide the value."
              ]
        }
    | inst <- moduleContextInstances mf,
      instanceFactOrigin inst == OriginInstanceDecl MethodsUseArguments,
      instanceFactClass inst `elem` ["Show", "Read"]
    ]
