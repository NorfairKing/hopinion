{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoCustomShowRead (rule) where

import qualified Data.Text as T
import Hopinion.Facts
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoCustomShowRead",
      ruleText = "Show and Read are derived, never written out.",
      ruleWhy =
        "A written Show is a second, undeclared serialisation of the type, and\
        \ the one every debugger, test failure and log line goes through. It\
        \ drifts from the type it prints without anything noticing, and a Read\
        \ written to match it drifts separately, so the pair stops round\
        \ tripping while still compiling. Derive them and the compiler keeps\
        \ them honest. Where the point is to hide something rather than to\
        \ print it, say so, because that is a decision about secrecy rather\
        \ than about formatting.",
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
                " instance is written out. Derive it instead."
              ]
        }
    | inst <- moduleContextInstances mf,
      instanceFactOrigin inst == OriginInstanceDecl,
      instanceFactClass inst `elem` ["Show", "Read"]
    ]
