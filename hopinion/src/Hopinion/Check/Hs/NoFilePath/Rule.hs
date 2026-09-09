{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoFilePath.Rule (rule) where

import Hopinion.Facts.Module
import Hopinion.Facts.Occurrence
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoFilePath",
      ruleText = "A path is a Path, not a FilePath.",
      ruleWhy =
        "FilePath is String, so it does not say file or directory, absolute or\
        \ relative, or path at all. Path says all three, so the mistakes stop\
        \ compiling. Converting at the edge with toFilePath is not this: what is\
        \ reported is the type in your own signatures.",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = nameFactScope nf,
          findingSpan = nameFactSpan nf,
          findingMessage = "The type FilePath."
        }
    | nf <- moduleContextNames mf,
      nameFactText nf == "FilePath"
    ]
