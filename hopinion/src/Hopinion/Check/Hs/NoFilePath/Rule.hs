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
        "Path says in the type whether it is a file or a directory, and whether\
        \ it is absolute or relative. FilePath is String, so those mistakes\
        \ compile. Converting at the edge with toFilePath is fine; this reports\
        \ the type in your own signatures.",
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
