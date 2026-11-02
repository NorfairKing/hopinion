{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.NoFilePath.Rule (rule) where

import Hopinion.Facts.Module
import Hopinion.Facts.Occurrence
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoFilePath",
      ruleText = "A path is a Path, not a FilePath.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> ParsedModule -> CheckResult
check mf _ =
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
