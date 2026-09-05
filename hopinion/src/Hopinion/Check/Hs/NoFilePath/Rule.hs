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
      ruleText = "Say what a path is with path's Path, rather than with FilePath.",
      ruleWhy =
        "A FilePath is a String, so it says nothing about what it holds: not\
        \ whether it is a file or a directory, not whether it is absolute or\
        \ relative, not whether it is a path at all. Every function taking one\
        \ has to document what it accepts and then trust its callers, and what\
        \ that invites goes wrong quietly: a directory passed where a file was\
        \ meant, a relative path resolved against a working directory nobody\
        \ checked, two paths joined into something that is neither. Path carries\
        \ all of it in the type and path-io is the same operations over it, so\
        \ the mistakes stop compiling.\
        \ Converting at the edge is not what this reports: a library that\
        \ demands a String wants toFilePath at the call, and that is correct.\
        \ What it reports is the type in your own signatures, where it could\
        \ have said what the path is.",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = nameFactScope nf,
          findingSpan = nameFactSpan nf,
          findingMessage = "The type FilePath. Say what the path is with Path from path."
        }
    | nf <- moduleContextNames mf,
      nameFactText nf == "FilePath"
    ]
