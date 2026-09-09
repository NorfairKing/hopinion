{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.LambdaCase.Rule (rule) where

import Data.Text (Text)
import Hopinion.Check.Hs.LambdaCase.Fact
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsLambdaCase",
      ruleText = "Use \\case rather than naming an argument to take it apart.",
      ruleWhy =
        "A \\case definition writes the function's name once; equations write it\
        \ once each, so renaming the function reformats every equation and\
        \ buries the real change in the diff.",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = casedArgumentScope ca,
          findingSpan = casedArgumentSpan ca,
          findingMessage = messageFor (casedArgumentShape ca)
        }
    | ca <- moduleContextCasedArguments mf
    ]

messageFor :: ArgumentShape -> Text
messageFor = \case
  ArgumentNamedThenCased -> "This names its last argument and then only cases on it."
  ArgumentSplitOverEquations -> "This is spread over one equation per pattern."
