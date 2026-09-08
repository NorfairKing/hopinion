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
      ruleText = "Use \\case rather than naming an argument just to take it apart.",
      ruleWhy =
        "The name of a function is written once in a \\case definition and once\
        \ per equation in the other kind, so renaming the function reformats\
        \ every equation it has and the diff says nothing about what changed.\
        \ The argument's name is the same cost with nothing bought: a name that\
        \ appears twice and means nothing beyond the case it feeds.",
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
  ArgumentNamedThenCased ->
    "This names its last argument and then only cases on it. Drop the name and\
    \ write \\case."
  ArgumentSplitOverEquations ->
    "This is spread over one equation per pattern, each repeating the name of\
    \ the function. Write one equation ending in \\case."
