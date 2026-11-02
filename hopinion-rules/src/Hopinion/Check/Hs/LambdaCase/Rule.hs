{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.LambdaCase.Rule (rule) where

import Data.Text (Text)
import Hopinion.Check.Hs.LambdaCase.Extract
import Hopinion.Check.Hs.LambdaCase.Fact
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsLambdaCase",
      ruleText = "Use \\case rather than naming an argument to take it apart.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> ParsedModule -> CheckResult
check mf parsed =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = casedArgumentScope ca,
          findingSpan = casedArgumentSpan ca,
          findingMessage = messageFor (casedArgumentShape ca)
        }
    | ca <- casedArgumentsOf mf parsed
    ]

messageFor :: ArgumentShape -> Text
messageFor = \case
  ArgumentShapeNamedThenCased -> "This names its last argument and then only cases on it."
  ArgumentShapeSplitOverEquations -> "This is spread over one equation per pattern."
