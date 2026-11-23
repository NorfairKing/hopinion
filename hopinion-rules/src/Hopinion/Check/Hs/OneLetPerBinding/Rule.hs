{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.OneLetPerBinding.Rule (rule) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs (GhcPs, LHsExpr)
import Hopinion.Check.Hs.OneLetPerBinding.Extract
import Hopinion.Check.Hs.OneLetPerBinding.Fact
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsOneLetPerBinding",
      ruleText = "Write one let statement per binding in a do-block.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
    }

check :: ModuleContext -> ParsedModule -> [LHsExpr GhcPs] -> CheckResult
check mf _ exprs =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = crowdedLetScope cl,
          findingSpan = crowdedLetSpan cl,
          findingMessage = messageFor (crowdedLetBindings cl)
        }
    | cl <- crowdedLetsOf mf exprs
    ]

messageFor :: Word -> Text
messageFor bindings =
  T.concat ["A let statement of ", T.pack (show bindings), " bindings."]
