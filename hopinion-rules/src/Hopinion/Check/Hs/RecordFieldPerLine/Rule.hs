{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.RecordFieldPerLine.Rule (rule) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs (GhcPs, LHsExpr)
import Hopinion.Check.Hs.RecordFieldPerLine.Extract
import Hopinion.Check.Hs.RecordFieldPerLine.Fact
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsRecordFieldPerLine",
      ruleText = "Write a record value with more than one field a field per line.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
    }

check :: ModuleContext -> ParsedModule -> [LHsExpr GhcPs] -> CheckResult
check mf _ exprs =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = crowdedRecordScope cr,
          findingSpan = crowdedRecordSpan cr,
          findingMessage = messageFor (crowdedRecordUse cr) (crowdedRecordFields cr)
        }
    | cr <- crowdedRecordsOf mf exprs
    ]

messageFor :: RecordUse -> Word -> Text
messageFor use fields =
  let subject :: Text
      subject = case use of
        RecordUseConstructed -> "A record value"
        RecordUseUpdated -> "A record update"
   in T.concat [subject, " of ", T.pack (show fields), " fields, sharing a line."]
