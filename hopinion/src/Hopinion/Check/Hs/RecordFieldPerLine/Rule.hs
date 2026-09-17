{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.RecordFieldPerLine.Rule (rule) where

import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Check.Hs.RecordFieldPerLine.Fact
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsRecordFieldPerLine",
      ruleText = "Write a record value with more than one field a field per line.",
      ruleWhy =
        "A field per line makes changing one field a one-line diff, so review\
        \ sees the field that changed rather than the whole value. Adding a\
        \ field to a value written on one line rewrites that line and reflows\
        \ whatever followed. A value with one field has nothing to line up\
        \ against, so it is left alone.",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = crowdedRecordScope cr,
          findingSpan = crowdedRecordSpan cr,
          findingMessage = messageFor (crowdedRecordUse cr) (crowdedRecordFields cr)
        }
    | cr <- moduleContextCrowdedRecords mf
    ]

messageFor :: RecordUse -> Word -> Text
messageFor use fields =
  let subject :: Text
      subject = case use of
        RecordConstructed -> "A record value"
        RecordUpdated -> "A record update"
   in T.concat [subject, " of ", T.pack (show fields), " fields, sharing a line."]
