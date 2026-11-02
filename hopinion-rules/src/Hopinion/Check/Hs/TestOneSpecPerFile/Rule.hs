{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.TestOneSpecPerFile.Rule (rule) where

import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Check.Hs.TestOneSpecPerFile.Extract (exportListOf)
import Hopinion.Check.Hs.TestOneSpecPerFile.Fact
import Hopinion.Facts.Module
import Hopinion.Facts.Place
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsTestOneSpecPerFile",
      ruleText = "A test file exports exactly (spec).",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> ParsedModule -> CheckResult
check ctx parsed
  | not (moduleContextIsSpecFile ctx) = noResult
  | otherwise = case exportListOf ctx parsed of
      ExportListNone ->
        finding (wholeFileSpan (moduleContextPath ctx)) "This test file has no export list."
      ExportList sp exported
        | exported == [specExport] -> noResult
        | otherwise -> finding sp (exportsInstead exported)
  where
    finding :: Span -> Text -> CheckResult
    finding sp message =
      findingsResult
        [ Finding
            { findingRule = ruleId rule,
              findingScope = ScopeKeyOfFile (moduleContextRef ctx),
              findingSpan = sp,
              findingMessage = message
            }
        ]

specExport :: Text
specExport = "spec"

exportsInstead :: [Text] -> Text
exportsInstead = \case
  [] -> "This test file exports nothing."
  exported ->
    T.pack
      ( unwords
          [ "This test file exports",
            concat [T.unpack (T.intercalate ", " exported), "."]
          ]
      )
