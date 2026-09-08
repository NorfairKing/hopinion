{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.TestOneSpecPerFile.Rule (rule) where

import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Facts.Export
import Hopinion.Facts.Module
import Hopinion.Facts.Place
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsTestOneSpecPerFile",
      ruleText = "A test file exports exactly (spec).",
      ruleWhy =
        "Nothing reads a test file but the Main that gathers the specs, and it\
        \ asks for spec alone, so any other export is a name no caller wants and\
        \ no export list at all exports whatever gets defined there next.",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check ctx
  | not (moduleContextIsSpecFile ctx) = noResult
  | otherwise = case moduleContextExports ctx of
      NoExportList ->
        finding
          (wholeFileSpan (moduleContextPath ctx))
          "This test file has no export list, so it exports every helper in it as well as spec."
      ExportList sp exported
        | exported == [specExport] -> noResult
        | otherwise -> finding sp (exportsInstead exported)
  where
    finding :: Span -> Text -> CheckResult
    finding sp message =
      findingsResult
        [ Finding
            { findingRule = ruleId rule,
              findingScope = ScopeOfFile (moduleContextRef ctx),
              findingSpan = sp,
              findingMessage = message
            }
        ]

specExport :: Text
specExport = "spec"

exportsInstead :: [Text] -> Text
exportsInstead = \case
  [] -> "This test file exports nothing, so nothing can run the tests in it."
  exported ->
    T.pack
      ( unwords
          [ "This test file exports",
            concat [T.unpack (T.intercalate ", " exported), "."]
          ]
      )
