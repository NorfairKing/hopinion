{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.StrictFields.Rule (rule) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs (GhcPs, HsDataDefn)
import Hopinion.Check.Hs.StrictFields.Extract
import Hopinion.Check.Hs.StrictFields.Fact
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsStrictFields",
      ruleText = "Make every field of a data type strict.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckOverNodes check)
    }

check :: ModuleContext -> ParsedModule -> [HsDataDefn GhcPs] -> CheckResult
check mf parsed defns =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = lazyFieldScope lf,
          findingSpan = lazyFieldSpan lf,
          findingMessage = messageFor (lazyFieldName lf)
        }
    | lf <- lazyFieldsOf mf parsed defns
    ]

messageFor :: LazyFieldName -> Text
messageFor = \case
  LazyFieldNameRecord (name :| []) ->
    T.concat ["The field ", name, " is lazy."]
  LazyFieldNameRecord names ->
    T.concat ["The fields ", T.intercalate ", " (NE.toList names), " are lazy."]
  LazyFieldNamePositional constructor position ->
    T.concat ["Field ", T.pack (show position), " of ", constructor, " is lazy."]
