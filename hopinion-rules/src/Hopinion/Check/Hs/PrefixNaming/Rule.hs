{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.PrefixNaming.Rule (rule) where

import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Check.Hs.PrefixNaming.Extract
import Hopinion.Check.Hs.PrefixNaming.Fact
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsPrefixNaming",
      ruleText = "Name a constructor or field after its type, and a newtype's field unExample.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> ParsedModule -> CheckResult
check mf parsed =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = unprefixedNameScope un,
          findingSpan = unprefixedNameSpan un,
          findingMessage =
            messageFor
              (unprefixedNameRole un)
              (unprefixedNameWritten un)
              (unprefixedNameType un)
        }
    | un <- unprefixedNamesOf mf parsed
    ]

-- | A newtype's field is told what to be called, because there is exactly one
-- name it may have. The other two are told what they are, because what is
-- wrong with them is the prefix and what follows it is theirs to choose.
messageFor :: NameRole -> Text -> TypeHead -> Text
messageFor role written th = case role of
  NameRoleConstructor ->
    T.concat ["A constructor of ", unTypeHead th, " called ", written, "."]
  NameRoleField ->
    T.concat ["A field of ", unTypeHead th, " called ", written, "."]
  NameRoleNewtypeField ->
    T.concat
      [ "The field of the newtype ",
        unTypeHead th,
        " called ",
        written,
        " rather than ",
        newtypeFieldName th,
        "."
      ]
