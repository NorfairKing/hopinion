{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.PrefixNaming.Rule (rule) where

import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Check.Hs.PrefixNaming.Fact
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsPrefixNaming",
      ruleText = "Start a constructor or field with the name of its own type.",
      ruleWhy =
        "A constructor and a field are in scope across the whole module that\
        \ declares them and across every module importing it, where the type\
        \ they belong to is not written beside them. The prefix is what says\
        \ which type a name at a use site is about, and it is what keeps two\
        \ types in one module from wanting the same field name. The whole of\
        \ the type's name, because an abbreviation is a second name for the\
        \ type that nothing keeps in step with the first.",
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
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
    | un <- moduleContextUnprefixedNames mf
    ]

messageFor :: NameRole -> Text -> TypeHead -> Text
messageFor role written th =
  let subject :: Text
      subject = case role of
        NameRoleConstructor -> "A constructor"
        NameRoleField -> "A field"
   in T.concat [subject, " of ", typeHeadText th, " called ", written, "."]
