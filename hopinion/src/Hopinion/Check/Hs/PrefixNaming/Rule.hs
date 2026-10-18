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
      ruleText = "Name a constructor or field after its type, and a newtype's field unExample.",
      ruleWhy =
        "A constructor and a field are in scope across the whole module that\
        \ declares them and across every module importing it, where the type\
        \ they belong to is not written beside them. The prefix is what says\
        \ which type a name at a use site is about, and it is what keeps two\
        \ types in one module from wanting the same field name. The whole of\
        \ the type's name, because an abbreviation is a second name for the\
        \ type that nothing keeps in step with the first. The one field of a\
        \ newtype is the exception: it has no sibling to be told apart from,\
        \ so a prefix would only repeat what the declaration says, and the\
        \ name says that applying it undoes the wrapper instead.",
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
