{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoAnonymousScalar.Rule (rule) where

import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Check.Hs.NoAnonymousScalar.Fact
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoAnonymousScalar",
      ruleText = "A constructor of two or more fields holds no anonymous primitive.",
      ruleWhy =
        "A primitive a constructor holds is named by a record field, by a type\
        \ of its own, or by nothing at all, and the last leaves a reader\
        \ counting arguments to find out which one it is. A constructor of one\
        \ field needs none of that, because its name is that field's name. A\
        \ constructor of several names the whole of what it holds and no part\
        \ of it, so a primitive among them is named nowhere and two fields a\
        \ caller has the wrong way round still typecheck. Give each of them a\
        \ newtype, which is what the guide asks for, or write the constructor\
        \ with record syntax, which names them without asking for a type. Bool\
        \ is HsNoDomainBool's business, and the fields of a record constructor\
        \ are named already.",
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = anonymousScalarsScope as,
          findingSpan = anonymousScalarsSpan as,
          findingMessage =
            messageFor
              (anonymousScalarsConstructor as)
              (anonymousScalarsFields as)
              (anonymousScalarsTypes as)
        }
    | as <- moduleContextAnonymousScalars mf
    ]

messageFor :: Text -> Word -> [Text] -> Text
messageFor con fields = \case
  [t] ->
    T.concat
      [ "The constructor ",
        con,
        " holds ",
        T.pack (show fields),
        " fields, and nothing names its ",
        t,
        "."
      ]
  types ->
    T.concat
      [ "The constructor ",
        con,
        " holds ",
        T.pack (show fields),
        " fields, and nothing names ",
        T.intercalate ", " types,
        "."
      ]
