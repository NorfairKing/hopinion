{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.NewtypeNotSynonym.Rule (rule) where

import qualified Data.Text as T
import Hopinion.Check.Hs.NewtypeNotSynonym.Extract (scalarSynonymsOf)
import Hopinion.Check.Hs.NewtypeNotSynonym.Fact
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNewtypeNotSynonym",
      ruleText = "A domain scalar is a newtype, not a type synonym.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> ParsedModule -> CheckResult
check mf parsed =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = scalarSynonymScope ss,
          findingSpan = scalarSynonymSpan ss,
          findingMessage =
            T.pack
              ( concat
                  [ unwords
                      [ "The type",
                        T.unpack (scalarSynonymName ss),
                        "is a synonym for",
                        T.unpack (scalarSynonymScalar ss)
                      ],
                    "."
                  ]
              )
        }
    | ss <- scalarSynonymsOf mf parsed
    ]
