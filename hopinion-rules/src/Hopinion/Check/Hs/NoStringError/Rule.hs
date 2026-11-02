{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Hs.NoStringError.Rule (rule) where

import qualified Data.Text as T
import Hopinion.Check.Hs.NoStringError.Extract
import Hopinion.Check.Hs.NoStringError.Fact
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule)
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoStringError",
      ruleText = "An error is a type of its own, not a String.",
      ruleWhy = $(proseBeside "why.txt"),
      ruleFix = $(proseBeside "fix.txt"),
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> ParsedModule -> CheckResult
check mf parsed = slotFindings mf parsed <> declFindings mf parsed

-- | A failure written into a type that carries one.
slotFindings :: ModuleContext -> ParsedModule -> CheckResult
slotFindings mf parsed =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = errorSlotScope es,
          findingSpan = errorSlotSpan es,
          findingMessage =
            sentence
              [ "The error type of",
                T.unpack (errorSlotCarrier es),
                "is",
                name
              ]
        }
    | es <- errorSlotsOf mf parsed,
      Just name <- [proseErrorName (errorSlotError es)]
    ]

-- | A failure given a name of its own and nothing else.
declFindings :: ModuleContext -> ParsedModule -> CheckResult
declFindings mf parsed =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = errorDeclScope ed,
          findingSpan = errorDeclSpan ed,
          findingMessage =
            sentence
              [ "The error type",
                T.unpack (errorDeclName ed),
                shapeSaying (errorDeclShape ed),
                name
              ]
        }
    | ed <- errorDeclsOf mf parsed,
      Just name <- [proseErrorName (errorDeclError ed)]
    ]

-- | How a declaration holds its prose, which is what a reader has to undo.
shapeSaying :: ErrorDeclShape -> String
shapeSaying = \case
  ErrorDeclShapeSynonym -> "is"
  ErrorDeclShapeWrapper -> "wraps nothing but"

sentence :: [String] -> T.Text
sentence ws = T.pack (concat [unwords ws, "."])

-- | What to call an error that is prose, and nothing at all for one that is
-- already a type.
proseErrorName :: ErrorType -> Maybe String
proseErrorName = \case
  ErrorTypeString -> Just "String"
  ErrorTypeText -> Just "Text"
  ErrorTypeSomethingElse -> Nothing
