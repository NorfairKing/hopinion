{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Hs.NoStringError.Rule (rule) where

import qualified Data.Text as T
import Hopinion.Check.Hs.NoStringError.Fact
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "HsNoStringError",
      ruleText = "An error is a type of its own, not a String.",
      ruleWhy =
        "A caller can branch on a constructor. It cannot branch on prose\
        \ without taking the message apart again, so a String error asks every\
        \ handler to treat every failure the same way. Writing the words at the\
        \ point of failure also fixes them there, which is the place that knows\
        \ least about who will read them. Wrap what a library hands you into a\
        \ type of your own at the edge, and render that type where the failure\
        \ is reported. Naming the prose does not make it a type: a synonym for\
        \ String is a String, and a wrapper around one has a single constructor,\
        \ which is still nothing to branch on.",
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

check :: ModuleContext -> CheckResult
check mf = slotFindings mf <> declFindings mf

-- | A failure written into a type that carries one.
slotFindings :: ModuleContext -> CheckResult
slotFindings mf =
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
    | es <- moduleContextErrorSlots mf,
      Just name <- [proseErrorName (errorSlotError es)]
    ]

-- | A failure given a name of its own and nothing else.
declFindings :: ModuleContext -> CheckResult
declFindings mf =
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
    | ed <- moduleContextErrorDecls mf,
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
