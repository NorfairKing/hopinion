{-# LANGUAGE OverloadedStrings #-}

-- | [check:ref BareTodo]
module Hopinion.Check.Comment.BareTodo.Rule (rule) where

import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Comment
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "CommentBareTodo",
      ruleText = "A TODO names an issue or a URL.",
      ruleWhy =
        "A reference tells the reader where the work is tracked. A TODO on its\
        \ own reads like a plan, and goes stale in place.",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  findingsResult
    [ Finding
        { findingRule = ruleId rule,
          findingScope = scopeOfComment (moduleContextRef mf) cf,
          findingSpan = commentFactSpan cf,
          findingMessage = "A TODO with no issue reference or URL."
        }
    | cf <- moduleContextComments mf,
      commentFactStyle cf /= StylePragma,
      hasBareTodo (commentFactText cf)
    ]

-- | A block is bare when it carries the marker and no reference anywhere in the
-- block. Searching the whole block rather than the offending line is
-- deliberate: a reason spanning three lines is one comment, so a reference on
-- the next line still answers "where is this tracked".
hasBareTodo :: Text -> Bool
hasBareTodo t = elem "TODO" (tokens t) && not (hasReference t)

hasReference :: Text -> Bool
hasReference t =
  T.isInfixOf "http://" t
    || T.isInfixOf "https://" t
    || any isIssueNumber (T.words t)
    || any isTicket (tokens t)

-- | A hash and a number, as in @#412@, anywhere in a word.
isIssueNumber :: Text -> Bool
isIssueNumber w = case T.breakOn "#" w of
  (_, after) -> case T.uncons after of
    Just ('#', rest) -> not (T.null (T.takeWhile isDigit rest))
    _ -> False

-- | An upper-case project prefix, a dash and a number, as in @FOO-412@.
isTicket :: Text -> Bool
isTicket w =
  let (before, after) = T.breakOn "-" w
      digits = T.drop 1 after
   in T.length before >= 2
        && T.all isUpper before
        && not (T.null digits)
        && T.all isDigit digits

-- | Words, but keeping a dash inside a word so a ticket reference stays one
-- token.
tokens :: Text -> [Text]
tokens = T.split (\c -> not (isAlphaNum c || c == '_' || c == '-'))
