{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The rule sets the specs and the example executable run with.
--
-- In the gen package because both the test suite and the example binary want
-- them, and because a rule written to demonstrate that a repository can write
-- one is test material rather than something to compile into the tool.
module Hopinion.Rule.Gen
  ( shippedRules,
    exampleRule,
    exampleMain,
  )
where

import Data.Either (fromRight)
import qualified Data.Text as T
import GHC.Parser.Lexer (Token (..))
import Hopinion (hopinionWith)
import Hopinion.Extract.Ghc (declScopeOf)
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule (..), SourceToken (..))
import Hopinion.Rule
import Hopinion.Rule.Id
import Hopinion.Rule.Registry (builtinRules)

-- | The shipped rules as a set, for the specs that run the tool.
--
-- Built once here rather than in every spec, because each of them needs one and
-- none of them is the place to decide what to do when the rules this repository
-- ships are not a set. Empty when they are not, which no run can reach: it is a
-- mistake in that list rather than in anything a run reads. What keeps the
-- fallback from being a suite that passes over no rules is that
-- 'Hopinion.RuleSpec' asserts this set is the rules that ship.
shippedRules :: RuleSet
shippedRules = fromRight emptyRuleSet (ruleSet builtinRules [])

-- | A rule the repository writes for itself, which is the point of it.
--
-- It is here so that the example executable and the spec that watches the
-- example executable run are looking at one value rather than at two that have
-- to be kept the same. Deliberately about something no shipped rule is about,
-- so that a run finding it can only have found this.
--
-- It reads the comments off the parse's tokens rather than off the shared
-- comment facts, so that the way an outside rule reaches the parse is a way
-- the end to end checks walk.
exampleRule :: Rule
exampleRule =
  Rule
    { ruleId = RuleId "ExampleNoShouting",
      ruleText = "A comment in this repository uses ordinary capitalisation.",
      ruleWhy =
        "This rule belongs to the repository, so that a repository adding a\
        \ rule of its own can be watched doing it. Its standard is invented.",
      ruleFix = "Write the comment in ordinary capitalisation.",
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }
  where
    check :: ModuleContext -> ParsedModule -> CheckResult
    check mf parsed =
      findingsResult
        [ Finding
            { findingRule = ruleId exampleRule,
              findingScope = declScopeOf (moduleContextRef mf) (moduleContextDecls mf) sp,
              findingSpan = sp,
              findingMessage = "A comment in capitals."
            }
        | SourceToken tok sp <- parsedModuleTokens parsed,
          Just text <- [commentText tok],
          isShouting (T.pack text)
        ]

    commentText :: Token -> Maybe String
    commentText = \case
      ITlineComment s _ -> Just s
      ITblockComment s _ -> Just s
      _ -> Nothing

    isShouting t =
      let letters = T.filter (\c -> c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z']) t
       in not (T.null letters) && T.all (`elem` ['A' .. 'Z']) letters

-- | What the example executable runs: everything hopinion ships, plus the one
-- the example repository wrote.
exampleMain :: IO ()
exampleMain = hopinionWith (builtinRules ++ [exampleRule])
