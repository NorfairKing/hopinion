{-# LANGUAGE OverloadedStrings #-}

-- | The rule sets the specs and the example executable run with.
--
-- In the gen package because both the test suite and the example binary want
-- them, and because a rule written to demonstrate that a repository can write
-- one is test material rather than something to compile into the tool.
module Hopinion.Rule.Gen
  ( shippedRules,
    exampleRule,
    exampleRules,
  )
where

import Data.Either (fromRight)
import qualified Data.Text as T
import Hopinion.Comment
import Hopinion.Facts.Module
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

-- | A rule that ships with nothing, which is the point of it.
--
-- This is what a repository adding a rule of its own writes, and it is here so
-- that the example executable and the spec that watches the example executable
-- run are looking at one value rather than at two that have to be kept the same.
-- Deliberately about something no shipped rule is about, so that a run finding
-- it can only have found this.
exampleRule :: Rule
exampleRule =
  Rule
    { ruleId = RuleId "ExampleNoShouting",
      ruleText = "A comment in this repository is not written in capitals.",
      ruleWhy =
        "There is no standard about this and there does not need to be. It is\
        \ here to be a rule that hopinion does not ship, so that a repository\
        \ adding one of its own can be watched doing it.",
      ruleImpl = ModuleRule (FromSource check)
    }
  where
    check mf =
      findingsResult
        [ Finding
            { findingRule = ruleId exampleRule,
              findingScope = scopeOfComment (moduleContextRef mf) cf,
              findingSpan = commentFactSpan cf,
              findingMessage = "A comment in capitals."
            }
        | cf <- moduleContextComments mf,
          commentFactStyle cf /= StylePragma,
          isShouting (commentFactText cf)
        ]

    isShouting t =
      let letters = T.filter (\c -> c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z']) t
       in not (T.null letters) && T.all (`elem` ['A' .. 'Z']) letters

-- | What the example executable runs: everything hopinion ships, plus the one
-- the example repository wrote.
exampleRules :: [Rule]
exampleRules = builtinRules ++ [exampleRule]
