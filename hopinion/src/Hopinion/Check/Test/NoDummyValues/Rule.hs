{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Test.NoDummyValues.Rule (rule) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Hopinion.Check.Test.NoDummyValues.Fact
import Hopinion.Facts.Module
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "TestNoDummyValues",
      ruleText = "A test runs on generated values, not on literals made up for it.",
      ruleWhy =
        "A made-up value tests one input and reads as if it tested all of them.\n\
        \The ideal replacement is a property test: quantify over the value with\n\
        \forAllValid, so the test states the claim it is making and the\n\
        \generator finds the inputs nobody thought to write down.\n\
        \Binding the literal somewhere else does not make it less made up, so a\n\
        \value is followed into every binding of this module the test uses.\n\
        \When the literals are the point of the test, such as a regression input\n\
        \or the edge case a bug was found at, suppress it and say why: one\n\
        \suppression above a test answers for every literal in it.",
      ruleImpl = RuleImplModule (ModuleCheckFromSource check)
    }

-- | One finding per test, or per binding outside every test, naming every
-- literal in it and pointing at the first.
--
-- Per test rather than per literal, because what a suppression says is why a
-- test is written with the values it has, and a test whose values are the
-- point usually has several of them.
check :: ModuleContext -> CheckResult
check ctx
  | not (moduleContextIsSpecFile ctx) = noResult
  | otherwise =
      findingsResult
        [ Finding
            { findingRule = ruleId rule,
              findingScope = testLiteralScope first,
              findingSpan = testLiteralSpan first,
              findingMessage =
                T.pack
                  ( concat
                      [ "Literals a test runs on: ",
                        T.unpack (T.intercalate ", " (map testLiteralText (NE.toList literals))),
                        "."
                      ]
                  )
            }
        | literals@(first :| _) <- ownedTogether (moduleContextTestLiterals ctx)
        ]

-- | The literals of each owner, in source order, owners in the order of their
-- first literal.
ownedTogether :: [TestLiteral] -> [NonEmpty TestLiteral]
ownedTogether ls =
  sortOn
    (testLiteralSpan . NE.head)
    [ NE.sortWith testLiteralSpan group
    | group <- M.elems (M.fromListWith NE.append [(testLiteralOwner l, l :| []) | l <- ls])
    ]
