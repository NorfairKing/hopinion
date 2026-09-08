{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Test.NoTestHelpers.Rule (rule) where

import Hopinion.Facts.Decl
import Hopinion.Facts.Module
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "TestNoTestHelpers",
      ruleText = "A test file binds nothing at the top level but spec.",
      ruleWhy =
        "A helper in a test file is untested code nothing else can import, so a\
        \ test that calls it asserts whatever the helper happens to mean today:\
        \ duplicate it into the tests that use it, move it to the library if it\
        \ holds real logic, or put it in a TestUtils module if it builds a Spec.",
      ruleImpl = ModuleRule (FromSource check)
    }

-- | Every top-level binding a test file has beside spec.
--
-- A type signature is not one of these. It cannot move on its own, and the
-- binding it belongs to is reported already, so counting it would ask for two
-- suppressions where there is one thing to move. Nor is a type, a class or an
-- instance: this is about the helpers, and a test file that defines a type has
-- a different conversation to have.
check :: ModuleContext -> CheckResult
check ctx
  | not (moduleContextIsSpecFile ctx) = noResult
  | otherwise =
      findingsResult
        [ Finding
            { findingRule = ruleId rule,
              findingScope = ScopeOfDecl (moduleContextRef ctx) (declFactName d),
              findingSpan = declFactSpan d,
              findingMessage =
                "A top-level binding in a test file, beside spec."
            }
        | d <- moduleContextDecls ctx,
          declFactKind d == DeclValue,
          declFactName d /= specName
        ]

specName :: DeclName
specName = DeclName "spec"
