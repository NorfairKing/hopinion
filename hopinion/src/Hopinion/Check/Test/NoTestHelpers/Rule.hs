{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Test.NoTestHelpers.Rule (rule) where

import qualified Data.Text as T
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
      ruleText = "A test file binds only spec at the top level.",
      ruleWhy =
        "A helper in a test file is untested code, private to that file, so a\
        \ test that calls it asserts whatever the helper happens to mean today.",
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
                T.concat
                  [ "A top-level binding beside spec: ",
                    declNameText (declFactName d),
                    "."
                  ]
            }
        | d <- moduleContextDecls ctx,
          declFactKind d == DeclValue,
          declFactName d /= specName
        ]

specName :: DeclName
specName = DeclName "spec"
