{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hopinion.Check.Project.JsonSpecPerJsonType.Rule (rule) where

import Data.List.NonEmpty (NonEmpty (..))
import Hopinion.Check.Obligation
import Hopinion.Rule (Rule)
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  obligationRule
    Obligation
      { obligationId = RuleId "TestJsonSpecPerJsonType",
        obligationText =
          "ToJSON and FromJSON oblige jsonSpec @T in the gen package's test suite.",
        obligationWhy = $(proseBeside "why.txt"),
        obligationFix = $(proseBeside "fix.txt"),
        obligationClasses = "ToJSON" :| ["FromJSON"],
        obligationCombinator = "jsonSpec"
      }
