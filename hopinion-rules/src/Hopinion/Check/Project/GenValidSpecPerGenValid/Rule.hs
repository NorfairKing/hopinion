{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | [check:ref GenValidSpecPerGenValid]
module Hopinion.Check.Project.GenValidSpecPerGenValid.Rule (rule) where

import Data.List.NonEmpty (NonEmpty (..))
import Hopinion.Check.Obligation
import Hopinion.Rule (Rule)
import Hopinion.Rule.Id
import Hopinion.Rule.Prose (proseBeside)

rule :: Rule
rule =
  obligationRule
    Obligation
      { obligationId = RuleId "TestGenValidSpecPerGenValid",
        obligationText =
          "A GenValid instance obliges genValidSpec @T in the gen package's test\
          \ suite.",
        obligationWhy = $(proseBeside "why.txt"),
        obligationFix = $(proseBeside "fix.txt"),
        obligationClasses = "GenValid" :| [],
        obligationCombinator = "genValidSpec"
      }
