{-# LANGUAGE OverloadedStrings #-}

-- | [check:ref GenValidSpecPerGenValid]
module Hopinion.Check.Project.GenValidSpecPerGenValid.Rule (rule) where

import Data.List.NonEmpty (NonEmpty (..))
import Hopinion.Check.Obligation
import Hopinion.Rule (Rule)
import Hopinion.Rule.Id

rule :: Rule
rule =
  obligationRule
    Obligation
      { obligationId = RuleId "TestGenValidSpecPerGenValid",
        obligationText =
          "A GenValid instance obliges genValidSpec @T in the gen package's test\
          \ suite.",
        obligationWhy =
          "A generator nothing runs is one nobody has checked: genValid can produce\
          \ values the type's own Validity rejects, and every property built on it\
          \ inherits that. The spec belongs to the package that declares the\
          \ instance.",
        obligationClasses = "GenValid" :| [],
        obligationCombinator = "genValidSpec"
      }
