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
          "A GenValid instance creates an obligation: genValidSpec @T must be called\
          \ in the test suite of that package's gen package.",
        obligationWhy =
          "A generator nothing runs is a generator nobody has checked. genValid can\
          \ produce values the type's own Validity instance rejects, and\
          \ shrinkValid can shrink to them, and every property built on it inherits\
          \ that quietly: the failures it reports are about values the code was\
          \ never meant to see. genValidSpec asserts exactly the two things the\
          \ instance promises, and it is one line. The spec belongs to the package\
          \ that declares the instance, because a spec written next door disappears\
          \ the day that other package stops mentioning the type.",
        obligationClasses = "GenValid" :| [],
        obligationCombinator = "genValidSpec"
      }
