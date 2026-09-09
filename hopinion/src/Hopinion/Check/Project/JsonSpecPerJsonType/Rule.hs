{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Check.Project.JsonSpecPerJsonType.Rule (rule) where

import Data.List.NonEmpty (NonEmpty (..))
import Hopinion.Check.Obligation
import Hopinion.Rule (Rule)
import Hopinion.Rule.Id

rule :: Rule
rule =
  obligationRule
    Obligation
      { obligationId = RuleId "TestJsonSpecPerJsonType",
        obligationText =
          "ToJSON and FromJSON oblige jsonSpec @T in the gen package's test suite.",
        obligationWhy =
          "Both instances mean something outside this program reads the type, and\
          \ the compiler accepts an encoder and a decoder that disagree. jsonSpec\
          \ asserts the roundtrip over generated values. The spec belongs to the\
          \ package that declares the instances.",
        obligationClasses = "ToJSON" :| ["FromJSON"],
        obligationCombinator = "jsonSpec"
      }
