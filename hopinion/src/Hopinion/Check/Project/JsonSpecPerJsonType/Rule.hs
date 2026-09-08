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
          "A type with both a ToJSON and a FromJSON instance creates an obligation:\
          \ jsonSpec @T must be called in the test suite of that package's gen\
          \ package.",
        obligationWhy =
          "A type with both instances is a type something outside this program\
          \ reads or writes, and the two halves are what has to agree. Nothing in\
          \ the types makes them: a field renamed on one side, a constructor\
          \ spelled differently, an optional field that decodes to a different\
          \ default than it encoded from, and the type still compiles while the\
          \ data stops roundtripping. jsonSpec asserts that a value survives being\
          \ written and read back, over generated values rather than the two or\
          \ three anybody would think to write down, and it is one line. It\
          \ belongs to the package that declares the instances, because a test\
          \ written next door disappears the day that other package stops\
          \ mentioning the type.",
        obligationClasses = "ToJSON" :| ["FromJSON"],
        obligationCombinator = "jsonSpec"
      }
