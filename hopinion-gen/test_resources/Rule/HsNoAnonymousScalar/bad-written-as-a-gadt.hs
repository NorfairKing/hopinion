{-# LANGUAGE GADTs #-}

module Bad where

import Data.Text (Text)

-- The same constructor written the other way round, and reported for the same
-- reason: what the arrows hold is held in a position, and a caller counts
-- arguments to find out which is which.
data Rule where
  RuleUnbounded :: Text -> String -> Rule

-- One signature introducing two names is two constructors, and a reader meets
-- each of them on its own.
data Outcome where
  OutcomeRefused, OutcomeAccepted :: Text -> Text -> Outcome

-- A record written in the same syntax holds nothing in a position, which is
-- what keeps this one out of the findings below.
data Settled where
  Settled :: {settledHost :: Text, settledUser :: Text} -> Settled
