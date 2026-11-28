module Lonely where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)

-- A package with no gen package meets its obligations in its own test suite.
data Orphan = Orphan
  deriving (Show, Eq, Generic)

instance Validity Orphan

instance GenValid Orphan
