module Lonely where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)

data Orphan = Orphan
  deriving (Generic)

instance Validity Orphan

instance GenValid Orphan
