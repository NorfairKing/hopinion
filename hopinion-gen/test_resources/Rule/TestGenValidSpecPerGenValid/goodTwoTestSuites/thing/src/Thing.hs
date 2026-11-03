module Thing where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)

-- Tested by one test suite's Main.
data A = A
  deriving (Show, Generic)

instance Validity A

instance GenValid A

-- Tested by the other's, which is a different module that GHC also calls Main.
data B = B
  deriving (Show, Generic)

instance Validity B

instance GenValid B
