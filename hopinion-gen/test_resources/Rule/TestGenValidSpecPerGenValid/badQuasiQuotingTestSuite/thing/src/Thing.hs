module Thing where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)

-- Nothing calls the combinator for this type, and the quasiquote in the gen
-- package's test suite expands a body written out there, so it cannot be the
-- call that is missing.
data Visible = Visible
  deriving (Show, Generic)

instance Validity Visible

instance GenValid Visible
