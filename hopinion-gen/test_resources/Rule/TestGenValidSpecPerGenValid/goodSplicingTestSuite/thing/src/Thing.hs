module Thing where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)

-- Nothing written anywhere calls the combinator for this type. The splice in
-- the gen package's test suite does, which only what the compiler wrote down
-- about it can show.
data Visible = Visible
  deriving (Show, Generic)

instance Validity Visible

instance GenValid Visible
