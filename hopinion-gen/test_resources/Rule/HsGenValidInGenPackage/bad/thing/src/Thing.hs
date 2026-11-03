module Thing where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)

data Widget = Widget
  deriving (Generic)

instance Validity Widget

instance GenValid Widget
