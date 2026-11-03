module Thing where

import Data.Validity (Validity)
import GHC.Generics (Generic)

data Widget = Widget
  deriving (Generic)

instance Validity Widget
