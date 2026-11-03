{-# LANGUAGE TemplateHaskell #-}

module Thing.Spliced where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)

-- The engine cannot see what the splice generates, so it abstains for this
-- module. The instance written out here is visible all the same, and untested.
data Generated = Generated
  deriving (Show, Generic)

instance Validity Generated

instance GenValid Generated

$(pure [])
