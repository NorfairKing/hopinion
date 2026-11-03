{-# LANGUAGE DeriveGeneric #-}

module Lonely where

import Data.GenValidity (GenValid)
import Data.Validity (Validity)
import GHC.Generics (Generic)

data Orphan = Orphan
  deriving (Generic)

instance Validity Orphan

-- A package with no gen package has nowhere to put the test, so this obligation
-- can never be met and is reported where it is made. The fix is a lonely-gen,
-- or moving the instance to a package that has one.
instance GenValid Orphan
