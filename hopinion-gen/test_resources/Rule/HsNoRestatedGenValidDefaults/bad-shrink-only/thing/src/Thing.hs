{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)
import Test.QuickCheck (choose)

newtype Percentage = Percentage Int
  deriving (Generic)

instance Validity Percentage

-- Writing out a generator of its own does not make the shrinker anything but
-- the default.
instance GenValid Percentage where
  genValid = Percentage <$> choose (0, 100)
  shrinkValid = shrinkValidStructurally
