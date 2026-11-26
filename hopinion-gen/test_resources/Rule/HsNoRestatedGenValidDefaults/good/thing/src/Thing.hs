{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)
import Test.QuickCheck (choose)

data User = User
  { userName :: String,
    userAge :: Int
  }
  deriving (Generic)

instance Validity User

instance GenValid User

newtype Percentage = Percentage Int
  deriving (Generic)

-- Reusing the structural check in a validation of its own is a decision.
instance Validity Percentage where
  validate p@(Percentage i) =
    mconcat
      [ genericValidate p,
        declare "is at most 100" (i <= 100)
      ]

-- Using an argument for something other than handing it to the default is a
-- decision too.
instance GenValid Percentage where
  genValid = Percentage <$> choose (0, 100)
  shrinkValid p = filter isValid (shrinkValidStructurally p)
