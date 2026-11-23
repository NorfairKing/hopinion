{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)

data User = User
  { userName :: String,
    userAge :: Int
  }
  deriving (Generic)

instance Validity User

instance GenValid User where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

newtype Percentage = Percentage Int
  deriving (Generic)

-- Checking more than the fields is what the default's extra check is for.
instance Validity Percentage where
  validate p@(Percentage i) =
    mconcat
      [ genericValidate p,
        declare "is at most 100" (i <= 100)
      ]

instance GenValid Percentage

-- A type by the same name as one whose Validity checks only its fields.
data Score = Score Int
  deriving (Generic)

instance Validity Score where
  validate s@(Score i) =
    mconcat
      [ genericValidate s,
        declare "is not negative" (i >= 0)
      ]

instance GenValid Score
