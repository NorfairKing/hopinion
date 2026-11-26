{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.Validity
import GHC.Generics (Generic)

newtype Percentage = Percentage Int
  deriving (Generic)

-- A top-level binding can carry annotations that an instance method cannot,
-- so the method may name one, and it is that binding which has the shape.
validatePercentage :: Percentage -> Validation
validatePercentage p@(Percentage i) =
  mconcat
    [ genericValidate p,
      declare "is at most 100" (i <= 100)
    ]

instance Validity Percentage where
  validate = validatePercentage

newtype Score = Score Int
  deriving (Generic)

validateScore :: Score -> Validation
validateScore s@(Score i) =
  mconcat
    [ genericValidate s,
      declare "is not negative" (i >= 0)
    ]

-- Applied to the argument rather than eta-reduced.
instance Validity Score where
  validate s = validateScore s

newtype Weight = Weight Int
  deriving (Generic)

-- The binding below the instance, with its annotation after it.
instance Validity Weight where
  validate = validateWeight

validateWeight :: Weight -> Validation
validateWeight w@(Weight i) =
  mconcat
    [ genericValidate w,
      declare "is positive" (i > 0)
    ]
{-# ANN validateWeight "a note for a plugin" #-}
