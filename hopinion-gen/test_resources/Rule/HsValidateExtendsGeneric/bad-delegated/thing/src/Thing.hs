{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.Validity
import GHC.Generics (Generic)

newtype Percentage = Percentage Int
  deriving (Generic)

-- Leaves the field unchecked.
validatePercentage :: Percentage -> Validation
validatePercentage (Percentage i) = declare "is at most 100" (i <= 100)

instance Validity Percentage where
  validate = validatePercentage

newtype Score = Score Int
  deriving (Generic)

-- The default, written out.
validateScore :: Score -> Validation
validateScore s = genericValidate s

instance Validity Score where
  validate s = validateScore s

newtype Weight = Weight Int
  deriving (Generic)

-- Not a binding of this module, so the method is what has the shape.
instance Validity Weight where
  validate = trivialValidation

newtype Height = Height Int
  deriving (Generic)

-- The binding is further away than its own signature and pragmas.
instance Validity Height where
  validate = validateHeight

newtype Depth = Depth Int
  deriving (Generic)

validateHeight :: Height -> Validation
validateHeight h@(Height i) =
  mconcat
    [ genericValidate h,
      declare "is positive" (i > 0)
    ]
