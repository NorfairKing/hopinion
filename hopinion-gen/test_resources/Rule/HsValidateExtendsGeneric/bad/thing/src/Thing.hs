{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.Validity
import GHC.Generics (Generic)

newtype Percentage = Percentage Int
  deriving (Generic)

-- Leaves the field unchecked.
instance Validity Percentage where
  validate (Percentage i) = declare "is at most 100" (i <= 100)

newtype Score = Score Int
  deriving (Generic)

-- The structural check comes last.
instance Validity Score where
  validate s@(Score i) =
    mconcat
      [ declare "is not negative" (i >= 0),
        genericValidate s
      ]

data Shape = Circle Int | Square Int
  deriving (Generic)

-- One equation per constructor.
instance Validity Shape where
  validate (Circle r) = declare "the radius is positive" (r > 0)
  validate (Square w) = declare "the width is positive" (w > 0)

newtype Weight = Weight Percentage
  deriving (Generic)

-- The structural check of something other than the argument.
instance Validity Weight where
  validate w@(Weight p) =
    mconcat
      [ genericValidate p,
        declare "is not the weight itself" (isValid w)
      ]

newtype Height = Height Int
  deriving (Generic)

-- A local binding, even around the right list.
instance Validity Height where
  validate h@(Height i) =
    mconcat
      [ genericValidate h,
        positive
      ]
    where
      positive = declare "is positive" (i > 0)

newtype Depth = Depth Int
  deriving (Generic)

-- The list behind an operator rather than applied to directly.
instance Validity Depth where
  validate d@(Depth i) =
    mconcat $
      [ genericValidate d,
        declare "is positive" (i > 0)
      ]

newtype Width = Width Int
  deriving (Generic)

-- A guard, even with the right list behind it.
instance Validity Width where
  validate w@(Width i)
    | i > 0 = mconcat [genericValidate w, declare "is small" (i < 100)]
    | otherwise = invalid "is not positive"
