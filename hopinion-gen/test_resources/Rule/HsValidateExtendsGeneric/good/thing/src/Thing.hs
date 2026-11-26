{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Thing where

import Data.Validity
import GHC.Generics (Generic)

data User = User
  { userName :: String,
    userAge :: Int
  }
  deriving (Generic, Eq)

instance Validity User

newtype Percentage = Percentage Int
  deriving (Generic, Eq)

instance Validity Percentage where
  validate p@(Percentage i) =
    mconcat
      [ genericValidate p,
        declare "is at most 100" (i <= 100)
      ]

newtype Name = Name String
  deriving (Generic, Eq)

-- Parentheses and type applications do not change which functions these are.
instance Validity Name where
  validate n =
    (mconcat)
      [ (genericValidate @Name) n,
        decorate "the name" (declare "is not empty" (n /= Name ""))
      ]

data Shape = Circle Int | Square Int
  deriving (Generic, Eq)

-- A check that cases on the argument is one more element of the list.
instance Validity Shape where
  validate s =
    mconcat
      [ genericValidate s,
        case s of
          Circle r -> declare "the radius is positive" (r > 0)
          Square w -> declare "the width is positive" (w > 0)
      ]

newtype Length = Length Int
  deriving (Generic, Eq)

-- A lazy pattern still names the whole argument.
instance Validity Length where
  validate ~l@(Length i) =
    mconcat
      [ genericValidate l,
        declare "is not negative" (i >= 0)
      ]
