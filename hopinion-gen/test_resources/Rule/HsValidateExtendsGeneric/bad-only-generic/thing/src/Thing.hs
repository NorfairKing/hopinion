{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.Validity
import GHC.Generics (Generic)

newtype Percentage = Percentage Int
  deriving (Generic)

-- A list of nothing but the structural check is the default, written out.
instance Validity Percentage where
  validate p = mconcat [genericValidate p]
