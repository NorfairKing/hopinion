{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)

data Colour = Red | Green
  deriving (Generic)

instance Validity Colour where
  validate c = genericValidate c

instance GenValid Colour where
  shrinkValid c = shrinkValidStructurally c
