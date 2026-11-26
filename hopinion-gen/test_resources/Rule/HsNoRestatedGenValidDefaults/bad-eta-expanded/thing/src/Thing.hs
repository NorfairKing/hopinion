{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)

data Colour = Red | Green
  deriving (Generic)

instance Validity Colour

instance GenValid Colour where
  shrinkValid c = shrinkValidStructurally c
