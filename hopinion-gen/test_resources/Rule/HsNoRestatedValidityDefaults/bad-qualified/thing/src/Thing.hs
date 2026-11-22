{-# LANGUAGE DeriveGeneric #-}

module Thing where

import qualified Data.GenValidity as GenValidity
import qualified Data.Validity as Validity
import GHC.Generics (Generic)

data Colour = Red | Green
  deriving (Generic)

instance Validity.Validity Colour where
  validate = (Validity.genericValidate)

instance GenValidity.GenValid Colour where
  shrinkValid = GenValidity.shrinkValidStructurally
