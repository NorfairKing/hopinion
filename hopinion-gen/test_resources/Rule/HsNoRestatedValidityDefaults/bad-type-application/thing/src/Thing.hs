{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)

data Colour = Red | Green
  deriving (Generic)

instance Validity Colour where
  validate = genericValidate @Colour

instance GenValid Colour where
  genValid = genValidStructurally @Colour
  shrinkValid = (shrinkValidStructurally @Colour)
