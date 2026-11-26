{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Thing where

import qualified Data.Validity as Validity
import GHC.Generics (Generic)

data Colour = Red | Green
  deriving (Generic)

instance Validity.Validity Colour where
  validate = (Validity.genericValidate)

data Size = Small | Large
  deriving (Generic)

instance Validity.Validity Size where
  validate s = Validity.genericValidate s

data Shade = Light | Dark
  deriving (Generic)

instance Validity.Validity Shade where
  validate = Validity.genericValidate @Shade
