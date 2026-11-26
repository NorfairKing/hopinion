{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)

data Colour = Red | Green
  deriving (Generic)

instance Validity Colour

instance GenValid Colour where
  -- [allow:HsNoRestatedGenValidDefaults] Spelled out to show that the suppression covers this method alone.
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
