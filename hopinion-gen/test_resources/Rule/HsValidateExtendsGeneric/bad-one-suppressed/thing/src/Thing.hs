{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.Validity
import GHC.Generics (Generic)

newtype Percentage = Percentage Int
  deriving (Generic)

instance Validity Percentage where
  -- [allow:HsValidateExtendsGeneric] Spelled out to show that the suppression covers this instance alone.
  validate (Percentage i) = declare "is at most 100" (i <= 100)

newtype Score = Score Int
  deriving (Generic)

instance Validity Score where
  validate (Score i) = declare "is not negative" (i >= 0)
