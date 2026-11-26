{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.Validity
import GHC.Generics (Generic)

newtype Percentage = Percentage Int
  deriving (Generic)

instance Validity Percentage where
  validate _ = valid

newtype Score = Score Int
  deriving (Generic)

instance Validity Score where
  validate s = valid

newtype Weight = Weight Int
  deriving (Generic)

instance Validity Weight where
  validate = const valid
