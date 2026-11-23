{-# LANGUAGE DeriveGeneric #-}

module Other where

import Data.Validity
import GHC.Generics (Generic)

newtype Score = Score Int
  deriving (Generic)

instance Validity Score
