{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)
import qualified Own

newtype Score = Score Int
  deriving (Generic)

instance Own.Validity Score

instance Validity Score where
  validate s@(Score i) =
    mconcat
      [ genericValidate s,
        declare "is not negative" (i >= 0)
      ]

instance GenValid Score
