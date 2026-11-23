{-# LANGUAGE DeriveGeneric #-}

module B where

import Data.Validity
import GHC.Generics (Generic)

newtype Score = Score Int
  deriving (Generic)

instance Validity Score where
  validate s@(Score i) =
    mconcat
      [ genericValidate s,
        declare "is not negative" (i >= 0)
      ]
