{-# OPTIONS_GHC -Wno-orphans #-}

module Gen where

import qualified A
import qualified B
import Data.GenValidity

instance GenValid A.Score where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid B.Score
