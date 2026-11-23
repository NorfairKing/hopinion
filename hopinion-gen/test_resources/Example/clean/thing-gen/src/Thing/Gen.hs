{-# OPTIONS_GHC -Wno-orphans #-}

module Thing.Gen () where

import Data.GenValidity (GenValid (..), genValidStructurallyWithoutExtraChecking, shrinkValidStructurallyWithoutExtraFiltering)
import Thing

instance GenValid Widget where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
