{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- A package with no gen package keeps its generators beside its tests.
module AloneChecks (spec) where

import Alone
import Data.GenValidity (GenValid)
import Test.Syd (Spec)
import Test.Syd.Validity.Aeson (jsonSpec)

instance GenValid Alone

spec :: Spec
spec = jsonSpec @Alone
