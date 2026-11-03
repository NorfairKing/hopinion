{-# LANGUAGE TypeApplications #-}

module ThingChecks (spec) where

import Test.Syd (Spec)
import Test.Syd.Validity (genValidSpec)
import Thing
import Thing.Gen ()

spec :: Spec
spec = genValidSpec @Widget
