{-# LANGUAGE TypeApplications #-}

module ThingChecks (spec) where

import Test.Syd (Spec)
import Test.Syd.Validity (genValidSpec)
import Thing

spec :: Spec
spec = genValidSpec @Plain
