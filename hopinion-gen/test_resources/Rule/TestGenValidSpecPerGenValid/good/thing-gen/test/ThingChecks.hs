{-# LANGUAGE TypeApplications #-}

module ThingChecks (spec) where

import Test.Syd (Spec)
import Test.Syd.Validity (genValidSpec)
import Thing
import Thing.Spliced

spec :: Spec
spec = do
  genValidSpec @Plain
  genValidSpec @Standalone
  genValidSpec @Unstrategised
  genValidSpec @Wrapped
  genValidSpec @AnyOf
  genValidSpec @ViaInt
  genValidSpec @(Allowed Int)
  genValidSpec @Generated
