{-# LANGUAGE TypeApplications #-}

module ThingChecks (spec) where

import Thing
import Thing.Spliced

spec :: IO ()
spec = do
  genValidSpec @Plain
  genValidSpec @Standalone
  genValidSpec @Stocked
  genValidSpec @Wrapped
  genValidSpec @AnyOf
  genValidSpec @ViaCodec
  genValidSpec @(Allowed Int)
  genValidSpec @Generated
