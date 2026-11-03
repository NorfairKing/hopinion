{-# LANGUAGE TypeApplications #-}

module ThingChecks (spec) where

import Test.Syd (Spec)
import Test.Syd.Validity.Aeson (jsonSpec)
import Thing
import Thing.Gen ()

spec :: Spec
spec = do
  jsonSpec @Both
  jsonSpec @Written
