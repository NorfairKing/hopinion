{-# LANGUAGE TemplateHaskell #-}

module ThingChecks (spec) where

import Test.Syd (Spec)
import Thing
import Thing.Gen

spec :: Spec
spec = $(genValidSpecsFor ''Visible)
