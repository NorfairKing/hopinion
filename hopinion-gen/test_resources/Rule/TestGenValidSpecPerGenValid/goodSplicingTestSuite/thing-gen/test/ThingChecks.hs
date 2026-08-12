{-# LANGUAGE TemplateHaskell #-}

module ThingChecks (spec) where

import Thing
import Thing.Gen

spec :: IO ()
spec = $(genValidSpecsFor ''Visible)
