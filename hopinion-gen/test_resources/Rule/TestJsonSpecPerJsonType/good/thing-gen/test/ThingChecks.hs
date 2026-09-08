{-# LANGUAGE TypeApplications #-}

module ThingChecks (spec) where

import Thing

spec :: IO ()
spec = do
  jsonSpec @Both
  jsonSpec @Written
