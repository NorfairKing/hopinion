{-# LANGUAGE TypeApplications #-}

module ThingChecks (spec) where

import Thing

spec :: IO ()
spec = genValidSpec @Plain
