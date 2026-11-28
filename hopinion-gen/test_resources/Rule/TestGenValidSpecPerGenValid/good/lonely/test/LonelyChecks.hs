{-# LANGUAGE TypeApplications #-}

module LonelyChecks (spec) where

import Lonely
import Test.Syd (Spec)
import Test.Syd.Validity (genValidSpec)

spec :: Spec
spec = genValidSpec @Orphan
