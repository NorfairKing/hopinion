{-# OPTIONS_GHC -Wno-orphans #-}

module Thing.Gen where

import Data.GenValidity
import Thing

instance GenValid User
