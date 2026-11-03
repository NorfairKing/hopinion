{-# OPTIONS_GHC -Wno-orphans #-}

module Thing.Gen () where

import Data.GenValidity (GenValid)
import Thing

instance GenValid Both

instance GenValid Written
