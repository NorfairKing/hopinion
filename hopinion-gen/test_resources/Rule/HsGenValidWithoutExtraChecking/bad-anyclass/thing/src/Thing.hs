{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)

data User = User
  { userName :: String,
    userAge :: Int
  }
  deriving stock (Generic)
  deriving anyclass (Validity, GenValid)
