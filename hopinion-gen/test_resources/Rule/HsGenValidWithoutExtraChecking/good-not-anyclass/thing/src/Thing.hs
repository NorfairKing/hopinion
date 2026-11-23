{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)

-- A newtype deriving takes its methods from the wrapped type's instance.
newtype Score = Score Int
  deriving stock (Generic)
  deriving newtype (Validity, GenValid)

data User = User
  { userName :: String,
    userAge :: Int
  }
  deriving stock (Generic)

-- What a standalone deriving does is not read from the source.
deriving anyclass instance Validity User

instance GenValid User
