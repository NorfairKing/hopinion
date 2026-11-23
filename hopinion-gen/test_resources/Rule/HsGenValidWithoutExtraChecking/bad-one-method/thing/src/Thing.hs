{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.GenValidity
import GHC.Generics (Generic)

data User = User
  { userName :: String,
    userAge :: Int
  }
  deriving (Generic)

instance Validity User

instance GenValid User where
  genValid = User <$> genValid <*> genValid
