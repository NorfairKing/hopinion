{-# LANGUAGE DeriveGeneric #-}

module Thing where

import Data.Validity
import GHC.Generics (Generic)

data User = User
  { userName :: String,
    userAge :: Int
  }
  deriving (Generic)

instance Validity User
