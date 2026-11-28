{-# LANGUAGE DerivingVia #-}

module Alone where

import Autodocodec (Autodocodec (..), HasCodec (..), dimapCodec, nullCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity (Validity)
import GHC.Generics (Generic)

data Alone = Alone
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Alone)

instance Validity Alone

instance HasCodec Alone where
  codec = dimapCodec (const Alone) (const ()) nullCodec
