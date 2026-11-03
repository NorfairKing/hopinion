{-# LANGUAGE DerivingVia #-}

module Thing where

import Autodocodec (Autodocodec (..), HasCodec (..), dimapCodec, nullCodec)
import Data.Aeson (FromJSON, ToJSON)

data Both = Both
  deriving (FromJSON, ToJSON) via (Autodocodec Both)

instance HasCodec Both where
  codec = dimapCodec (const Both) (const ()) nullCodec
