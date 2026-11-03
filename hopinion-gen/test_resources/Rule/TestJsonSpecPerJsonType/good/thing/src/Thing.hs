{-# LANGUAGE DerivingVia #-}

module Thing where

import Autodocodec (Autodocodec (..), HasCodec (..), dimapCodec, nullCodec)
import Data.Aeson (FromJSON, ToJSON)
import Data.Validity (Validity)
import GHC.Generics (Generic)

-- Both halves through a deriving clause, which is how most of them arrive.
data Both = Both
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Both)

instance Validity Both

instance HasCodec Both where
  codec = dimapCodec (const Both) (const ()) nullCodec

-- Both halves written out.
data Written = Written
  deriving (Show, Eq, Generic)

instance Validity Written

instance ToJSON Written

instance FromJSON Written

-- One half is not the pair, so there is nothing for jsonSpec to assert and no
-- obligation to meet.
data OnlyOut = OnlyOut
  deriving (Generic)

instance ToJSON OnlyOut

data OnlyIn = OnlyIn
  deriving (Generic)

instance FromJSON OnlyIn
