{-# LANGUAGE DerivingVia #-}

module Thing where

data Both = Both
  deriving (FromJSON, ToJSON) via (Autodocodec Both)
