{-# LANGUAGE DerivingVia #-}

module Thing where

-- Both halves through a deriving clause, which is how most of them arrive.
data Both = Both
  deriving (FromJSON, ToJSON) via (Autodocodec Both)

-- Both halves written out.
data Written = Written

instance ToJSON Written

instance FromJSON Written

-- One half is not the pair, so there is nothing for jsonSpec to assert and no
-- obligation to meet.
data OnlyOut = OnlyOut

instance ToJSON OnlyOut

data OnlyIn = OnlyIn

instance FromJSON OnlyIn
