module Clean where

newtype Token = Token String
  deriving (Show, Read)

data Colour = Red | Green
  deriving stock (Show)
