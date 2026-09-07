module EmptyOrd where

data Colour = Red | Green
  deriving (Eq)

instance Ord Colour
