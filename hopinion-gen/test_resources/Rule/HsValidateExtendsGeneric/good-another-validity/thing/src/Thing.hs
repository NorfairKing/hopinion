module Thing where

-- A class of the same name, with a method of the same name, is still not the
-- class whose validate has to start from genericValidate.
class Validity a where
  validate :: a -> Bool

data Colour = Red | Green

instance Validity Colour where
  validate Red = True
  validate Green = False
