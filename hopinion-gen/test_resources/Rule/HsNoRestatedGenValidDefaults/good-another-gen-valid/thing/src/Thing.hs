module Thing where

-- A class of the same name, with a method of the same name and a function of
-- the same name, is still not the class whose default that function is.
class GenValid a where
  shrinkValid :: a -> [a]

shrinkValidStructurally :: a -> [a]
shrinkValidStructurally = const []

data Colour = Red | Green

instance GenValid Colour where
  shrinkValid = shrinkValidStructurally
