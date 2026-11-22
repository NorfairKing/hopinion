module Thing where

-- A class of the same name, with a method of the same name and a function of
-- the same name, is still not the class whose default that function is.
class Validity a where
  validate :: a -> Bool

genericValidate :: a -> Bool
genericValidate = const True

data Colour = Red | Green

instance Validity Colour where
  validate = genericValidate
