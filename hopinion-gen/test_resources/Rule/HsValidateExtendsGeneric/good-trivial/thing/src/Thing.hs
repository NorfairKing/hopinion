module Thing where

import Data.Validity

-- A type with no Generic instance cannot start from genericValidate, and
-- trivialValidation says in its name that it checks nothing.
newtype Token = Token (Int -> Int)

instance Validity Token where
  validate = trivialValidation

newtype Key = Key (Int -> Int)

instance Validity Key where
  validate k = trivialValidation k
