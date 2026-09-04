module WrittenShow where

newtype Token = Token String

instance Show Token where
  show (Token s) = concat ["Token ", s]
