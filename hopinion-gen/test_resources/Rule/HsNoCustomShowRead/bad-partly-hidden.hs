module PartlyHiddenShow where

newtype Token = Token String

-- One method discarding the value does not stop the other from printing it.
instance Show Token where
  show (Token s) = s
  showsPrec _ _ = showString "<token>"
