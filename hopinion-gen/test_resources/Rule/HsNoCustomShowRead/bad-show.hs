module WrittenShow where

newtype Token = Token String

instance Show Token where
  show _ = "<token>"
