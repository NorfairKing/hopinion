{-# LANGUAGE DefaultSignatures #-}

module Own where

-- A class of the project's own by the name of the one from validity.
class Validity a where
  isFine :: a -> Bool
  default isFine :: a -> Bool
  isFine _ = True
