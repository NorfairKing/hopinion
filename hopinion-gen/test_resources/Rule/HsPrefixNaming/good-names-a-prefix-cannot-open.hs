{-# LANGUAGE TypeFamilies #-}

module GoodNamesAPrefixCannotOpen where

-- An infix constructor is the operator between its arguments at every use, so
-- there is nowhere in it for a prefix to go.
data Pair a = a :*: a

-- A field opening with an underscore is the one behind a lens, and the lens
-- takes the name that carries the type.
data Point = Point
  { _x :: Int,
    _y :: Int
  }

-- An instance of a data family names what the family declared, not what the
-- head above it is called.
data family Store a

data instance Store Int = IntStore {slot :: Int}
