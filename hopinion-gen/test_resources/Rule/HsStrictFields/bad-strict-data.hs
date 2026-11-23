{-# LANGUAGE StrictData #-}

module BadStrictData where

data Point = Point
  { pointX :: Int,
    pointY :: ~Int
  }
