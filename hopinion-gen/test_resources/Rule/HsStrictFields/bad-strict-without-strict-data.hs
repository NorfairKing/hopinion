{-# LANGUAGE Strict #-}
{-# LANGUAGE NoStrictData #-}

module BadStrictWithoutStrictData where

data Point = Point
  { pointX :: Int,
    pointY :: !Int
  }
