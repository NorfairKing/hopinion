{-# LANGUAGE StrictData #-}

module GoodStrictData where

data Point = Point
  { pointX :: Int,
    pointY :: Int
  }

-- [allow:HsStrictFields] A stream can be infinite.
data Stream = Stream Int ~Stream
