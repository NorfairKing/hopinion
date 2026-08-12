module Fixture where

describeIt :: Int -> String
describeIt n = case n of
  -- Zero is special.
  0 -> "zero"
  _ -> "other"
