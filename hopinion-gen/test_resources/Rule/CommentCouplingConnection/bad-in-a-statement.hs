module Fixture where

widen :: Int -> Int
widen n =
  let -- This bound and the one in 'Writer' change together.
      bound = 8
   in n + bound
