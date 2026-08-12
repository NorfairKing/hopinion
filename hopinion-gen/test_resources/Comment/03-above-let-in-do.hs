module Fixture where

run :: IO ()
run = do
  -- The let is about to happen.
  let x = 1
  print x
