module Tests (tests) where

import Helper (helper)

tests :: IO [Int]
tests = pure [helper]
