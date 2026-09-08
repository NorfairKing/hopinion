module Bad where

data Verdict = Good | Bad

verdictNumber :: Verdict -> Int
verdictNumber v = case v of
  Good -> 1
  Bad -> 2
