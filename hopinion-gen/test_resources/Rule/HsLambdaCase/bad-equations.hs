module BadEquations where

data Verdict = Good | Bad

verdictNumber :: Verdict -> Int
verdictNumber Good = 1
verdictNumber Bad = 2
