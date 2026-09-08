module BadInAWhereClause where

data Verdict = Good | Bad

verdicts :: [Verdict] -> [Int]
verdicts = map number
  where
    number Good = 1
    number Bad = 2
