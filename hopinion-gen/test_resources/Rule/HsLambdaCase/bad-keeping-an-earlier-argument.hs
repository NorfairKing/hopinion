module BadKeepingAnEarlierArgument where

data Verdict = Good | Bad

weigh :: Int -> Verdict -> Int
weigh n Good = n
weigh n Bad = negate n
