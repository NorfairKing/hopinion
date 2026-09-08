{-# LANGUAGE LambdaCase #-}

module Good where

data Verdict = Good | Bad

-- What the rule asks for.
verdictNumber :: Verdict -> Int
verdictNumber = \case
  Good -> 1
  Bad -> 2

-- The argument is read as well as taken apart, so the name is doing something.
describe :: Verdict -> (Verdict, Int)
describe v = case v of
  Good -> (v, 1)
  Bad -> (v, 2)

-- The equations differ in an argument that is not the last one.
weigh :: Int -> Verdict -> Int
weigh 0 _ = 0
weigh n Good = n
weigh _ Bad = 0

-- One equation, and what it does with the argument is not a case.
negate' :: Verdict -> Verdict
negate' v = other v

other :: Verdict -> Verdict
other = \case
  Good -> Bad
  Bad -> Good

-- An infix definition, which has no \case form to prefer.
(<+>) :: Verdict -> Verdict -> Verdict
Good <+> v = v
Bad <+> _ = Bad
