module Good where

separate :: IO Int
separate = do
  let x = 5
  let y = x + 1
  pure (x + y)

withSignature :: IO Int
withSignature = do
  let double :: Int -> Int
      double n = n * 2
  pure (double 3)

overEquations :: IO Int
overEquations = do
  let sign 0 = 0
      sign n = if n < 0 then -1 else 1 :: Int
  pure (sign 3)

pattern :: (Int, Int) -> IO Int
pattern pair = do
  let (a, b) = pair
  pure (a + b)

-- A let expression is not a statement of a do-block, so its bindings are left
-- alone, as are those of a where clause, a guard and a comprehension.
expression :: Int
expression =
  let x = 5
      y = 6
   in x + y

whereClause :: Int
whereClause = x + y
  where
    x = 5
    y = 6

guarded :: Int -> Int
guarded n
  | let x = n + 1
        y = n + 2,
    x < y =
      x
  | otherwise = n

comprehension :: [Int]
comprehension = [x + y | n <- [1, 2], let {x = n; y = n}]
