module Bad where

together :: IO Int
together = do
  let x = 5
      y = x + 1
  pure (x + y)

nested :: Maybe Int
nested = do
  n <- Just 1
  let action = do
        let a = n
            b = n
            c = n
        pure (a + b + c)
  action
