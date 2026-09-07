module WrittenEq where

data User = User
  { userName :: String,
    userAge :: Int
  }

instance Eq User where
  a == b = userName a == userName b
