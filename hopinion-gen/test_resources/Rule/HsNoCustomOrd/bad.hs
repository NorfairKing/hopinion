module WrittenOrd where

data Task = Task
  { taskPriority :: Int,
    taskName :: String
  }
  deriving (Eq)

instance Ord Task where
  compare a b = compare (taskPriority a) (taskPriority b)
