module BadCommented where

import Data.Map (Map)

data Memo = Memo
  { memoKey :: !Int,
    -- | Lazy, so that a memo nobody reads never computes its table.
    memoTable :: Map Int Int
  }

data Stream = Stream !Int Stream -- Lazy, so that a stream can be infinite.

data Tree = Tree
  { treeValue :: !Int,
    treeChildren :: [Tree]
    -- ^ Lazy, so that a tree can be unfolded as it is read.
  }
