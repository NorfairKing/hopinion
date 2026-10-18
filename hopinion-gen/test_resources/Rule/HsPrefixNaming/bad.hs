module Bad where

data Point = Point
  { x :: Int,
    pointY :: Int
  }

data Configuration = Configuration
  { confPath :: Int,
    configurationDepth :: Int
  }

-- A newtype's field carries the prefix every other field carries, which is
-- the one place that spelling is wrong.
newtype Wrapper = Wrapper {wrapperInner :: Int}

-- And an acronym is no excuse for a second spelling either.
newtype URL = URL {urlText :: String}

data Shape
  = Circle Int
  | ShapeSquare Int
