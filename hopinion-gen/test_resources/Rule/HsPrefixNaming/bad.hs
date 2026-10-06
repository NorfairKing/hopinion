module Bad where

data Point = Point
  { x :: Int,
    pointY :: Int
  }

data Configuration = Configuration
  { confPath :: Int,
    configurationDepth :: Int
  }

newtype Wrapper = Wrapper {unWrapper :: Int}

data Shape
  = Circle Int
  | ShapeSquare Int
