module Bad where

data Point = Point
  { pointX :: Int,
    pointY :: Int
  }

origin :: Point
origin = Point {pointX = 0, pointY = 0}

moved :: Point -> Point
moved p = p {pointX = pointX p + 1, pointY = pointY p + 1}

nested :: [Point]
nested =
  [ Point {pointX = 1, pointY = 2}
  ]
