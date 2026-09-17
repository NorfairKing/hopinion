module Good where

data Point = Point
  { pointX :: Int,
    pointY :: Int
  }

newtype Wrapped = Wrapped {unWrapped :: Int}

origin :: Point
origin =
  Point
    { pointX = 0,
      pointY = 0
    }

wrapped :: Wrapped
wrapped = Wrapped {unWrapped = 0}

moved :: Point -> Point
moved p =
  p
    { pointX = pointX p + 1,
      pointY = pointY p + 1
    }

nudged :: Point -> Point
nudged p = p {pointX = pointX p + 1}

-- A field whose value runs onto a second line still has a line of its own, so
-- the whole value spanning two lines is not what this rule is about.
stretched :: Point
stretched =
  Point
    { pointX =
        sum
          [1, 2, 3],
      pointY = 0
    }
