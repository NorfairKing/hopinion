module Good where

data Point = Point
  { pointX :: Int,
    pointY :: Int
  }

newtype Wrapper = Wrapper {wrapperInner :: Int}

data Shape
  = ShapeCircle Int
  | ShapeSquare Int

data Event
  = EventOpened {eventOpenedAt :: Int}
  | EventClosed {eventClosedAt :: Int}

-- A type whose name opens with an acronym has more than its first letter
-- lowered in a field anyone writes by hand, so the case of the prefix is not
-- what a name is held to.
newtype URL = URL {urlText :: String}
