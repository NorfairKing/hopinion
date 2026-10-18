module Good where

data Point = Point
  { pointX :: Int,
    pointY :: Int
  }

data Shape
  = ShapeCircle Int
  | ShapeSquare Int

data Event
  = EventOpened {eventOpenedAt :: Int}
  | EventClosed {eventClosedAt :: Int}

-- A type whose name opens with an acronym has more than its first letter
-- lowered in a field anyone writes by hand, so the case of the prefix is not
-- what a name is held to.
data URL = URL
  { urlScheme :: String,
    urlHost :: String
  }

-- The one field of a newtype has no sibling to be told apart from, so it says
-- what applying it undoes rather than which type it belongs to.
newtype Wrapper = Wrapper {unWrapper :: Int}

-- The same spelling whatever the case of the type's name.
newtype URI = URI {unURI :: String}

-- A newtype written without a field introduces no field to name.
newtype Age = Age Int
