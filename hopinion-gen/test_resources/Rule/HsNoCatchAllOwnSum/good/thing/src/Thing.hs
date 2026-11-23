{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Thing where

data Shape
  = ShapeCircle Double
  | ShapeSquare Double
  | ShapeDot

data Point = Point
  { pointX :: !Double,
    pointY :: !Double
  }

newtype Name = Name String

-- Every constructor, written out.
area :: Shape -> Double
area = \case
  ShapeCircle r -> 3 * r * r
  ShapeSquare s -> s * s
  ShapeDot -> 0

-- A catch-all that is the only alternative matches on nothing.
describe :: Shape -> String
describe shape = case shape of
  s -> seq s "a shape"

-- A wildcard nested in a constructor pattern is about the field.
radius :: Maybe Shape -> Double
radius = \case
  Just (ShapeCircle r) -> r
  Just (ShapeSquare _) -> 0
  Just ShapeDot -> 0
  Nothing -> 0

-- Catch-alls on types from dependencies.
orZero :: Maybe Double -> Double
orZero = \case
  Just d -> d
  _ -> 0

isPositive :: Int -> Bool
isPositive = \case
  0 -> False
  n -> n > 0

-- A type with one constructor has nothing to enumerate.
onAxis :: Point -> Bool
onAxis = \case
  Point 0 _ -> True
  _ -> False

nameless :: Name -> Bool
nameless = \case
  Name "" -> True
  _ -> False

-- An argument that every equation names is not matched on.
scaled :: Double -> Shape -> Shape
scaled k = \case
  ShapeCircle r -> ShapeCircle (k * r)
  ShapeSquare s -> ShapeSquare (k * s)
  ShapeDot -> ShapeDot

larger :: Shape -> Double -> Double
larger ShapeDot x = x
larger (ShapeCircle r) x = max r x
larger (ShapeSquare s) x = max s x

-- Another instance of a family with an instance of the project's.
data family Key a

data instance Key Int
  = KeyOne
  | KeyTwo

newtype instance Key Bool = KeyBool Bool

isTrue :: Key Bool -> Bool
isTrue = \case
  KeyBool True -> True
  _ -> False

isOne :: Key Int -> Bool
isOne = \case
  KeyOne -> True
  KeyTwo -> False

-- A synonym of a dependency's type.
type Count = Maybe Int

counted :: Count -> Int
counted = \case
  Just n -> n
  _ -> 0
