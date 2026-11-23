{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Thing where

data Shape
  = ShapeCircle Double
  | ShapeSquare Double
  | ShapeDot

isRound :: Shape -> Bool
isRound shape = case shape of
  ShapeCircle _ -> True
  _ -> False

sides :: Shape -> Int
sides = \case
  ShapeSquare _ -> 4
  !other -> seq other 0

size :: Shape -> Double
size = \case
  ShapeCircle r -> r
  (s :: Shape) -> seq s 1

mentioned :: Shape -> String
mentioned shape =
  case shape of
    ShapeDot -> "dot"
    -- [allow:HsNoCatchAllOwnSum] Spelled out to show that a suppression covers one catch-all.
    _ -> "something else"

-- A guard can fail, and the alternative still takes any constructor added
-- later whenever it holds.
big :: Shape -> Bool
big = \case
  ShapeDot -> False
  s | isRound s -> True
  ShapeCircle _ -> False
  ShapeSquare _ -> False

-- A lazy constructor pattern still takes the value apart by constructor.
lazily :: Shape -> Int
lazily = \case
  ~(ShapeCircle _) -> 1
  _ -> 0
