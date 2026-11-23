module Thing where

data Shape
  = ShapeCircle Double
  | ShapeDot

type Figure = Shape

type Drawing = Figure

isDot :: Figure -> Bool
isDot ShapeDot = True
isDot _ = False

isDrawnDot :: Drawing -> Bool
isDrawnDot ShapeDot = True
isDrawnDot _ = False

size :: Shape -> Double
size ShapeDot = 0
size ~s = seq s 1
