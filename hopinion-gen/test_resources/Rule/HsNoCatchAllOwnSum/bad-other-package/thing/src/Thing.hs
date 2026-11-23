module Thing where

import Shapes

isRound :: Shape -> Bool
isRound shape = case shape of
  ShapeCircle _ -> True
  _ -> False
